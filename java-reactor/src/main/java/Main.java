import com.sun.management.OperatingSystemMXBean;

import java.lang.management.ManagementFactory;
import java.net.URI;
import java.net.URISyntaxException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Duration;
import java.util.List;
import java.util.Random;
import java.util.UUID;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;
import reactor.netty.http.client.HttpClient;
import reactor.netty.resources.ConnectionProvider;
import reactor.util.function.Tuple2;

// Note: Intentionally, code is not shared across scenarios
public class Main {

    static class Scenarios {
        private final URI url;
        private final HttpClient client;

        Scenarios(URI url) {
            this.url = url;
            this.client = HttpClient.create();
        }

        public String scenario1() {
            Supplier<Mono<String>> request = () -> client.get()
                    .uri(url + "/1")
                    .responseContent()
                    .aggregate()
                    .asString();

            return Mono.firstWithValue(request.get(), request.get()).block();
        }

        public String scenario2() {
            Supplier<Mono<String>> request = () -> client.get()
                    .uri(url + "/2")
                    .responseContent()
                    .aggregate()
                    .asString();

            return Mono.firstWithValue(request.get(), request.get()).block();
        }

        public String scenario3() {
            var provider = ConnectionProvider.builder("scenario3")
                    .maxConnections(10_000)
                    .pendingAcquireMaxCount(10_000)
                    .build();
            var scenario3Client = HttpClient.create(provider);

            Supplier<Mono<String>> request = () -> scenario3Client.get()
                    .uri(url + "/3")
                    .responseContent()
                    .aggregate()
                    .asString();

            var requests = IntStream.rangeClosed(1, 10_000)
                    .mapToObj(_ -> request.get())
                    .toList();

            return Mono.firstWithValue(requests).block();
        }

        public String scenario4() {
            Supplier<Mono<String>> request = () -> client.get()
                    .uri(url + "/4")
                    .responseContent()
                    .aggregate()
                    .asString();

            var timedRequest = request.get().timeout(Duration.ofSeconds(1));
            var normalRequest = request.get();

            return Mono.firstWithValue(timedRequest, normalRequest).block();
        }

        public String scenario5() {
            Supplier<Mono<String>> request = () -> client.get()
                    .uri(url + "/5")
                    .responseSingle((resp, bytes) -> {
                        if (resp.status().code() == 200) {
                            return bytes.asString();
                        } else {
                            return Mono.error(new RuntimeException("invalid response"));
                        }
                    });

            return Mono.firstWithValue(request.get(), request.get()).block();
        }

        public String scenario6() {
            Supplier<Mono<String>> request = () -> client.get()
                    .uri(url + "/6")
                    .responseSingle((resp, bytes) -> {
                        if (resp.status().code() == 200) {
                            return bytes.asString();
                        } else {
                            return Mono.error(new RuntimeException("invalid response"));
                        }
                    });

            return Mono.firstWithValue(request.get(), request.get(), request.get()).block();
        }

        public String scenario7() {
            Supplier<Mono<String>> request = () -> client.get()
                    .uri(url + "/7")
                    .responseContent()
                    .aggregate()
                    .asString();

            var delayedRequest = Mono.delay(Duration.ofSeconds(3)).then(request.get());

            return Mono.firstWithValue(request.get(), delayedRequest).block();
        }

        public String scenario8() {
            Supplier<Mono<String>> openReq = () -> client.get()
                    .uri(url + "/8?open")
                    .responseContent()
                    .aggregate()
                    .asString();

            Supplier<Mono<String>> requestWithResource = () -> Mono.usingWhen(
                    openReq.get(),
                    id -> client.get()
                            .uri(url + "/8?use=" + id)
                            .responseSingle((resp, bytes) -> {
                                if (resp.status().code() == 200) {
                                    return bytes.asString();
                                } else {
                                    return Mono.error(new RuntimeException("invalid response"));
                                }
                            }),
                    id -> client.get()
                            .uri(url + "/8?close=" + id)
                            .responseContent()
                            .aggregate()
                            .asString()
            );

            return Mono.firstWithValue(requestWithResource.get(), requestWithResource.get()).block();
        }

        public String scenario9() {
            Supplier<Mono<String>> request = () -> client.get()
                    .uri(url + "/9")
                    .responseSingle((resp, bytes) -> {
                        if (resp.status().code() == 200) {
                            return bytes.asString();
                        } else {
                            return Mono.empty();
                        }
                    });

            var requests = IntStream.rangeClosed(1, 10)
                    .mapToObj(_ -> request.get())
                    .toList();

            return Flux.merge(requests)
                    .collect(Collectors.joining())
                    .block();
        }

        public String scenario10() {
            var id = UUID.randomUUID().toString();

            Mono<Void> cpuIntensiveTask = Mono.<Void>fromCallable(() -> {
                try {
                    var messageDigest = MessageDigest.getInstance("SHA-512");
                    var result = new byte[512];
                    new Random().nextBytes(result);
                    while (!Thread.interrupted()) {
                        result = messageDigest.digest(result);
                    }
                } catch (NoSuchAlgorithmException e) {
                    throw new RuntimeException(e);
                }
                return null;
            }).subscribeOn(Schedulers.boundedElastic());

            Mono<String> blockerRequest = client.get()
                    .uri(url + "/10?" + id)
                    .responseContent()
                    .aggregate()
                    .asString();

            // blocker: HTTP request races with CPU task - when HTTP completes, CPU is cancelled
            Mono<Void> blocker = Mono.firstWithSignal(blockerRequest.then(), cpuIntensiveTask);

            var osBean = ManagementFactory.getPlatformMXBean(OperatingSystemMXBean.class);

            Mono<String> loadPoller = Mono.defer(() -> {
                var load = osBean.getProcessCpuLoad() * osBean.getAvailableProcessors();
                return client.get()
                        .uri(url + "/10?" + id + "=" + load)
                        .responseSingle((resp, bytes) -> {
                            if (resp.status().code() >= 200 && resp.status().code() < 300) {
                                return bytes.asString();
                            } else if (resp.status().code() >= 300 && resp.status().code() < 400) {
                                return Mono.error(new RuntimeException("continue"));
                            } else {
                                return bytes.asString().flatMap(body -> Mono.error(new RuntimeException(body)));
                            }
                        });
            }).retryWhen(reactor.util.retry.Retry.fixedDelay(Long.MAX_VALUE, Duration.ofSeconds(1))
                    .filter(e -> e.getMessage().equals("continue")));

            // Run blocker and loadPoller in parallel, return loadPoller's result
            return Mono.zip(blocker.then(Mono.just("")), loadPoller)
                    .map(Tuple2::getT2)
                    .block();
        }

        public String scenario11() {
            Supplier<Mono<String>> request = () -> client.get()
                    .uri(url + "/11")
                    .responseContent()
                    .aggregate()
                    .asString();

            var innerRace = Mono.firstWithValue(request.get(), request.get());
            return Mono.firstWithValue(request.get(), innerRace).block();
        }


        List<String> results() {
            return List.of(scenario1(), scenario2(), scenario3(), scenario4(), scenario5(), scenario6(), scenario7(), scenario8(), scenario9(), scenario10(), scenario11());
        }
    }

    void main() throws URISyntaxException {
        var scenarios = new Scenarios(new URI("http://localhost:8080"));
        scenarios.results().forEach(System.out::println);
    }

}
