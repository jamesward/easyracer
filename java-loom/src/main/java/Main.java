import com.sun.management.OperatingSystemMXBean;

import java.io.IOException;
import java.lang.management.ManagementFactory;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Duration;
import java.time.Instant;
import java.util.Comparator;
import java.util.List;
import java.util.Random;
import java.util.UUID;
import java.util.concurrent.*;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.IntStream;

// Note: Intentionally, code is not shared across scenarios
public class Main {

    @SuppressWarnings("preview")
    static class Scenarios {
        private final URI url;
        private final HttpClient client;

        Scenarios(URI url) {
            this.url = url;
            this.client = HttpClient.newHttpClient();
        }

        public String scenario1() throws InterruptedException {
            var req = HttpRequest.newBuilder(url.resolve("/1")).build();
            try (var scope = StructuredTaskScope.open(StructuredTaskScope.Joiner.<HttpResponse<String>>anySuccessfulResultOrThrow())) {
                scope.fork(() -> client.send(req, HttpResponse.BodyHandlers.ofString()));
                scope.fork(() -> client.send(req, HttpResponse.BodyHandlers.ofString()));
                return scope.join().body();
            }
        }


        public String scenario2() throws InterruptedException {
            var req = HttpRequest.newBuilder(url.resolve("/2")).build();
            try (var scope = StructuredTaskScope.open(StructuredTaskScope.Joiner.<HttpResponse<String>>anySuccessfulResultOrThrow())) {
                scope.fork(() -> client.send(req, HttpResponse.BodyHandlers.ofString()));
                scope.fork(() -> client.send(req, HttpResponse.BodyHandlers.ofString()));
                return scope.join().body();
            }
        }


        public String scenario3() throws ExecutionException, InterruptedException {
            var req = HttpRequest.newBuilder(url.resolve("/3")).build();
            try (var scope = StructuredTaskScope.open(StructuredTaskScope.Joiner.<HttpResponse<String>>anySuccessfulResultOrThrow())) {
                IntStream.rangeClosed(1, 10_000)
                        .forEach(_ ->
                                scope.fork(() ->
                                        client.send(req, HttpResponse.BodyHandlers.ofString())
                                )
                        );

                return scope.join().body();
            }
        }


        public String scenario4() throws InterruptedException {
            var req = HttpRequest.newBuilder(url.resolve("/4")).build();
            try (var outer = StructuredTaskScope.open(StructuredTaskScope.Joiner.<String>anySuccessfulResultOrThrow())) {
                outer.fork(() -> {
                    try (var inner = StructuredTaskScope.open(StructuredTaskScope.Joiner.<String>anySuccessfulResultOrThrow(), cf -> cf.withTimeout(Duration.ofSeconds(1)))) {
                        inner.fork(() -> client.send(req, HttpResponse.BodyHandlers.ofString()).body());
                        return inner.join();
                    }
                });

                outer.fork(() -> client.send(req, HttpResponse.BodyHandlers.ofString()).body());

                return outer.join();
            }
        }


        public String scenario5() throws InterruptedException {
            class Req {
                final HttpRequest req = HttpRequest.newBuilder(url.resolve("/5")).build();

                HttpResponse<String> make() throws Exception {
                    var resp = client.send(req, HttpResponse.BodyHandlers.ofString());
                    if (resp.statusCode() == 200) {
                        return resp;
                    } else {
                        throw new Exception("invalid response");
                    }
                }
            }

            try (var scope = StructuredTaskScope.open(StructuredTaskScope.Joiner.<HttpResponse<String>>anySuccessfulResultOrThrow())) {
                scope.fork(() -> new Req().make());
                scope.fork(() -> new Req().make());
                return scope.join().body();
            }
        }


        public String scenario6() throws InterruptedException {
            class Req {
                final HttpRequest req = HttpRequest.newBuilder(url.resolve("/6")).build();

                HttpResponse<String> make() throws Exception {
                    var resp = client.send(req, HttpResponse.BodyHandlers.ofString());
                    if (resp.statusCode() == 200) {
                        return resp;
                    } else {
                        throw new Exception("invalid response");
                    }
                }
            }

            try (var scope = StructuredTaskScope.open(StructuredTaskScope.Joiner.<HttpResponse<String>>anySuccessfulResultOrThrow())) {
                scope.fork(() -> new Req().make());
                scope.fork(() -> new Req().make());
                scope.fork(() -> new Req().make());
                return scope.join().body();
            }
        }


        public String scenario7() throws InterruptedException {
            var req = HttpRequest.newBuilder(url.resolve("/7")).build();
            try (var scope = StructuredTaskScope.open(StructuredTaskScope.Joiner.<HttpResponse<String>>anySuccessfulResultOrThrow())) {
                scope.fork(() -> client.send(req, HttpResponse.BodyHandlers.ofString()));
                scope.fork(() -> {
                    Thread.sleep(3000);
                    return client.send(req, HttpResponse.BodyHandlers.ofString());
                });
                return scope.join().body();
            }
        }


        public String scenario8() throws InterruptedException {
            class Req implements AutoCloseable {
                final HttpRequest openReq =
                        HttpRequest.newBuilder(url.resolve("/8?open")).build();
                final Function<String, HttpRequest> useReq = (id) ->
                        HttpRequest.newBuilder(url.resolve("/8?use=" + id)).build();
                final Function<String, HttpRequest> closeReq = (id) ->
                        HttpRequest.newBuilder(url.resolve("/8?close=" + id)).build();

                final String id;

                public Req() throws IOException, InterruptedException {
                    id = client.send(openReq, HttpResponse.BodyHandlers.ofString()).body();
                }

                HttpResponse<String> make() throws Exception {
                    var resp = client.send(useReq.apply(id), HttpResponse.BodyHandlers.ofString());
                    if (resp.statusCode() == 200) {
                        return resp;
                    } else {
                        throw new Exception("invalid response");
                    }
                }

                @Override
                public void close() throws IOException, InterruptedException {
                    client.send(closeReq.apply(id), HttpResponse.BodyHandlers.ofString()).body();
                }
            }

            try (var scope = StructuredTaskScope.open(StructuredTaskScope.Joiner.<HttpResponse<String>>anySuccessfulResultOrThrow())) {
                scope.fork(() -> {
                    try (var req = new Req()) {
                        return req.make();
                    }
                });
                scope.fork(() -> {
                    try (var req = new Req()) {
                        return req.make();
                    }
                });

                return scope.join().body();
            }
        }

        public String scenario9() throws InterruptedException {
            record TimedResponse(Instant instant, HttpResponse<String> response) {
            }

            final HttpRequest req = HttpRequest.newBuilder(url.resolve("/9")).build();
            try (var scope = StructuredTaskScope.open()) {
                var futures = IntStream.rangeClosed(1, 10)
                        .mapToObj(_ ->
                                scope.fork(() -> {
                                    var resp = client.send(req, HttpResponse.BodyHandlers.ofString());
                                    return new TimedResponse(Instant.now(), resp);
                                })
                        ).toList();

                scope.join();

                return futures.stream()
                        .map(StructuredTaskScope.Subtask::get)
                        .filter(r -> r.response.statusCode() == 200)
                        .sorted(Comparator.comparing(TimedResponse::instant)).collect(
                                StringBuilder::new,
                                (acc, timedResponse) -> acc.append(timedResponse.response.body()),
                                StringBuilder::append
                        ).toString();
            }
        }

        public String scenario10() throws InterruptedException {
            var id = UUID.randomUUID().toString();

            Supplier<String> blocker = () -> {
                try (var scope = StructuredTaskScope.open(StructuredTaskScope.Joiner.<HttpResponse<String>>anySuccessfulResultOrThrow())) {
                    var req = HttpRequest.newBuilder(url.resolve("/10?" + id)).build();
                    var messageDigest = MessageDigest.getInstance("SHA-512");

                    scope.fork(() -> client.send(req, HttpResponse.BodyHandlers.ofString()));
                    scope.fork(() -> {
                        var result = new byte[512];
                        new Random().nextBytes(result);
                        while (!Thread.interrupted())
                            result = messageDigest.digest(result);
                        return null;
                    });
                    return scope.join().body();
                } catch (InterruptedException | NoSuchAlgorithmException e) {
                    throw new RuntimeException(e);
                }
            };

            class Recursive<I> {
                public I func;
            }

            Recursive<Supplier<String>> recursive = new Recursive<>();
            recursive.func = () -> {
                var osBean = ManagementFactory.getPlatformMXBean(OperatingSystemMXBean.class);
                var load = osBean.getProcessCpuLoad() * osBean.getAvailableProcessors();
                var req = HttpRequest.newBuilder(url.resolve("/10?" + id + "=" + load)).build();
                try {
                    var resp = client.send(req, HttpResponse.BodyHandlers.ofString());
                    if ((resp.statusCode() >= 200) && (resp.statusCode() < 300)) {
                        return resp.body();
                    } else if ((resp.statusCode() >= 300) && (resp.statusCode() < 400)) {
                        Thread.sleep(1000);
                        return recursive.func.get();
                    } else {
                        throw new RuntimeException(resp.body());
                    }
                } catch (IOException | InterruptedException e) {
                    throw new RuntimeException(e);
                }
            };

            try (var scope = StructuredTaskScope.open()) {
                scope.fork(blocker::get);
                var task = scope.fork(recursive.func::get);
                scope.join();
                return task.get();
            }
        }

        public String scenario11() throws InterruptedException {
            var req = HttpRequest.newBuilder(url.resolve("/11")).build();

            try (var outerScope = StructuredTaskScope.open(StructuredTaskScope.Joiner.<HttpResponse<String>>anySuccessfulResultOrThrow())) {
                outerScope.fork(() -> client.send(req, HttpResponse.BodyHandlers.ofString()));
                outerScope.fork(() -> {
                    try (var innerScope = StructuredTaskScope.open(StructuredTaskScope.Joiner.<HttpResponse<String>>anySuccessfulResultOrThrow())) {
                        innerScope.fork(() -> client.send(req, HttpResponse.BodyHandlers.ofString()));
                        innerScope.fork(() -> client.send(req, HttpResponse.BodyHandlers.ofString()));
                        return innerScope.join();
                    }
                });
                return outerScope.join().body();
            }
        }

        List<String> results() throws ExecutionException, InterruptedException {
            return List.of(scenario1(), scenario2(), scenario3(), scenario4(), scenario5(), scenario6(), scenario7(), scenario8(), scenario9(), scenario10(), scenario11());
//            return List.of(scenario11());
        }
    }

    void main() throws URISyntaxException, ExecutionException, InterruptedException {
        var scenarios = new Scenarios(new URI("http://localhost:8080"));
        scenarios.results().forEach(System.out::println);
    }

}
