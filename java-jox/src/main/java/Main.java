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
import java.time.Instant;
import java.util.*;
import java.util.concurrent.*;
import java.util.function.Function;
import java.util.function.Supplier;

import static com.softwaremill.jox.structured.Par.par;
import static com.softwaremill.jox.structured.Race.race;
import static com.softwaremill.jox.structured.Race.timeout;
import static com.softwaremill.jox.structured.Scopes.supervised;

// Note: Intentionally, code is not shared across scenarios
public class Main {

    static class Scenarios {
        private final URI url;
        private final HttpClient client;

        Scenarios(URI url) {
            this.url = url;
            this.client = HttpClient.newHttpClient();
        }

        public String scenario1() throws ExecutionException, InterruptedException {
            var req = HttpRequest.newBuilder(url.resolve("/1")).build();
            return race(
                    () -> client.send(req, HttpResponse.BodyHandlers.ofString()),
                    () -> client.send(req, HttpResponse.BodyHandlers.ofString())
            ).body();
        }


        public String scenario2() throws ExecutionException, InterruptedException {
            var req = HttpRequest.newBuilder(url.resolve("/1")).build();
            return race(
                    () -> client.send(req, HttpResponse.BodyHandlers.ofString()),
                    () -> client.send(req, HttpResponse.BodyHandlers.ofString())
            ).body();
        }


        public String scenario3() throws ExecutionException, InterruptedException {
            var req = HttpRequest.newBuilder(url.resolve("/3")).build();
            List<Callable<HttpResponse<String>>> reqs =
                    Collections.nCopies(10_000, () ->
                        client.send(req, HttpResponse.BodyHandlers.ofString())
                    );
            return race(reqs).body();
        }


        public String scenario4() throws ExecutionException, InterruptedException {
            var req = HttpRequest.newBuilder(url.resolve("/4")).build();
            return race(
                    () -> client.send(req, HttpResponse.BodyHandlers.ofString()),
                    () -> timeout(1000, () -> client.send(req, HttpResponse.BodyHandlers.ofString()))
            ).body();
        }


        public String scenario5() throws ExecutionException, InterruptedException {
            final HttpRequest req = HttpRequest.newBuilder(url.resolve("/5")).build();
            Callable<HttpResponse<String>> makeReq = () -> {
                var resp = client.send(req, HttpResponse.BodyHandlers.ofString());
                if (resp.statusCode() == 200) {
                    return resp;
                } else {
                    throw new Exception("invalid response");
                }
            };

            return race(makeReq, makeReq).body();
        }


        public String scenario6() throws ExecutionException, InterruptedException {
            final HttpRequest req = HttpRequest.newBuilder(url.resolve("/5")).build();
            Callable<HttpResponse<String>> makeReq = () -> {
                var resp = client.send(req, HttpResponse.BodyHandlers.ofString());
                if (resp.statusCode() == 200) {
                    return resp;
                } else {
                    throw new Exception("invalid response");
                }
            };

            return race(makeReq, makeReq, makeReq).body();
        }


        public String scenario7() throws ExecutionException, InterruptedException {
            var req = HttpRequest.newBuilder(url.resolve("/7")).build();
            return race(
                    () -> client.send(req, HttpResponse.BodyHandlers.ofString()),
                    () -> {
                        Thread.sleep(3000);
                        return client.send(req, HttpResponse.BodyHandlers.ofString());
                    }
            ).body();
        }


        public String scenario8() throws InterruptedException, ExecutionException {
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

            return race(
                    () -> {
                        try (var req = new Req()) {
                            return req.make();
                        }
                    },
                    () -> {
                        try (var req = new Req()) {
                            return req.make();
                        }
                    }
            ).body();
        }


        public String scenario9() throws InterruptedException, ExecutionException {
            record TimedResponse(Instant instant, HttpResponse<String> response) { }

            final HttpRequest req = HttpRequest.newBuilder(url.resolve("/9")).build();

            List<Callable<TimedResponse>> reqs =
                    Collections.nCopies(10, () -> {
                                var resp = client.send(req, HttpResponse.BodyHandlers.ofString());
                                return new TimedResponse(Instant.now(), resp);
                            }
                    );

            var all = par(reqs);

            return all.stream()
                    .filter(r -> r.response.statusCode() == 200)
                    .sorted(Comparator.comparing(TimedResponse::instant)).collect(
                            StringBuilder::new,
                            (acc, timedResponse) -> acc.append(timedResponse.response.body()),
                            StringBuilder::append
                    ).toString();
        }


        public String scenario10() throws InterruptedException, ExecutionException {
            var id = UUID.randomUUID().toString();

            Supplier<String> blocker = () -> {
                try {
                    var req = HttpRequest.newBuilder(url.resolve(STR."/10?\{id}")).build();
                    var messageDigest = MessageDigest.getInstance("SHA-512");

                    return race(
                            () -> client.send(req, HttpResponse.BodyHandlers.ofString()),
                            () -> {
                                var result = new byte[512];
                                new Random().nextBytes(result);
                                while (!Thread.interrupted())
                                    result = messageDigest.digest(result);
                                return null;
                            }
                    ).body();
                }
                catch (ExecutionException | InterruptedException | NoSuchAlgorithmException e) {
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
                var req = HttpRequest.newBuilder(url.resolve(STR."/10?\{id}=\{load}")).build();
                try {
                    var resp = client.send(req, HttpResponse.BodyHandlers.ofString());
                    if ((resp.statusCode() >= 200) && (resp.statusCode() < 300)) {
                        return resp.body();
                    }
                    else if ((resp.statusCode() >= 300) && (resp.statusCode() < 400)) {
                        Thread.sleep(1000);
                        return recursive.func.get();
                    }
                    else {
                        throw new RuntimeException(resp.body());
                    }
                } catch (IOException | InterruptedException e) {
                    throw new RuntimeException(e);
                }
            };

            return supervised(scope -> {
                scope.fork(blocker::get);
                return scope.fork(recursive.func::get).join();
            });
        }


        public String scenario11() throws ExecutionException, InterruptedException {
            var req = HttpRequest.newBuilder(url.resolve("/11")).build();
            return race(
                    () -> client.send(req, HttpResponse.BodyHandlers.ofString()),
                    () -> race(
                            () -> client.send(req, HttpResponse.BodyHandlers.ofString()),
                            () -> client.send(req, HttpResponse.BodyHandlers.ofString())
                    )
            ).body();
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
