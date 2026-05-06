(ns info.jab.easyracer.scenarios
  "EasyRacer client implementation using babashka.http-client + core.async.

  See https://github.com/jamesward/easyracer for the scenario specs.

  Each scenario function takes a base URL string and returns either
  :right (success) or :left (failure)."
  (:require
    [info.jab.easyracer.http :as http])
  (:import
    (java.security MessageDigest)
    (java.time Instant)
    (java.util Random)
    (java.util.concurrent CompletableFuture TimeUnit)
    (java.util.function BiConsumer))
  (:gen-class))

;; ---------------------------------------------------------------------------
;; Scenarios
;; ---------------------------------------------------------------------------

(defn scenario-1
  "Race two concurrent requests; the winner returns 'right'."
  [base-url]
  (http/race-futures
    [(http/get-async (str base-url "/1"))
     (http/get-async (str base-url "/1"))]))

(defn scenario-2
  "Race two requests; one of them errors out."
  [base-url]
  (http/race-futures
    [(http/get-async (str base-url "/2"))
     (http/get-async (str base-url "/2"))]))

(defn scenario-3
  "Race 10 000 concurrent requests."
  [base-url]
  (let [url (str base-url "/3")
        futures (mapv (fn [_] (http/get-async url {:timeout 120000}))
                      (range 10000))]
    (http/race-futures futures)))

(defn scenario-4
  "Race two requests; one of them must time out after 1s. The short
   request is given a 1s :timeout so the underlying connection is
   actually closed when it expires."
  [base-url]
  (http/race-futures
    [(http/get-async (str base-url "/4") {:timeout 1000})
     (http/get-async (str base-url "/4"))]))

(defn scenario-5
  "Race two requests; non-200 is a loser."
  [base-url]
  (http/race-futures
    [(http/get-async (str base-url "/5"))
     (http/get-async (str base-url "/5"))]))

(defn scenario-6
  "Race three requests; non-200 is a loser."
  [base-url]
  (http/race-futures
    [(http/get-async (str base-url "/6"))
     (http/get-async (str base-url "/6"))
     (http/get-async (str base-url "/6"))]))

(defn scenario-7
  "Hedging: start a request, wait at least 3 s, then start a second one."
  [base-url]
  (let [url (str base-url "/7")
        first-cf  (http/get-async url)
        ;; Build a CompletableFuture that fires the second call
        ;; after a 3-second delay using the JDK delayedExecutor.
        delayed-exec (CompletableFuture/delayedExecutor
                       3 TimeUnit/SECONDS (http/executor))
        second-cf (-> (CompletableFuture/supplyAsync
                        ^java.util.function.Supplier
                        (reify java.util.function.Supplier
                          (get [_] (http/get-async url)))
                        delayed-exec)
                      (.thenCompose
                        (reify java.util.function.Function
                          (apply [_ cf] cf))))]
    (http/race-futures [first-cf second-cf])))

(defn scenario-8
  "Resource management: open -> use -> close. Race two such flows."
  [base-url]
  (letfn [(resource-flow []
            (let [open-cf (http/get-async (str base-url "/8?open"))]
              (-> open-cf
                  (.thenCompose
                    (reify java.util.function.Function
                      (apply [_ open-resp]
                        (let [resource-id (:body open-resp)
                              use-cf  (http/get-async (str base-url "/8?use=" resource-id))
                              ;; Always close, regardless of use outcome.
                              close!  (fn [_]
                                        (http/get-async (str base-url "/8?close=" resource-id)))]
                          (-> use-cf
                              (.whenComplete
                                (reify BiConsumer
                                  (accept [_ _r _ex]
                                    (close! resource-id)))))))))))) ]
    (http/race-futures [(resource-flow) (resource-flow)])))

(defn scenario-9
  "Make 10 concurrent requests; 5 return 200 with a single letter.
   Concatenated in response-arrival order they spell 'right'."
  [base-url]
  (let [url (str base-url "/9")
        cfs (mapv (fn [_]
                    (-> (http/get-async url)
                        (.thenApply
                          (reify java.util.function.Function
                            (apply [_ resp]
                              {:at   (Instant/now)
                               :resp resp})))))
                  (range 10))
        ;; Wait for all to complete.
        results (->> cfs
                     (mapv #(.join ^CompletableFuture %))
                     (filter #(= 200 (:status (:resp %))))
                     (sort-by :at)
                     (map #(:body (:resp %)))
                     (apply str))]
    (if (= "right" results) :right :left)))

(defn scenario-10
  "Run a CPU-heavy task in parallel with a load-reporting loop, then
   cancel when the blocker connection closes.

   Part 1: open /10?<id>; while it is open, hash SHA-512 in a tight loop.
   Part 2: every second, POST current process load to /10?<id>=<load>.
           2xx => done, 3xx => keep polling, 4xx => failure."
  [base-url]
  (let [id        (str (random-uuid))
        cancelled (volatile! false)
        ;; OperatingSystemMXBean is the Sun-specific extended bean.
        os-bean   (java.lang.management.ManagementFactory/getPlatformMXBean
                    com.sun.management.OperatingSystemMXBean)
        cpus      (.availableProcessors (Runtime/getRuntime))]
    ;; Part 1a: SHA loop on a virtual thread.
    (http/run-on-vt
      (fn []
        (let [md (MessageDigest/getInstance "SHA-512")
              buf (byte-array 512)]
          (.nextBytes (Random.) buf)
          (loop [b buf]
            (if @cancelled
              :done
              (recur (.digest md b)))))))
    ;; Part 1b: blocker request - keep connection open then signal cancel.
    (let [blocker-cf (http/get-async (str base-url "/10?" id))
          _ (.whenComplete blocker-cf
              (reify BiConsumer
                (accept [_ _r _ex]
                  (vreset! cancelled true))))
          ;; Part 2: load-reporting loop until 2xx/4xx.
          poll (fn []
                 (loop []
                   (let [load (* (.getProcessCpuLoad os-bean) cpus)
                         {:keys [status body]}
                         (try
                           (http/get-sync (str base-url "/10?" id "=" load))
                           (catch Exception _ {:status 500 :body ""}))]
                     (cond
                       (and (>= status 200) (< status 300))
                       (do (vreset! cancelled true)
                           (if (= "right" body) :right :left))

                       (and (>= status 300) (< status 400))
                       (do (Thread/sleep 1000) (recur))

                       :else
                       (do (vreset! cancelled true) :left)))))
          result (poll)]
      ;; Make sure the blocker is fully done so the server knows the loser left.
      (try (.join blocker-cf) (catch Exception _))
      result)))

(defn scenario-11
  "All-failures-handled race: race a single request against a nested
   race of two requests."
  [base-url]
  (let [url (str base-url "/11")
        inner-future (CompletableFuture/supplyAsync
                       ^java.util.function.Supplier
                       (reify java.util.function.Supplier
                         (get [_]
                          (let [v (http/race-futures
                                     [(http/get-async url) (http/get-async url)])]
                            ;; race-futures returns :right/:left; we need
                            ;; to feed the outer race a "response-shaped"
                            ;; map so cf->chan can interpret it.
                            (if (= :right v)
                               {:status 200 :body "right"}
                               {:status 500 :body ""}))))
                       ^java.util.concurrent.Executor (http/executor))]
    (http/race-futures
      [inner-future
       (http/get-async url)])))

