(ns info.jab.easyracer.scenarios-test
  "Integration tests for easyracer scenarios.

  Boots ghcr.io/jamesward/easyracer in a Testcontainer and runs every
  scenario against it. Mirrors the parameterised test in the Java + CF
  reference implementation.

  Per-test timeouts work like Surefire's `forkedProcessTimeoutInSeconds`
  / JUnit `@Timeout`: each `deftest` is run inside a future and aborted
  after `:timeout-ms` (defaults to `default-test-timeout-ms`).
  Override per-test via `^{:timeout-ms N}` on the `deftest`."
  (:require
    [clj-test-containers.core :as tc]
    [clojure.test :refer [deftest do-report is testing use-fixtures]]
    [info.jab.easyracer.scenarios :as ez]))

(def ^:private logger
  (org.slf4j.LoggerFactory/getLogger "info.jab.easyracer.scenarios-test"))

(def ^:private easyracer-image "ghcr.io/jamesward/easyracer")

(def ^:private default-test-timeout-ms
  "Per-test cap, analogue of JUnit `@Timeout`. Most scenarios finish
   in seconds; scenario 3 (10k requests) and scenario 7 (3s hedge)
   can take longer, so override with metadata."
  60000)

(def ^:private run-timeout-ms
  "Run-wide cap, analogue of Surefire's `forkedProcessTimeoutInSeconds`.
   If the whole test run exceeds this, the JVM is force-exited with
   code 124 (matching `timeout(1)` semantics)."
  (Long/parseLong (or (System/getProperty "easyracer.run.timeout.ms")
                      "600000")))

(def ^:dynamic *base-url* nil)

(defonce ^:private container (atom nil))

(defn- start! []
  (let [c (-> (tc/create
                {:image-name    easyracer-image
                 :exposed-ports [8080]
                 :wait-for      {:wait-strategy :http
                                 :path          "/"
                                 :port          8080}})
              tc/start!)]
    (reset! container c)
    (str "http://" (:host c) ":" (get (:mapped-ports c) 8080))))

(defn- stop! []
  (when-let [c (first (swap-vals! container (constantly nil)))]
    (tc/stop! c)))

(defonce ^:private shutdown-hook
  (let [hook (doto (Thread.
                    ^Runnable
                    (fn []
                      ;; Ensure container cleanup when the JVM exits unexpectedly
                      ;; (for example, when the run watchdog triggers System/exit).
                      (stop!)))
               (.setName "easyracer-container-shutdown-hook")
               (.setDaemon true))]
    (.addShutdownHook (Runtime/getRuntime) hook)
    hook))

(defn- with-easyracer-server [test-fn]
  (let [url (start!)]
    (try
      (binding [*base-url* url]
        (test-fn))
      (finally (stop!)))))

(defn- with-run-watchdog
  "Mirrors Surefire's `forkedProcessTimeoutInSeconds`: a daemon thread
   that hard-exits the JVM if the whole test run exceeds the cap. The
   fixture interrupts the watchdog when tests finish normally."
  [test-fn]
  (let [done?    (atom false)
        watchdog (doto (Thread.
                         ^Runnable
                         (fn []
                           (try
                             (Thread/sleep run-timeout-ms)
                             (when-not @done?
                               (binding [*out* *err*]
                                 (println
                                   (format "FATAL: test run exceeded %d ms; aborting"
                                           run-timeout-ms)))
                               (System/exit 124))
                             (catch InterruptedException _ ::ok))))
                   (.setDaemon true)
                   (.setName "easyracer-test-watchdog")
                   .start)]
    (try
      (test-fn)
      (finally
        (reset! done? true)
        (.interrupt watchdog)))))

(defn- run-with-timeout
  "Runs one scenario function on a future and fails if it exceeds
   `timeout-ms`."
  [timeout-ms test-fn]
  (let [timeout timeout-ms
        fut     (future (test-fn))
        result  (deref fut timeout ::timeout)]
    (when (= ::timeout result)
      (future-cancel fut)
      (do-report
        {:type     :fail
         :message  (format "Test timed out after %d ms" timeout)
         :expected (format "completion within %d ms" timeout)
         :actual   :timeout}))))

(use-fixtures :once with-run-watchdog with-easyracer-server)

(def ^:private scenarios
  [{:id 1 :name "scenario-1-test" :timeout-ms 30000 :description "Race two concurrent requests" :run ez/scenario-1}
   {:id 2 :name "scenario-2-test" :timeout-ms 30000 :description "Race two requests, one errors" :run ez/scenario-2}
   {:id 3 :name "scenario-3-test" :timeout-ms 180000 :description "Race 10000 concurrent requests" :run ez/scenario-3}
   {:id 4 :name "scenario-4-test" :timeout-ms 30000 :description "Race two requests, one with 1s timeout" :run ez/scenario-4}
   {:id 5 :name "scenario-5-test" :timeout-ms 30000 :description "Race two requests; non-200 is a loser" :run ez/scenario-5}
   {:id 6 :name "scenario-6-test" :timeout-ms 30000 :description "Race three requests; non-200 is a loser" :run ez/scenario-6}
   {:id 7 :name "scenario-7-test" :timeout-ms 30000 :description "Hedging: second request after 3s" :run ez/scenario-7}
   {:id 8 :name "scenario-8-test" :timeout-ms 60000 :description "Resource open/use/close" :run ez/scenario-8}
   {:id 9 :name "scenario-9-test" :timeout-ms 30000 :description "Concatenate body in response order" :run ez/scenario-9}
   {:id 10 :name "scenario-10-test" :timeout-ms 60000 :description "CPU work + load reporting + cancellation" :run ez/scenario-10}
   {:id 11 :name "scenario-11-test" :timeout-ms 30000 :description "All-failures-handled nested race" :run ez/scenario-11}])

(deftest all-scenarios-test
  (doseq [{:keys [id timeout-ms description run]} scenarios]
    (.info logger (format "Scenario %d" id))
    (testing description
      (run-with-timeout timeout-ms
                        #(is (= :right (run *base-url*)))))))
