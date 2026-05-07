(ns info.jab.easyracer.scenarios-test
  "Integration tests for easyracer scenarios.

  Boots ghcr.io/jamesward/easyracer in a Testcontainer and runs every
  scenario against it.

  Per-test timeouts work like Surefire's `forkedProcessTimeoutInSeconds`
  / JUnit `@Timeout`: each scenario is run inside a future and aborted
  after `default-test-timeout-ms`."
  (:require
    [clj-test-containers.core :as tc]
    [clojure.test :refer [deftest do-report is testing use-fixtures]]
    [info.jab.easyracer.scenarios :as ez]))

(def ^:private logger
  (org.slf4j.LoggerFactory/getLogger "info.jab.easyracer.scenarios-test"))

(def ^:private easyracer-image "ghcr.io/jamesward/easyracer")

(def ^:private default-test-timeout-ms
  "Per-scenario cap, analogue of JUnit `@Timeout`."
  60000)

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
                      ;; so Testcontainers resources are released on process exit.
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

(use-fixtures :once with-easyracer-server)

(def ^:private scenarios
  [{:id 1 :name "scenario-1-test" :run ez/scenario-1}
   {:id 2 :name "scenario-2-test" :run ez/scenario-2}
   {:id 3 :name "scenario-3-test" :run ez/scenario-3}
   {:id 4 :name "scenario-4-test" :run ez/scenario-4}
   {:id 5 :name "scenario-5-test" :run ez/scenario-5}
   {:id 6 :name "scenario-6-test" :run ez/scenario-6}
   {:id 7 :name "scenario-7-test" :run ez/scenario-7}
   {:id 8 :name "scenario-8-test" :run ez/scenario-8}
   {:id 9 :name "scenario-9-test" :run ez/scenario-9}
   {:id 10 :name "scenario-10-test" :run ez/scenario-10}
   {:id 11 :name "scenario-11-test" :run ez/scenario-11}])

(deftest all-scenarios-test
  (doseq [{:keys [id name run]} scenarios]
    (.info logger (format "Scenario %d" id))
    (testing name
      (run-with-timeout default-test-timeout-ms
                        #(is (= :right (run *base-url*)))))))
