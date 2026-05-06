(ns info.jab.easyracer.http
  "HTTP client utilities using babashka.http-client + core.async."
  (:require
    [babashka.http-client :as client]
    [clojure.core.async :as async])
  (:import
    (java.util.concurrent CompletableFuture Executor Executors)
    (java.util.function BiConsumer)))

;; ---------------------------------------------------------------------------
;; Configuration
;; ---------------------------------------------------------------------------

(def ^:private virtual-executor
  "Virtual-thread executor shared by HTTP async calls and CPU-heavy tasks."
  (delay
    (Executors/newThreadPerTaskExecutor
      (-> (Thread/ofVirtual)
          (.name "easyracer-vt-" 0)
          .factory))))

(def ^:private http-client
  "Single shared HTTP client (JDK HttpClient under the hood)."
  (delay
    (client/client
      {:version :http2
       :executor @virtual-executor
       :connect-timeout 10000})))

(defn executor
  "Expose the shared executor for delayed/nested CompletableFuture work."
  []
  @virtual-executor)

;; ---------------------------------------------------------------------------
;; Response helpers
;; ---------------------------------------------------------------------------

(defn http-ok?
  "Check if a response indicates success (200 + \"right\" body)."
  [{:keys [status body]}]
  (and (= 200 status) (= "right" body)))

(defn response->verdict
  "Convert an HTTP response map to :right or :left."
  [resp]
  (if (http-ok? resp) :right :left))

;; ---------------------------------------------------------------------------
;; Sync + async wrappers
;; ---------------------------------------------------------------------------

(defn get-sync
  "Synchronous HTTP GET with consistent exception handling."
  ([url] (get-sync url {}))
  ([url opts]
   (try
     (client/get url
       (merge {:client @http-client
               :throw false}
              opts))
     (catch Exception e
       {:status 500 :body "" :error e}))))

(defn post-sync
  "Synchronous HTTP POST with consistent exception handling."
  ([url] (post-sync url {}))
  ([url opts]
   (try
     (client/post url
       (merge {:client @http-client
               :throw false}
              opts))
     (catch Exception e
       {:status 500 :body "" :error e}))))

(defn get-async
  "Asynchronous HTTP GET returning CompletableFuture<response-map>."
  ([url] (get-async url {}))
  ([url opts]
   (client/get url
     (merge {:async true
             :client @http-client
             :throw false}
            opts))))

(defn post-async
  "Asynchronous HTTP POST returning CompletableFuture<response-map>."
  ([url] (post-async url {}))
  ([url opts]
   (client/post url
     (merge {:async true
             :client @http-client
             :throw false}
            opts))))

;; ---------------------------------------------------------------------------
;; Racing patterns
;; ---------------------------------------------------------------------------

(defn- cf->chan
  "Adapt CompletableFuture<response-map> to channel producing :right/:left."
  [^CompletableFuture cf]
  (let [c (async/chan 1)]
    (.whenComplete cf
      (reify BiConsumer
        (accept [_ result ex]
          (let [v (cond
                    ex     :left
                    result (response->verdict result)
                    :else  :left)]
            (async/put! c v)
            (async/close! c)))))
    c))

(defn race-futures
  "Race CompletableFutures and return first :right, else :left.
   Pending futures are cancelled to force connection cleanup."
  [futures]
  (let [futures (vec futures)
        chans   (mapv cf->chan futures)
        winner  (async/<!!
                  (async/go-loop [pending (vec (map vector futures chans))]
                    (if (empty? pending)
                      :left
                      (let [pending-chans (mapv second pending)
                            [v ch] (async/alts! pending-chans)]
                        (if (= :right v)
                          :right
                          (recur (filterv #(not= (second %) ch) pending)))))))]
    (doseq [^CompletableFuture cf futures]
      (when-not (.isDone cf)
        (.cancel cf true)))
    winner))

(defn race-urls
  "Race multiple URLs for first :right response."
  [urls]
  (race-futures (mapv get-async urls)))

;; ---------------------------------------------------------------------------
;; Virtual-thread support
;; ---------------------------------------------------------------------------

(defn run-on-vt
  "Run function `f` on the shared virtual-thread executor."
  [f]
  (.execute ^Executor @virtual-executor f))
