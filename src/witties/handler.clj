(ns witties.handler
  (:gen-class :main true)
  (:require [clojure.core.async :refer [<!! >! chan close! go put!]]
            [clojure.string :as string]
            [aleph.http :as http]
            [aleph.netty :as netty]
            [cheshire.core :as j]
            [compojure.core :refer [defroutes ANY GET POST rfn]]
            [compojure.response :refer [Renderable]]
            [manifold.stream :as stream]
            [pandect.algo.sha1 :refer [sha1-hmac]]
            [ring.middleware.absolute-redirects :refer [wrap-absolute-redirects]]
            [ring.middleware.json :refer [wrap-json-body]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.params :refer [wrap-params]]
            [schema.core :as s]
            [taoensso.timbre :as timbre :refer [debugf errorf infof warnf]]
            [witties.core :as core]
            [witties.db :as db]))

(defonce state (atom nil))

(timbre/set-level! :info)

(defn log
  [{:keys [body query-string request-method uri]} resp]
  (let [meth (-> request-method name string/upper-case)]
    (infof "%s %s%s body=%s -> %s body=%s" meth uri
           (if query-string (str "?" query-string) "") (pr-str body)
           (:status resp) (:body resp))
    resp))

(defmacro defhandler
  "Requires an :as directive when destructuring request."
  [handler args & body]
  `(defn ~handler
     ~args
     (go (let [req# ~(first args)
               body# (do ~@body)]
           (log (or (:as req#) req#) body#)
           body#))))

(defhandler index
  [request]
  {:body "hey" :status 200})

(defhandler fb-get
  [{:keys [params] {:keys [fb-verify-token]} :state :as request}]
  (if-let [challenge (and (= "subscribe" (get params "hub.mode"))
                          (= fb-verify-token (get params "hub.verify_token"))
                          (get params "hub.challenge"))]
    {:body challenge :status 200}
    {:body "Bad Request" :status 400}))

(defn verified?
  "Verifies origin."
  [{:keys [headers raw-body]} secret]
  (= (get headers "x-hub-signature")
     (str "sha1=" (sha1-hmac (slurp raw-body) secret))))

(s/defn messaging-entry->event :- (s/maybe core/Event)
  [{:keys [recipient sender timestamp]
    {:keys [attachments quick_reply sticker_id text]} :message
    {:keys [payload]} :postback}]
  (when (or attachments payload sticker_id text)
    (cond-> {:recipient (:id recipient)
             :sender (:id sender)
             :timestamp timestamp}
      attachments (assoc :attachments attachments)
      payload (assoc :postback payload)
      quick_reply (assoc :quick-reply (:payload quick_reply))
      sticker_id (assoc :sticker sticker_id)
      text (assoc :text text))))

(defn entry->secret
  "Returns the app secret from page id in `entry`."
  [entry]
  (some->> entry first :id core/bot-for-page second :fb-app-secret))

(defhandler fb-post
  [{:as request
    {:keys [entry object] :as body} :body
    {:keys [event-chan]} :state}]
  (when (and (verified? request (entry->secret entry))
             (= "page" object))
    (let [events (->> entry
                      (mapcat (comp (partial keep messaging-entry->event)
                                    :messaging))
                      (sort-by :timestamp))]
      (doseq [event events]
        (debugf "Sending event=%s" (pr-str (dissoc event :timestamp)))
        (put! event-chan (dissoc event :timestamp)))))
  {:status 200})

(defhandler ping
  [{:keys [request-method] :as request}]
  (cond-> {:body "pong" :status 200}
    ;; Compojure tries to assoc a nil body on the response on HEAD requests,
    ;; which throws an Exception as the response is wrapped in a chan
    (= :head request-method) (dissoc :body)))

(defhandler default
  [request]
  {:body "Not Found" :status 404})

;; Compojure would normally deref chans
;; see https://github.com/weavejester/compojure/blob/1.5.0/src/compojure/response.clj#L21
(extend-protocol Renderable
  clojure.core.async.impl.channels.ManyToManyChannel
  (render [c _] c))

(defroutes app-routes
  (GET "/" [] index)
  (GET "/fb" [] fb-get)
  (POST "/fb" [] fb-post)
  (ANY "/ping" [] ping)
  (rfn [] default))

(defn wrap-raw-body
  "Copies stream reference to `raw-body`, and marks initial position."
  [handler]
  (fn [req]
    (if-let [body (:body req)]
      (do (.mark body 1e6)
          (handler (assoc req :raw-body body)))
      (handler req))))

(defn wrap-reset-raw-body
  "Resets `raw-body` to its marked position."
  [handler]
  (fn [req]
    (when-let [body (:raw-body req)]
      (.reset body))
    (handler req)))

(defn wrap-deferred
  "Converts a chan to a manifold.deferred"
  [handler]
  (fn [req]
    (-> req
        handler
        stream/->source
        stream/take!)))

(defn wrap-state
  "Adds the state to the request"
  [handler]
  (fn [req]
    (handler (assoc req :state @state))))

(def app
  (-> app-routes
      wrap-deferred
      wrap-state
      wrap-reset-raw-body
      (wrap-json-body {:keywords? true})
      wrap-raw-body
      wrap-keyword-params
      wrap-params
      wrap-absolute-redirects))

(defn reset-state!
  []
  (when-let [{:keys [core-chan ctrl-chan event-chan http-server]} @state]
    (infof "Cleaning up...")
    (when http-server (.close http-server))
    (when ctrl-chan
      (put! ctrl-chan :stop)
      (close! ctrl-chan))
    (when event-chan (close! event-chan))
    (when core-chan (<!! core-chan))
    (reset! state nil)
    (infof "...all good!")))

(defn handle-uncaught-exceptions
  []
  (Thread/setDefaultUncaughtExceptionHandler
   (reify
     Thread$UncaughtExceptionHandler
     (uncaughtException [this thread t]
       (errorf t "Uncaught exception on thread=%s" thread)))))

(defn shutdown-hook
  []
  (reset-state!)
  (infof "Shutting down..."))

(defn -main
  [& args]
  (handle-uncaught-exceptions)
  (when-not @state
    (.addShutdownHook (Runtime/getRuntime) (Thread. shutdown-hook)))
  (reset-state!)
  (let [http-port (or (some-> (System/getenv "PORT") (Integer.)) 8080)
        fb-verify-token (System/getenv "FB_VERIFY_TOKEN")
        db-url (System/getenv "DATABASE_URL")
        event-chan (chan 100)
        ctrl-chan (chan)
        core-chan (core/init! db-url event-chan ctrl-chan)
        http-server (http/start-server app {:port http-port})
        state' {:http-server http-server
                :fb-verify-token fb-verify-token
                :db-url db-url
                :event-chan event-chan
                :ctrl-chan ctrl-chan
                :core-chan core-chan}]
    (reset! state state')
    (infof "Up and running.")
    ;; Aleph uses only daemon threads, prevent process from closing
    (netty/wait-for-close http-server)))
