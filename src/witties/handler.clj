(ns witties.handler
  (:gen-class :main true)
  (:require [clojure.core.async :refer [<!! >! chan close! go put!]]
            [clojure.string :as string]
            [aleph.http :as http]
            [aleph.netty :as netty]
            [cheshire.core :as j]
            [compojure.core :refer [defroutes GET POST rfn]]
            [compojure.response :refer [Renderable]]
            [manifold.stream :as stream]
            [plumbing.core :refer [update-in-when]]
            [ring.middleware.absolute-redirects :refer [wrap-absolute-redirects]]
            [ring.middleware.json :refer [wrap-json-body]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.params :refer [wrap-params]]
            [taoensso.timbre :refer [debugf errorf infof warnf]]
            [witties.core :as core]
            [witties.db :as db]))

(defonce state (atom nil))

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
  ;; TODO save origin and check in fb-post-handler
  (if (and (= (get params "hub.mode") "subscribe")
           (= (get params "hub.verify_token") fb-verify-token)
           (get params "hub.challenge"))
    {:status 200
     :body (get params "hub.challenge")}
    {:status 400}))

(defhandler fb-post
  [{:keys [body] {:keys [event-chan]} :state :as request}]
  (when (= "page" (:object body))
    (let [events (->> (:entry body)
                      (mapcat (fn [{:keys [messaging]}]
                                (keep (fn [{:keys [message recipient sender timestamp]}]
                                        ;; Ignoring everything but text messages
                                        (when-let [text (:text message)]
                                          {:recipient (:id recipient)
                                           :sender (:id sender)
                                           :text text
                                           :timestamp timestamp}))
                                      messaging)))
                      (sort-by :timestamp))]
      (doseq [event events]
        (debugf "Sending event=%s" (pr-str (dissoc event :timestamp)))
        (put! event-chan (dissoc event :timestamp)))))
  {:status 200})

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
  (rfn [] default))

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
      (wrap-json-body {:keywords? true})
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
       (errorf t "Uncaught exceptionon thread=%s" thread)))))

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
        event-chan (chan)
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
