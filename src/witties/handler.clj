(ns witties.handler
  (:require [clojure.core.async :refer [<!! >! chan close! go put!]]
            [clojure.string :as string]
            [clojure.tools.nrepl.server :as nrepl]
            [aleph.http :as http]
            [cheshire.core :as j]
            [compojure.core :refer [defroutes GET POST rfn]]
            [compojure.response :refer [Renderable]]
            [manifold.stream :as stream]
            [plumbing.core :refer [update-in-when]]
            [ring.middleware.absolute-redirects :refer [wrap-absolute-redirects]]
            [ring.middleware.json :refer [wrap-json-body]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.params :refer [wrap-params]]
            [taoensso.timbre :refer [debugf infof warnf]]
            [witties.core :as core]))

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
     (let [req# ~(first args)
           body# (do ~@body)]
       (log (or (:as req#) req#) body#)
       (go body#))))

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

(defn reset-state! []
  (when-let [{:keys [core-chan ctrl-chan event-chan http-server nrepl-server]} @state]
    (when http-server (.close http-server))
    (when nrepl-server (nrepl/stop-server nrepl-server))
    (when ctrl-chan
      (put! ctrl-chan :stop)
      (close! ctrl-chan))
    (when event-chan (close! event-chan))
    (when core-chan (<!! core-chan))
    (reset! state nil)))

(defn -main
  [& args]
  (reset-state!)
  (let [http-port (or (some-> (System/getenv "HTTP_PORT") (Integer.)) 8080)
        nrepl-port (or (some-> (System/getenv "NREPL_PORT") (Integer.)) 8090)
        fb-verify-token (System/getenv "FB_VERIFY_TOKEN")
        event-chan (chan)
        ctrl-chan (chan)
        core-chan (core/init! event-chan ctrl-chan)
        state' {:http-server (http/start-server app {:port http-port})
                :nrepl-server (nrepl/start-server :bind "0.0.0.0"
                                                  :port nrepl-port)
                :fb-verify-token fb-verify-token
                :event-chan event-chan
                :ctrl-chan ctrl-chan
                :core-chan core-chan}]
    (reset! state state')))
