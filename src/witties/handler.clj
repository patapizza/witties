(ns witties.handler
  (:require [clojure.core.async :refer [go]]
            [clojure.tools.nrepl.server :as nrepl]
            [aleph.http :as http]
            [compojure.core :refer [defroutes GET POST rfn]]
            [compojure.response :refer [Renderable]]
            [manifold.stream :as stream]
            [ring.middleware.absolute-redirects :refer [wrap-absolute-redirects]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.params :refer [wrap-params]]))

(defonce state (atom nil))

(defn index-handler>
  [request]
  (go {:body "hey" :status 200}))

(defn fb-get-handler>
  [{:keys [params] {:keys [fb-verify-token]} :state :as request}]
  ;; TODO save origin and check in fb-post-handler
  (go (if (and (= (get params "hub.mode") "subscribe")
               (= (get params "hub.verify_token") fb-verify-token)
               (get params "hub.challenge"))
        {:status 200
         :body (get params "hub.challenge")}
        {:status 400})))

(defn fb-post-handler>
  [request]
  (go {:status 200}))

(defn default-handler>
  [request]
  (go {:body "Not Found" :status 404}))

;; Compojure would normally deref chans
;; see https://github.com/weavejester/compojure/blob/1.5.0/src/compojure/response.clj#L21
(extend-protocol Renderable
  clojure.core.async.impl.channels.ManyToManyChannel
  (render [c _] c))

(defroutes app-routes
  (GET "/" [] index-handler>)
  (GET "/fb" [] fb-get-handler>)
  (POST "/fb" [] fb-post-handler>)
  (rfn [] default-handler>))

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
      wrap-keyword-params
      wrap-params
      wrap-absolute-redirects
      wrap-state))

(defn reset-state! []
  (when-let [{:keys [http-server nrepl-server]} @state]
    (when http-server (.close http-server))
    (when nrepl-server (nrepl/stop-server nrepl-server))
    (reset! state nil)))

(defn -main
  [& args]
  ;; TODO protect connections on nrepl-port
  (reset-state!)
  (let [http-port (or (System/getenv "HTTP_PORT") 8080)
        nrepl-port (or (System/getenv "NREPL_PORT") 8090)
        fb-verify-token (System/getenv "FB_VERIFY_TOKEN")
        state' {:http-server (http/start-server app {:port http-port})
                :nrepl-server (nrepl/start-server :bind "0.0.0.0"
                                                  :port nrepl-port)
                :fb-verify-token fb-verify-token}]
    (reset! state state')))
