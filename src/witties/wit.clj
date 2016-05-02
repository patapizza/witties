(ns witties.wit
  (require [clojure.core.async :refer [go]]
           [cheshire.core :as j]
           [org.httpkit.client :as http]))

(def wit-url "https://api.wit.ai")

(defn req>
  ([access-token meth path] (req> access-token meth path nil))
  ([access-token meth path opts]
   (go
     (let [default-opts {:headers {"Accept" "application/json"
                                   "Authorization" (str "Bearer " access-token)
                                   "Content-Type" "application/json"}}
           opts (merge default-opts (or opts {}))
           {:keys [status body error]} @(meth (str wit-url path) opts)
           resp (try (j/decode body true)
                     (catch Exception e
                       {:error "could not parse JSON response"}))]
       (if-let [err (or error (:error resp) (and (not= 200 status) status))]
         {:error err}
         resp)))))

(defn message>
  [access-token message]
  (req> access-token http/get "/message" {:query-params {:q message}}))

(defn converse>
  ([access-token session-id message]
   (converse> access-token session-id message nil))
  ([access-token session-id message context]
   (let [opts (cond-> {:query-params {:session_id session-id}
                       :body (j/encode (or context {}))}
                message (assoc-in [:query-params :q] message))]
     (req> access-token http/post "/converse" opts))))
