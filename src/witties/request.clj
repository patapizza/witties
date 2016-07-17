(ns witties.request
  (require [clojure.core.async :refer [<! go]]
           [cheshire.core :as j]
           [org.httpkit.client :as http]
           [taoensso.timbre :refer [debugf warnf]]))

(defn req!>
  [meth url opts]
  (go (let [{:keys [status body error]} @(meth url opts)
            resp (try (j/decode body true)
                      (catch Exception e
                        (warnf e "couldn't parse JSON response body=%s" body)))]
        (if-let [err (or error (and (not= 200 status) body))]
          (do (warnf "received status=%s error=%s" status err)
              {:error err})
          resp))))

;; -----------------------------------------------------------------------------
;; Wit

(def wit-api-version 20160703)
(def wit-url "https://api.wit.ai")

(defn wit!>
  [access-token meth path opts]
  (go (let [default-opts {:headers {"Accept" (format "application/vnd.wit.%s+json" wit-api-version)
                                    "Authorization" (str "Bearer " access-token)
                                    "Content-Type" "application/json"}}
            opts (merge default-opts opts)
            {:keys [error] :as resp} (<! (req!> meth (str wit-url path) opts))]
        (debugf "Wit request: %s %s opts=%s -> %s"
                meth (str wit-url path) (pr-str opts) (pr-str resp))
        (if error
          (do (warnf "received error=%s" error)
              error)
          resp))))

(defn message!>
  [access-token message]
  (wit!> access-token http/get "/message" {:query-params {:q message}}))

(defn converse!>
  ([access-token session-id message]
   (converse!> access-token session-id message nil))
  ([access-token session-id message context]
   (let [opts (cond-> {:query-params {:session_id session-id}
                       :body (j/encode (or context {}))}
                message (assoc-in [:query-params :q] message))]
     (wit!> access-token http/post "/converse" opts))))

;; -----------------------------------------------------------------------------
;; Messenger

(def fb-url "https://graph.facebook.com/v2.6/me/messages")
(def quick-reply-max-len 20)

(defn fb!>
  [access-token meth opts]
  (go (let [default-opts {:headers {"Content-Type" "application/json"}
                          :query-params {"access_token" access-token}}
            opts (merge default-opts opts)
            resp (<! (req!> meth fb-url opts))]
        (debugf "FB request: %s %s opts=%s -> %s"
                meth fb-url (pr-str opts) (pr-str resp))
        resp)))

(defn maybe-truncate
  [text max-len]
  (cond-> text
    (< max-len (count text))
    (->> (take (- max-len 3))
         (apply str)
         (format "%s..."))))

(defn fb-message!>
  ([access-token recipient message]
   (fb-message!> access-token recipient message nil))
  ([access-token recipient message quickreplies]
   (let [mk-quickreply (fn [text]
                         {:content_type "text"
                          :title (maybe-truncate text quick-reply-max-len)
                          :payload text})
         payload {:recipient {:id recipient}
                  :message (cond-> {:text message}
                             (seq quickreplies) (assoc :quick_replies (map mk-quickreply quickreplies)))}
         opts {:body (j/encode payload)}]
     (fb!> access-token http/post opts))))
