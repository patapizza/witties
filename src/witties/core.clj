(ns witties.core
  (:require [clojure.core.async :refer [<! alts! go go-loop timeout]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clj-time.coerce :as c]
            [clj-time.core :as t]
            [plumbing.core :refer [map-from-keys map-vals]]
            [schema.core :as s]
            [taoensso.timbre :refer [debugf infof warnf]]
            [witties.bots.quickstart]
            [witties.bots.woodpecker]
            [witties.db :as db]
            [witties.request :as req])
  (:import [java.util UUID]))

(def bots
  "Bot config, with sessions.
   Shape:
   {:bot {:wit-token \"\"
          :fb-page-token \"\"
          :fb-page-id \"42\"
          :fb-app-secret \"abc\"
          :threads {\"42\" {:session-id \"\"
                            :started-at 241423535
                            :context {}}}}}"
  (atom nil))

(def Attachment
  {:type (s/enum "audio" "image" "video")
   :payload {:url s/Str}})
(def Event
  (s/constrained
   {:sender s/Str
    :recipient s/Str
    (s/optional-key :attachments) [Attachment]
    (s/optional-key :postback) s/Str
    (s/optional-key :sticker) s/Num
    (s/optional-key :text) s/Str}
   (some-fn :attachments :postback :sticker :text)))
(def event-checker
  (s/checker Event))

(def graceful-stop-ms 25000) ;; 25 seconds (30 seconds max on Heroku)
(def max-steps 10)
(def session-expire-ms (* 1000 60 3)) ;; 3 minutes

;; TODO: use fb-page-id as bot-id
(defn bot-for-page
  [page-id]
  (some (fn [[bot params]]
          (when (= page-id (:fb-page-id params))
            [bot params]))
        @bots))

(defn ->bot-fn
  [bot f]
  {:pre [(or (string? bot) (keyword? bot))
         (string? f)]}
  (let [qualified-ns (str "witties.bots." (cond-> bot (keyword? bot) name))]
    (ns-resolve (symbol qualified-ns) (symbol f))))

;; TODO refactoring
(defn run-actions!>
  ([bot params thread-id session-id user-msg]
   (run-actions!> bot params thread-id session-id user-msg nil))
  ([bot {:keys [wit-token] :as params} thread-id session-id user-msg context]
   {:pre [(keyword? bot) thread-id wit-token]}
   (let [step!> (fn step!> [msg context steps]
                  (go (if (>= 0 steps)
                        (do (warnf "%s - Max steps reached" bot)
                            context)
                        (let [{:keys [action entities error msg quickreplies type]}
                              (<! (req/converse!> wit-token session-id msg context))]
                          (cond
                            error
                            (warnf "%s - Converse error=%s" bot error)

                            (= "stop" type)
                            (do (infof "%s - Stopping." bot)
                                context)

                            (= "msg" type)
                            (do (infof "%s - Executing say!> with thread-id=%s context=%s msg=%s"
                                       bot session-id (pr-str context) msg)
                                (<! ((->bot-fn bot "say!>") params thread-id context msg quickreplies))
                                (<! (step!> nil context (dec steps))))

                            (= "merge" type)
                            (let [_ (infof "%s - Executing merge!> with thread-id=%s context=%s entities=%s user-msg=%s"
                                           bot thread-id (pr-str context) (pr-str entities) user-msg)
                                  context' (<! ((->bot-fn bot "merge!>") params thread-id context entities user-msg))]
                              (<! (step!> nil context' (dec steps))))

                            (= "action" type)
                            (let [action (str action "!>")
                                  _ (infof "%s - Executing %s with thread-id=%s context=%s"
                                           bot action thread-id (pr-str context))
                                  context' (<! ((->bot-fn bot action) params thread-id context))]
                              (<! (step!> nil context' (dec steps)))))))))]
     (step!> user-msg context max-steps))))

(defn get-or-create-session!
  "Sessions expire after `session-expire-ms`."
  [bot thread-id]
  (let [now-ms (-> (t/now) c/to-long)
        existing (get-in @bots [bot :threads thread-id])]
    (if (and existing (< (- now-ms session-expire-ms) (:started-at existing)))
      existing
      (let [new-session {:session-id (str (UUID/randomUUID))
                         :started-at now-ms}]
        (swap! bots assoc-in [bot :threads thread-id] new-session)
        new-session))))

(defn stop-bots!>
  "Gives each bot allowed-ms time to gracefully stop."
  ([bots] (stop-bots!> bots graceful-stop-ms))
  ([bots allowed-ms]
   (let [bot->chan (->> bots
                        (map-from-keys #(when-let [f (->bot-fn % "stop!>")] (f)))
                        (remove (fn [[_ v]] (nil? v)))
                        (into {})
                        (merge {:timeout (timeout allowed-ms)}))]
     (go-loop [chans (vals bot->chan)]
       (when (< 1 (count chans))
         (let [[_ c] (alts! chans)]
           (when-not (= c (bot->chan :timeout))
             (recur (remove (partial = c) chans)))))))))

(defn stop!>
  "Saves sessions in db and gives bots a chance to do it too."
  [db-url]
  (go (when db-url
        (db/exec! db-url "truncate table sessions")
        (->> @bots
             (mapcat (fn [[bot {:keys [threads]}]]
                       (map (fn [[thread-id session]]
                              (assoc session :bot bot :thread-id thread-id))
                            threads)))
             (db/insert-rows! db-url :sessions)))
      (<! (stop-bots!> (keys @bots)))))

(defn init!
  [db-url in ctrl]
  (when db-url
    (db/setup! db-url)
    (when-let [res (seq (db/q db-url "select * from bots"))]
      (infof "Loading bots: %s" (->> res
                                     (mapv (comp string/capitalize :bot))
                                     (string/join ", ")))
      (->> res
           (pmap (fn [{:keys [bot] :as bot-config}]
                   (when-let [f (->bot-fn bot "init!")]
                     (f (assoc bot-config :db-url db-url)))
                   (let [threads (->> ["select * from sessions where bot = ? order by started_at desc" bot]
                                      (db/q db-url)
                                      (group-by :thread-id)
                                      (map-vals first))]
                     [(keyword bot) (cond-> (dissoc bot-config :bot)
                                      (seq threads) (assoc :threads threads))])))
           (into {})
           (reset! bots))))

  ;; Event loop
  ;; TODO group events per bot, one worker per bot
  (go-loop []
    (let [[v c] (alts! [in ctrl])]
      (if (= c ctrl)
        (<! (stop!> db-url))
        (let [err (event-checker v)
              {:keys [recipient sender text]} v
              [bot params] (bot-for-page recipient)]
          (cond
            err (warnf "malformed event %s err=%s" v (pr-str err))
            (not bot) (warnf "couldn't find bot for recipient=%s" recipient)
            (not text) (let [msg "Oops I can only deal with text right now."]
                         (infof "Got no text for bot=%s from thread-id=%s. Executing say!> with msg=%s"
                                bot sender msg)
                         (<! ((->bot-fn bot "say!>") (dissoc params :threads) sender {} msg nil)))
            :else (let [{:keys [session-id context]} (get-or-create-session! bot sender)]
                    (debugf "Running actions for bot=%s thread-id=%s session-id=%s text=%s context=%s"
                            bot sender session-id text (pr-str context))
                    (some->> (run-actions!> bot (dissoc params :threads) sender session-id text context)
                             <!
                             (swap! bots assoc-in [bot :threads sender :context]))))
          (recur))))))
