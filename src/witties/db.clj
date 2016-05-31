(ns witties.db
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.string :as string]
            [cheshire.core :as j]
            [taoensso.timbre :refer [debugf errorf infof warnf]])
  (:import [clojure.lang IPersistentMap Keyword]
           [org.postgresql.util PGobject]))

;; JSON conversion
;; http://hiim.tv/clojure/2014/05/15/clojure-postgres-json/

(extend-protocol jdbc/IResultSetReadColumn
  PGobject
  (result-set-read-column [pgobj metadata idx]
    (let [type (.getType pgobj)
          value (.getValue pgobj)]
      (cond-> value
        (= "json" type) (j/decode true)))))

(extend-protocol jdbc/ISQLValue
  IPersistentMap
  (sql-value [value]
    (doto (PGobject.)
      (.setType "json")
      (.setValue (j/encode value))))
  Keyword
  (sql-value [value]
    (doto (PGobject.)
      (.setType "varchar")
      (.setValue (name value)))))

(defn create-table-if-not-exists-ddl
  [& args]
  (-> (apply jdbc/create-table-ddl args)
      (string/replace-first #"(CREATE TABLE)" "$1 IF NOT EXISTS")))

(defn drop-table-if-exists-ddl
  [& args]
  (-> (apply jdbc/drop-table-ddl args)
      (string/replace-first #"(DROP TABLE)" "$1 IF EXISTS")))

(defn q
  [db sql-params]
  (try
    (jdbc/query db sql-params {:identifiers #(string/replace % #"_" "-")})
    (catch Exception e
      (errorf e "Error while running query with sql-params=%s"
              (pr-str sql-params)))))

(defn exec!
  [db sql-params]
  (try
    (jdbc/execute! db sql-params)
    (catch Exception e
      (errorf e "Error while executing with sql-params=%s"
              (pr-str sql-params)))))

(defn insert-rows!
  [db table rows]
  (try
    (jdbc/insert-multi! db table rows {:entities #(string/replace % #"-" "_")})
    (catch Exception e
      (errorf e "Error while inserting rows=%s in table=%s"
              (pr-str rows) table))))

(def bots-table-query
  (create-table-if-not-exists-ddl :bots [[:bot "varchar(20)" :primary :key]
                                         [:wit_token :text]
                                         [:fb_page_id "varchar(64)"]
                                         [:fb_page_token :text]]))
(def sessions-table-query
  (create-table-if-not-exists-ddl :sessions [[:bot "varchar(20)"]
                                             [:thread_id "varchar(64)"]
                                             [:session_id "varchar(64)"]
                                             [:started_at :bigint]
                                             [:context :json]]))

(def woodpecker-table-query
  (create-table-if-not-exists-ddl :woodpecker [[:thread_id "varchar(64)"]
                                               [:at :bigint]
                                               [:about "varchar(50)"]
                                               [:message "varchar(100)"]]))

(defn setup!
  [db]
  (jdbc/db-do-commands db [bots-table-query
                           sessions-table-query
                           woodpecker-table-query]))

(defn teardown-yes-I-m-sure!
  [db]
  (->> [:bots :sessions :woodpecker]
       (map drop-table-if-exists-ddl)
       (jdbc/db-do-commands db)))
