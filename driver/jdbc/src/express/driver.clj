(ns express.driver
  (:import [java.sql Driver Connection SQLException DriverPropertyInfo])
  (:gen-class) 
  (:require
    [clojure.java.jdbc :as jdbc]))

(defn make-connection [props]
  (let [host (get props "host")
        port (get props "port")
        database (get props "database")
        user (get props "user")
        password (get props "password")
        url (str "jdbc:expressdb://" host ":" port "/" database)]
    (doto (java.sql.DriverManager/getConnection url user password)
      (.setAutoCommit false))))

(defn ExpressDriver []
  (reify Driver
    (connect [this url props]
      (assert (.acceptsURL this url) (str "Malformed URL: " url))
      (let [conn (make-connection props)]
        (if conn
          (.getConnection conn)
          (throw (SQLException. "Unable to connect")))))
    
    (acceptsURL [this url]
      (= "jdbc:expressdb://" (subs url 0 17)))

    (getPropertyInfo [this url props]
      (let [host-props (doto (DriverPropertyInfo. "host" (get props "host"))
                        (.setRequired true)
                        (.setDescription "The hostname or IP address of the database server."))
            port-props (doto (DriverPropertyInfo. "port" (get props "port"))
                         (.setRequired true)
                         (.setDescription "The port number of the database server."))
            database-props (doto (DriverPropertyInfo. "database" (get props "database"))
                             (.setRequired true)
                             (.setDescription "The name of the database to connect to."))
            user-props (doto (DriverPropertyInfo. "user" (get props "user"))
                         (.setRequired true)
                         (.setDescription "The username to use when connecting to the database."))
            password-props (doto (DriverPropertyInfo. "password" (get props "password"))
                             (.setRequired true)
                             (.setDescription "The password to use when connecting to the database."))]
        [host-props port-props database-props user-props password-props]))
      ;; (to-array []))

    (getMajorVersion [this]
      1)

    (getMinorVersion [this]
      0)

    (jdbcCompliant [this]
      false)

    (getParentLogger [this]
      nil)))

(def expressdb {:dbtype "Express"
                :dbname "expressdb"
                :classname (class (ExpressDriver))})


(defn -main [& _args]
  ;; (let [props (java.util.Properties.)]
  ;; (.setProperty props "a" "a")
  (jdbc/query expressdb
              ["PROJECT 'Hello World! :)'"]
              {:row-fn :cost}))
  ;; (jdbc/register-driver! (ExpressDriver))
  ;; (.connect (ExpressDriver) "" nil)
  ;; (println (keys (ns-publics 'express.driver))))


