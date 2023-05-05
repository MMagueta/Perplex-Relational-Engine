(ns express.connector
  (:import [java.sql Connection Statement ResultSet])
  (:require [clojure.java.io :as io]))

(defn Connector [host port db user password]
  (reify Connection
    (close [_]
      nil)
    (createStatement [this]
      (reify Statement
        (close [_]
          nil)
        (executeQuery [_ sql]
          (with-open [socket (java.net.Socket. host port)
                      out (.getOutputStream socket)
                      in (.getInputStream socket)]
            (.write out (byte-array (map #'byte sql)))
            (.flush out)
            (let [response (slurp (io/reader in))
                  data response]
              (println response)
              (proxy [ResultSet] []
                (next [] true)
                (getObject [^Integer columnIndex] data)
                (wasNull [] false)
                (close [] nil)))))))))





(def db (Connector "0.0.0.0" 4000 "mydb" "username" "password"))
(.getObject (.executeQuery (.createStatement db) "CREATE RELATION Hello (Message VARCHAR(10))") 0)
(.getObject (.executeQuery (.createStatement db) "INSERT Hello (Message VARCHAR(10) \"Hey there! :)\")") 0)

(.getObject (.executeQuery (.createStatement db) "CREATE RELATION Person (ID INTEGER Email VARCHAR(32) Name VARCHAR(10))") 0)
(.getObject (.executeQuery (.createStatement db) "INSERT Person (ID INTEGER 0 Email VARCHAR(32) \"test@email.com\" Name VARCHAR(10) \"Mathew\")") 0)
(.getObject (.executeQuery (.createStatement db) "INSERT Person (ID INTEGER 1 Email VARCHAR(32) \"luke-xyz@email.com\" Name VARCHAR(10) \"Luke\")") 0)
(.getObject (.executeQuery (.createStatement db) "INSERT Person (ID INTEGER 2 Email VARCHAR(32) \"mark-yehaw@email.com\" Name VARCHAR(10) \"Mark\")") 0)
(.getObject (.executeQuery (.createStatement db) "INSERT Person (ID INTEGER 3 Email VARCHAR(32) \"pro-logos@email.com\" Name VARCHAR(10) \"John\")") 0)

(def socket (java.net.Socket. "0.0.0.0" 4000))
(def out (.getOutputStream socket))
(def in (.getInputStream socket))
(.write out (byte-array (map #'byte "TEST")))
(.flush out)
(let [response (slurp (io/reader in))]
  (println response))
