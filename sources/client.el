(setq *mailbox* nil)

(defun perplex-handler (process response)
  ""
  ;; (setq *mailbox* (json-read-from-string response))
  (setq *mailbox* response)
  (let ((response (process-get process :response)))
    (message "Received: %s" response)    
    (delete-process process)))

(setq perplex-address "localhost")
(setq perplex-port 4000)

(defun perplex-client (content)
  ""
  (let ((connection (open-network-stream "perplexdb" "*perplex-tcp*" perplex-address perplex-port)))
    (process-put connection :response nil)
    (set-process-filter connection 'perplex-handler)
    (process-send-string connection content)))

(setq account-create-relation "LOCK WRITE CREATE RELATION Account (AccountNumber INTEGER Balance INTEGER Limit INTEGER)")
(setq credit-create-relation "LOCK WRITE CREATE RELATION Credit (AccountNumber INTEGER Value INTEGER)")
(setq debit-create-relation "LOCK WRITE CREATE RELATION Debit (AccountNumber INTEGER Value INTEGER)")
(setq default-insert "LOCK WRITE INSERT Account (AccountNumber INTEGER 5 Balance INTEGER 500 Limit INTEGER 2000)")
(setq insert-credit "LOCK WRITE INSERT Credit (AccountNumber INTEGER 5 Value INTEGER 500)")
(setq insert-debit "LOCK WRITE INSERT Debit (AccountNumber INTEGER 5 Value INTEGER 250)")
(setq default-search "LOCK READ PROJECT ALL Account SELECT AccountNumber = 5")
(setq default-sum "LOCK READ PROJECT SUM(Value INTEGER) Debit SELECT AccountNumber = 5")
(setq default-minus "LOCK WRITE UPDATE Account SET Balance (PROJECT SUM(Value INTEGER) Credit SELECT AccountNumber = 5 - PROJECT SUM(Value INTEGER) Debit SELECT AccountNumber = 5) SELECT AccountNumber = 5")
(setq project-debit "LOCK READ PROJECT SUM(Value INTEGER) Debit SELECT AccountNumber = 5")
(setq project-credit "LOCK READ PROJECT SUM(Value INTEGER) Credit SELECT AccountNumber = 5")

(setq transact "BEGIN Account Debit Credit
        INSERT Account (AccountNumber INTEGER 1 Balance INTEGER 500 Limit INTEGER 2000)
        UPDATE Account SET AccountNumber (PROJECT SUM(Value INTEGER) Credit SELECT AccountNumber = 1 - PROJECT SUM(Value INTEGER) Debit SELECT AccountNumber = 1) SELECT AccountNumber = 1
        END")

(perplex-client transact)

(perplex-client account-create-relation)
(perplex-client credit-create-relation)
(perplex-client debit-create-relation)

(perplex-client project-debit)
(perplex-client project-credit)

(perplex-client insert-debit)
(perplex-client insert-credit)
(perplex-client insert-credit)
(dotimes (i 100)
  (perplex-client default-insert))
(perplex-client default-search)
(perplex-client default-sum)
(perplex-client default-minus)
;; (perplex-client default-update)
*mailbox*
