(defun perplex-handler (process message)
  ""
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

(setq account-create-relation "CREATE RELATION Account (AccountNumber INTEGER Balance INTEGER Limit INTEGER)")
(setq credit-create-relation "CREATE RELATION Credit (AccountNumber INTEGER Value INTEGER Date VARCHAR(10))")
(setq debit-create-relation "CREATE RELATION Debit (AccountNumber INTEGER Value INTEGER Date VARCHAR(10))")
(setq default-insert "INSERT Account (AccountNumber INTEGER 1 Balance INTEGER 500 Limit INTEGER 2000)")
(setq default-search "PROJECT (AccountNumber INTEGER) Account")

(perplex-client account-create-relation)
(perplex-client credit-create-relation)
(perplex-client debit-create-relation)
(perplex-client default-insert)
(perplex-client default-search)
