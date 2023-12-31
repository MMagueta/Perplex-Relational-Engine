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

(setq default-create-relation "CREATE RELATION Person (Name VARCHAR(10) Age INTEGER)")
(setq default-insert "INSERT Person (Name VARCHAR(10) \"Marc\" Age INTEGER 25)")
(setq default-search "PROJECT (Name VARCHAR(10)) Person")

(perplex-client default-create-relation)
(perplex-client default-insert)
(perplex-client default-search)
