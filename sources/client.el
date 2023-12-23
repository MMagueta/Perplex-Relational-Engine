(defun perplex-handler (process message)
  ""
  (let ((response (process-get process :response)))
    (message "Received: %s" response)
    (delete-process process)))

(defun perplex-client ()
  ""
  (interactive)
  (let* ((address (read-string "Server Address: "))
	 (port (read-number "Server Port: "))
	 (connection (open-network-stream "perplexdb" "*perplex-tcp*" address port))
	 (content (read-string "Query: ")))
    (process-put connection :response nil)
    (set-process-filter connection 'perplex-handler)
    (process-send-string connection content)))

(setq default-create-relation "CREATE RELATION Person (Name VARCHAR(10) Age INTEGER)")
(setq default-insert "INSERT Person (Name VARCHAR(10) \"Marcos\" Age INTEGER 24)")
