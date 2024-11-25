(setq *mailbox* nil)

(defun relational-engine-handler (process response)
  ""
  ;; (setq *mailbox* (json-read-from-string response))
  (setq *mailbox* response)
  (let ((response (process-get process :response)))
    (message "Received: %s" response)    
    (delete-process process)))

(setq relational-engine-address "localhost")
(setq relational-engine-port 7524)

(defun relational-client (content)
  ""
  (let ((connection (open-network-stream "relational-engine" "*relational-engine-socket*" relational-engine-address relational-engine-port)))
    (process-put connection :response nil)
    (set-process-filter connection 'relational-engine-handler)
    (process-send-string connection content)))

(setq msg "Hello Relational Engine! :)")

(relational-client msg)
*mailbox*
