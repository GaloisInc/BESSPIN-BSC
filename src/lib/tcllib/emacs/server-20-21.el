
(require 'server)
(require 'cl)
(load "cl-macs")

(setq server-name (format "server-%d" (emacs-pid)))

(setq server-socket-dir
  (format "/tmp/emacs%d" (user-uid)))

(defun server-start-20-21 (&optional leave-dead)
  "Allow this Emacs process to be a server for client processes.
This starts a server communications subprocess through which
client \"editors\" can send your editing commands to this Emacs job.
To use the server, set up the program `emacsclient' in the
Emacs distribution as your standard \"editor\".

Prefix arg means just kill any existing server communications subprocess."
  (interactive "P")
  ;; kill it dead!
  (if server-process
      (progn
	(set-process-sentinel server-process nil)
	(condition-case () (delete-process server-process) (error nil))))
  ;; Delete the socket files made by previous server invocations.
  ;; Delete the socket files made by previous server invocations.
  (let* ((sysname (system-name))
	 (dot-index (string-match "\\." sysname))
	 (filename (format "/tmp/esrv%d-%s" (user-uid) sysname))
	 (filename_new (format "%s/%s" server-socket-dir server-name))
	 (shortname)
	 (donep nil)
	 )
    (server-ensure-safe-dir server-socket-dir)
    (condition-case ()
	(delete-file (format "~/.emacs-server-%s" sysname))
      (error nil))
    (condition-case ()
	(delete-file filename)
      (error nil))
    (condition-case ()
	(delete-file filename_new)
      (error nil))
    ;; In case the server file name was made with a domainless hostname,
    ;; try deleting that name too.
    (if dot-index
	(let ()
	  (setq shortname (substring sysname 0 dot-index))
	  (condition-case ()
	      (delete-file (format "~/.emacs-server-%s" shortname))
	    (error nil))
	  (setq shortname (format "/tmp/esrv%d-%s" (user-uid) shortname))
	  (condition-case ()
	      (delete-file shortname)
	    (error nil))
	  ))
    ;; If this Emacs already had a server, clear out associated status.
    (while server-clients
      (let ((buffer (nth 1 (car server-clients))))
	(server-buffer-done buffer)))
    (if leave-dead
	nil
      (if server-process
	  (server-log (message "Restarting server")))
      ;; Using a pty is wasteful, and the separate session causes
      ;; annoyance sometimes (some systems kill idle sessions).
      (let ((process-connection-type nil))
	(setq server-process (start-process "server" nil server-program)))
      (set-process-sentinel server-process 'server-sentinel-20-21-restart)
      (set-process-filter server-process 'server-process-filter-20-21)
      ;; We must receive file names without being decoded.  Those are
      ;; decoded by server-process-filter accoding to
      ;; file-name-coding-system.
      (set-process-coding-system server-process 'raw-text 'raw-text)
      (process-kill-without-query server-process))
    (while (not donep)
      (if (file-exists-p filename)
	  (let ()
	    (set-file-modes filename 511)
            ;;; 511 is 777 in hex
 	    (rename-file 
 	     filename
 	     filename_new
 	     t)
	    (setq donep t)))
      (if (and dot-index (file-exists-p shortname))
	  (let ()
	    (set-file-modes shortname 511)
            ;;; 511 is 777 in hex
 	    (rename-file 
 	     shortname
 	     filename_new
 	     t)
	    (setq donep t))))
    ))

;Process a request from the server to edit some files.
;Format of STRING is "Client: CLIENTID PATH PATH PATH... \n"
(defun server-process-filter-20-21 (proc string)
  (server-log string)
  (setq string (concat server-previous-string string))
  ;; If the input is multiple lines,
  ;; process each line individually.
  (while (string-match "\n" string)
    (let ((request (substring string 0 (match-beginning 0)))
	  (coding-system (and default-enable-multibyte-characters
			      (or file-name-coding-system
				  default-file-name-coding-system)))
	  client nowait eval
	  (files nil)
	  (lineno 1)
	  (columnno 0))
      ;; Remove this line from STRING.
      (setq string (substring string (match-end 0)))	  
      (if (string-match "^Error: " request)
	  (message "Server error: %s" (substring request (match-end 0)))
	(if (string-match "^Client: " request)
	    (progn
	      (setq request (substring request (match-end 0)))
	      (setq client (list (substring request 0 (string-match " " request))))
	      (setq request (substring request (match-end 0)))
	      (while (string-match "[^ ]+ " request)
		(let ((arg
		       (substring request (match-beginning 0) (1- (match-end 0))))
		      (pos 0))
		  (setq request (substring request (match-end 0)))
		  (cond ((string-match "\\`-nowait" arg) (setq nowait t))
			((string-match "\\`-eval" arg) (setq eval t))
    			;; ARG is a line number option.
			((string-match "\\`\\+[0-9]+\\'" arg)
			 (setq lineno (string-to-int (substring arg 1))))
			;; ARG is line number:column option. 
			((string-match "\\`+\\([0-9]+\\):\\([0-9]+\\)\\'" arg)
			 (setq lineno (string-to-int (match-string 1 arg))
			       columnno (string-to-int (match-string 2 arg))))
			(t 
			 ;; Undo the quoting that emacsclient does
			 ;; for certain special characters.
			 (setq arg (server-unquote-arg arg))
			 ;; Now decode the file name if necessary.
			 (when coding-system
			   (setq arg (decode-coding-string arg coding-system)))
			 (if eval
			     (let* (errorp
				    (v (condition-case errobj
					   (eval (car (read-from-string arg)))
					 (error (setq errorp t) errobj))))
			       (when v
				 (with-temp-buffer
				   (let ((standard-output (current-buffer)))
				     (when errorp (princ "error: "))
				     (pp v)
				     (ignore-errors
				       (process-send-region proc (point-min) (point-max)))
				     ))))
			   ;; ARG is a file name.
			   ;; Collapse multiple slashes to single slashes.
			   (setq arg (command-line-normalize-file-name arg))
			   ;; Undo the quoting that emacsclient does
			   ;; for certain special characters.
			   (while (string-match "&." arg pos)
			     (setq pos (1+ (match-beginning 0)))
			     (let ((nextchar (aref arg pos)))
			       (cond ((= nextchar ?&)
				      (setq arg (replace-match "&" t t arg)))
				     ((= nextchar ?-)
				      (setq arg (replace-match "-" t t arg)))
				     (t
				      (setq arg (replace-match " " t t arg))))))
			   ;; Now decode the file name if necessary.
			   (if coding-system
			       (setq arg (decode-coding-string arg coding-system)))
			   (setq files
				 (cons (list arg lineno columnno)
				       files))
			   (setq lineno 1)
			   (setq columnno 0)
			   )))))
	      (server-visit-files files client nowait)
	      ;; CLIENT is now a list (CLIENTNUM BUFFERS...)
	      (if (null (cdr client))
		  ;; This client is empty; get rid of it immediately.
		  (progn
		    (send-string server-process 
				 (format "Close: %s \n" (car client)))
		    (server-log (format "Close empty client: %s \n" (car client))))
		;; We visited some buffer for this client.
		(or nowait
		    (setq server-clients (cons client server-clients)))
		(server-switch-buffer (nth 1 client))
		(run-hooks 'server-switch-hook)
		(message (substitute-command-keys
			  "When done with a buffer, type \\[server-edit]"))))))))
  ;; Save for later any partial line that remains.
  (setq server-previous-string string))

(defun server-sentinel-20-21-restart (proc msg)
  (cond ((eq (process-status proc) 'exit)
	 (server-log (message "Server subprocess exited")))
	((eq (process-status proc) 'signal)
	 (server-log (message "Server subprocess killed -- restarting"))
	 (server-start-20-21)
	 )))


(defun server-unquote-arg (arg)
  (replace-regexp-in-string
   "&." (lambda (s)
	  (case (aref s 1)
	    (?& "&")
	    (?- "-")
	    (?n "\n")
	    (t " ")))
   arg t t))

(defun server-ensure-safe-dir (dir)
  "Make sure DIR is a directory with no race-condition issues.
Creates the directory if necessary and makes sure:
- there's no symlink involved
- it's owned by us
- it's not readable/writable by anybody else."
  (setq dir (directory-file-name dir))
  (let ((attrs (file-attributes dir)))
    (unless attrs
      (letf (((default-file-modes) ?\700)) (make-directory dir t))
      (setq attrs (file-attributes dir)))
    ;; Check that it's safe for use.
    (unless (and (eq t (car attrs)) (equal (nth 2 attrs) (user-uid))
                 (or (eq system-type 'windows-nt)
                     (zerop (logand ?\077 (file-modes dir)))))
      (error "The directory %s is unsafe" dir))))

(defmacro ignore-errors (&rest body)
  "Execute FORMS; if an error occurs, return nil. Otherwise, return result of last FORM."
  (let ((err (gensym)))
    (list 'condition-case err (cons 'progn body) '(error nil))))

(provide 'server-20-21)
