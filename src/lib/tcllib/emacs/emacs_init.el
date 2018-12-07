
(create-file-buffer "*server*")

(setq load-path 
      (cons 
       (concat (expand-file-name (getenv "BLUESPECDIR")) "/tcllib/emacs") 
       load-path))

;; make sure system name is set correctly
(when (equal system-name "localhost.localdomain")
  (let ((name (shell-command-to-string "uname -n")))
    (when (> (length name) 0)
      (setq system-name (substring name 0 -1)))))

(cond  
 ((string-match "^20." emacs-version)
  (require 'server-20-21)
  (require 'easy-mmode-20)
  (setq server-name (format "server-%d" (emacs-pid)))
  (server-start-20-21))
 ((string-match "^21." emacs-version)
  (require 'server-20-21)
  (setq server-name (format "server-%d" (emacs-pid)))
  (server-start-20-21))
 ((string-match "^22." emacs-version)
  (require 'server)
  (setq server-name (format "server-%d" (emacs-pid)))
  (server-start))
 (t
  (require 'server)
  (setq server-name (format "server-%d" (emacs-pid)))
  (server-start)))

(require 'workstation)

