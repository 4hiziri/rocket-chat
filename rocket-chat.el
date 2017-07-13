;;; package --- summary
;;; Commentary:
;;; Code:

(load "request.el")
; (require 'request)
(require 'json)
(require 'cl-lib)

(defstruct auth-token
  (user-id nil)
  (token nil))

(defvar test-url "https://rc.net.itc.nagoya-u.ac.jp")
(defvar auth-info nil)

(defun assoc-val (key alist)
  "This return val of KEY from ALIST."
  (cdr (assoc key alist)))

(defun url-concat (&rest seq)
  (apply #'concat seq))

(defun auth-headers (auth-token)
  "This return token-header from AUTH-TOKEN."
  `(("X-Auth-Token" . ,(auth-token-token auth-token))
    ("X-User-Id" . ,(auth-token-user-id auth-token))))

(defmacro exec-form (&rest form)
  "This expand &BODY in func-form."
  `(function*
    (lambda (&key data &allow-other-keys)
      ,@form)))

(defun info (url)
  "URL - server url.
This function return server-info"
  (let ((ret nil))
    (request (url-concat url "/api/v1/info")
	     :parser 'json-read
	     :success (exec-form (setq ret data))
	     :sync t)
    ret))

(defun login (url username password)
  "URL - server url.
USERNAME - registered username
PASSWORD - user's password"
  (let ((ret nil))
    (request (url-concat url "/api/v1/login")
	     :type "POST"
	     :data `(("username" . ,username)
		     ("password" . ,password))
	     :parser 'json-read
	     :success (exec-form (setq ret data))
	     :sync t)
    (if (string= (assoc-val 'status ret) "success")
	(let ((info (assoc-val 'data ret)))
	  (make-auth-token :user-id (assoc-val 'userId info)
			   :token (assoc-val 'authToken info)))
      nil)))

(defun logout (url auth-token)
  "This logout from URL, need AUTH-TOKEN."
  (let ((ret nil))
    (request (url-concat url "/api/v1/logout")
	     :parser 'json-read
	     :headers (auth-headers auth-token)
	     :success (exec-form (setq ret data))
	     :sync t)
    (if (string= (assoc-val 'status ret) "success")
	(let ((data (assoc-val 'data ret)))
	  (assoc-val 'message data))
      nil)))

(setf test (login test-url "tkgsy" "se1yao4io"))

(defun me (url auth-token)  
  (let ((ret nil))
    (request (url-concat url "/api/v1/me")
	     :parser 'json-read
	     :headers (auth-headers auth-token)
	     :success (exec-form (setq ret data))
	     :sync t)
    ret))

(me test-url test)


(provide 'rocket-chat)
;;; rocket-chat ends here
