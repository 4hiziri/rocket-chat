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

(defstruct reg-info
  email
  name ; display name
  password
  username
  (active t)
  (roles (list "user"))
  (joinDefaultChannels t)
  (requirePasswordChange nil)
  (sendWelcomeEmail nil)
  (verified nil)
  (customFields "undefined") ;; {field : value}
  )

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

(defun bool-to-str (bool)
  (if bool
      "true"
    "false"))

(defun post-json (url header arg-json-alist)
  (let ((ret nil))
    (request url
	     :type "POST"
	     :data (json-encode-alist arg-json-alist)
	     :parser 'json-read
	     :headers (cons '("Content-type" . "application/json") header)
	     :success (exec-form (setq ret data))
	     :sync t)
    ret))

(defun reg-info-to-alist (reg-info)
  (let ((ret nil))
    (push `(:email ,(reg-info-email reg-info)) ret)
    (push `(:name ,(reg-info-name reg-info)) ret)
    (push `(:password ,(reg-info-password reg-info)) ret)
    (push `(:username ,(reg-info-username reg-info)) ret)
    (push `(:active ,(bool-to-str (reg-info-active reg-info))) ret)
    (push `(:roles ,(list-to-str (reg-info-roles reg-info))) ret)
    (push `(:joinDefaultChannels ,(bool-to-str (reg-info-joinDefaultChannels reg-info))) ret)
    (push `(:requirePasswordChange
	    ,(bool-to-str (reg-info-requirePasswordChange reg-info)))
	  ret)
    (push `(:sendWelcomeEmail ,(bool-to-str (reg-info-sendWelcomeEmail reg-info))) ret)
    (push `(:verified ,(bool-to-str (reg-info-verified reg-info))) ret)
    (push `(:customFields ,(reg-info-customFields reg-info)) ret)
    ret))

;; api
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

(defun me (url auth-token)
  "Get user-info from URL, need AUTH-TOKEN."
  (let ((ret nil))
    (request (url-concat url "/api/v1/me")
	     :parser 'json-read
	     :headers (auth-headers auth-token)
	     :success (exec-form (setq ret data))
	     :sync t)
    ret))

(me test-url test)

;; need test
;; launch server
(defun users-create (url auth-token register-info)
  "Register to URL with AUTH-TOKEN, info is in REGISTER-INFO."
  (let ((ret nil))
    (request (concat url "/api/v1/users.create")
	   :type "POST"
	   :parser 'json-read
	   :headers (cons '("Content-type" . "application/json")
			  (auth-headers auth-token))
	   :data (json-encode-alist (reg-info-to-alist register-info)) 
	   :success (exec-form (setq ret data))
	   :sync t)
    ret))

(defun users-create-token (url auth-token user-id &optional userid-p)
  (let ((ret (post-json (concat url "/api/v1/users.createToken")
			(auth-headers auth-token)
			(if userid-p
			    `(("userId" . ,user-id))
			  `(("username" . ,user-id))))))
    (if (assoc-val 'success ret)	
	(let ((data (assoc-val 'data ret)))
	  (make-auth-token :user-id (assoc-val 'userId data) :token (assoc-val 'authToken data))))))

(defun users-delete (url auth-token)
  (let ((ret (post-json (concat url "/api/v1/users.delete")
			(auth-headers auth-token)
			`(("userId" . ,(auth-token-user-id auth-token))))))
    ret))

;; :TODO parser
(defun users-get-avatar (url user-id &optional userid-p)
  (let ((ret (request (concat url "/api/v1/users.getAvatar")
		      :params (if userid-p
				  `(("userId" . ,user-id))
				`(("username" . ,user-id)))
		      :success (exec-form)
		      :sync t)))
    ret))

(defun users-get-presence (url auth-token user-id &optional userid-p)
  (let ((ret nil))
    (request (concat url "/api/v1/users.getPresence")
	     :params (if userid-p
			 `(("userId" . ,user-id))
		       `(("username" . ,user-id)))
	     :parser 'json-read
	     :headers (auth-headers auth-token)
	     :success (exec-form (setq ret data))
	     :sync t)
    ret))

(defun users-info (url auth-token user-id &optional userid-p)
  (let ((ret nil))
    (request (concat url "/api/v1/users.info")
	     :params (if userid-p
			 `(("userId" . ,user-id))
		       `(("username" . ,user-id)))
	     :parser 'json-read
	     :headers (auth-headers auth-token)
	     :success (exec-form (setq ret data))
	     :sync t)
    ret))

(provide 'rocket-chat)
;;; rocket-chat ends here
