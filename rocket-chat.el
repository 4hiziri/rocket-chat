;;; package --- summary
;;; Commentary:
;;; Code:

(load "request.el")
; (require 'request)
(require 'json)
(require 'cl-lib)

(setq url "https://rc.net.itc.nagoya-u.ac.jp")

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

(defun get-json (url header arg-json-alist)
  (let ((ret nil))
    (request url
	     :params arg-json-alist
	     :parser 'json-read
	     :headers header
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

;; :TODO def user struct and return list of struct-user
(defun users-list (url auth-token)
  (let ((ret nil))
    (request (concat url "/api/v1/users.list")
	     :parser 'json-read
	     :headers (auth-headers auth-token)
	     :success (exec-form (setq ret data))
	     :sync t)
    ret))

;; :TODO optional secretURL
(defun users-register (url reg-info)
  (flet ((alist (reg-info)
		(list (cons :email (reg-info-email reg-info))
		      (cons :pass (reg-info-password reg-info))
		      (cons :name (reg-info-name reg-info)))))
    (let ((ret (post-json (concat url "/api/v1/users.register")
			  nil
			  (alist reg-info))))
      ret)))

(defun users-reset-avatar (url auth-token user-id &optional userid-p)
  (assoc-val 'success (post-json (concat url "/api/v1/users.resetAvatar")
				 (auth-headers auth-token)
				 (if userid-p
				     `(("userId" . ,user-id))
				   `(("username" . ,user-id))))))

;; :TODO picture
;; (defun users-set-avatar)

;; :TODO test
(defun users-update (url auth-token reg-info)
  (post-json (concat url "/api/v1/users.update")
	     (auth-headers auth-token)
	     (reg-info-to-alist reg-info)))


;;; channel

;; :TODO optional activity UsersOnly
(defun channels-add-all (url auth-token roomid &optional active-only-p)
  (let ((alist (list (cons :roomID roomid)))
	(ret nil))
    (if active-only-p
	(push (cons :activeUsersOnly "true") alist))
    (post-json (concat url "/api/v1/channels.addAll")
		  (auth-headers auth-token)
		  alist)))

;; :TODO test
(defun channels-add-moderator (url auth-token roomid userid)  
  (let ((ret (post-json (concat url "/api/v1/channels.addModerator")
		(auth-headers auth-token)
		(list (cons :roomId roomid)
		      (cons :userId userid)))))
    (assoc-val 'success ret)))

;; :TODO test
(defun channels-add-owner (url auth-token roomid userid)
  (let ((ret (post-json (cancat url "/api/v1/channels.addOwner")
			(auth-headers auth-token)
			(list (cons :roomId roomid)
			      (cons :userId userid)))))
    (assoc-val 'success ret)))

(defun channels-archive (url auth-token roomid)
  (let ((ret (post-json (concat url "/api/v1/channels.archive")
			(auth-headers auth-token)
			(list (cons :roomId roomid)))))
    (assoc-val 'success ret)))

;; :TODO time-converter
;; (defun channels-clean-history (url auth-token roomid start end &optional inclusive))

(defun channels-close (url auth-token roomid)
  (let ((ret (post-json (concat url "/api/v1/channels.close")
			(auth-headers auth-token)
			(list (cons :roomId roomid)))))
    (assoc-val 'success ret)))

;; :TODO add members
;; :TODO create channel struct
(defun channels-create (url auth-token room-name &optional members)
  (let ((ret (post-json (concat url "/api/v1/channels.create")
			(auth-headers auth-token)
			(list (cons :name room-name)))))
    ret))

(defun channels-get-integrations (url auth-token roomid)
  (let ((ret (get-json (concat url "/api/v1/channels.getIntegrations")
		       (auth-headers auth-token)
		       (list (cons "roomId" roomid)))))
    ret))

;; :TODO make struct
;; (defun channels-history (url auth-toke ))

;; :TODO channel-struct
(defun channels-info (url auth-token roomid &optional roomid-p)
  (let ((ret (get-json (concat url "/api/v1/channels.info")
		       (auth-headers auth-token)
		       (list (if roomid-p
				 (cons "roomId" roomid)
			       (cons "roomName" roomid))))))
    ret))

(defun channels-invite (url auth-token roomid userid)
  (let ((ret (post-json (concat url "/api/v1/channels.invite")
			(auth-headers auth-token)
			(list (cons :roomId roomid)
			      (cons :userId userid)))))
    ret))

(defun channels-kick (url auth-token roomid userid)
  (let ((ret (post-json (concat url "/api/v1/channels.kick")
			(auth-headers auth-token)
			(list (cons :roomId roomid)
			      (cons :userId userid)))))
    ret))

(defun channels-leave (url auth-token roomid)
  (let ((ret (post-json (concat url "/api/v1/channels.leave")
			(auth-headers auth-token)
			(list (cons :roomId roomid)))))
    ret))

(defun channels-list-joined (url auth-token)
  (let ((ret (get-json (concat url "/api/v1/channels.list.joined")
		       (auth-headers auth-token)
		       nil)))
    ret))

(defun channels-list (url auth-token)
  (let ((ret (get-json (concat url "/api/v1/channels.list")
		       (auth-headers auth-token)
		       nil)))
    ret))

(defun channels-open (url auth-token roomid)
  (let ((ret (post-json (concat url "/api/v1/channels.open")
			(auth-headers auth-token)
			(list (cons :roomId roomid)))))
    ret))

(defun channels-remove-moderator (url auth-token roomid userid)
  (let ((ret (post-json (concat url "/api/v1/channels.removeModerator")
			(auth-headers auth-token)
			(list (cons :roomId roomid)
			      (cons :userId userid)))))
    ret))

(defun channels-remove-owner (url auth-token roomid userid)
  (let ((ret (post-json (concat url "/api/v1/channels.removeOwner")
			(auth-headers auth-token)
			(list (cons :roomId roomid)
			      (cons :userId userid)))))
    ret))

(defun channels-rename (url auth-token roomid roomname)
  (let ((ret (post-json (concat url "/api/v1/channels.rename")
			(auth-headers auth-token)
			(list (cons :roomId roomid)
			      (cons :name roomname)))))
    ret))

(defun channels-set-description (url auth-token roomid description)
  (let ((ret (post-json (concat url "/api/v1/channels.setDescription")
			(auth-headers auth-token)
			(list (cons :roomId roomid)
			      (cons :description description)))))
    ret))

(defun channels-set-join-code (url auth-token roomid join-code)
  (let ((ret (post-json (concat url "/api/v1/channels.setJoinCode")
			(auth-headers auth-token)
			(list (cons :roomId roomid)
			      (cons :joinCode join-code)))))
    ret))

(defun channels-set-purpose (url auth-token roomid purpose)
  (let ((ret (post-json (concat url "/api/v1/channels.setPurpose")
			(auth-headers auth-token)
			(list (cons :roomId roomid)
			      (cons :purpose purpose)))))
    ret))

(defun channels-set-read-only (url auth-token roomid readonly)
  (let ((ret (post-json (concat url "/api/v1/channels.setPurpose")
			(auth-headers auth-token)
			(list (cons :roomId roomid)
			      (cons :readOnly (if readonly "true" "false"))))))
    ret))

(defun channels-set-topic (url auth-token roomid topic)
  (let ((ret (post-json (concat url "/api/v1/channels.setTopic")
			(auth-headers auth-token)
			(list (cons :roomId roomid)
			      (cons :topic topic)))))
    ret))

;; type-> c or p
(defun channels-set-type (url auth-token roomid type)
  (let ((ret (post-json (concat url "/api/v1/channels.setType")
			(auth-headers auth-token)
			(list (cons :roomId roomid)
			      (cons :type type)))))
    ret))

(defun channels-unarchive (url auth-token roomid)
  (let ((ret (post-json (concat url "/api/v1/channels.unarchive")
			(auth-headers auth-token)
			(list (cons :roomId roomid)))))
    ret))

(provide 'rocket-chat)
;;; rocket-chat ends here
