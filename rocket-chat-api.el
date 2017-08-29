;;; rocket-chat-api.el --- Emacs API interface for Rocket.chat

;; Copyright 2017 Takagi Seiya
;;
;; Author: meirvg@gmail.com
;; Version: 0.0.1
;; Keywords: Rocket.chat
;; X-URL: not distributed yet

;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'cl))
(require 'request)
(require 'json)
(require 'cl-lib)

;;; structs
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

(defstruct channel
  id
  time-stamp
  _t
  name
  usernames
  msgs
  default
  update-time
  lm)

(defstruct message
  id
  room-id
  message
  time-stamp
  user-info
  updated-at
  ;; editedAt
  ;; editedBy
  ;; urls
  ;; attachments
  ;; alias
  ;; avatar
  ;; groupable
  ;; parseUrls
  )

(defun json-to-msg (json)
  "This coverts JSON to struct message.

JSON - message-data formed json."
  (make-message :id (assoc-val '_id json)
		:room-id (assoc-val 'rid json)
		:message (decode-coding-string (assoc-val 'msg json) 'utf-8)
		:time-stamp (assoc-val 'ts json)
		:user-info (assoc-val 'u json) ;; :TODO defstruct user-info
		:updated-at (assoc-val'_updatedAt json)))

;;; utils
(defun assoc-val (key alist)
  "This return val of KEY from ALIST."
  (rest (assoc key alist)))

(defun json-channel (json)
  (make-channel :id (assoc-val '_id json)
		:time-stamp (assoc-val 'ts json) ;; :TODO convert time
		:t (assoc-val 't json)
		:name (assoc-val 'name json)
		:usernames (assoc-val 'usernames json)
		:msgs (assoc-val 'msgs json)
		:default (assoc-val 'default json)
		:update-time (assoc-val '_updatedAt json)
		:lm (assoc-val 'lm json)))

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

;;; api
(defun info (url)
  "URL - server url.
This function return server-info"
  (let ((ret (get-json (url-concat url "/api/v1/info") nil nil)))
    (if (assoc-val 'success ret)
	(list (assoc 'info ret) (assoc 'commit ret)))))

(defun login (url username password)
  "URL - server url.
USERNAME - registered username
PASSWORD - user's password"
  (let ((ret (post-json (url-concat url "/api/v1/login")
			nil
			`(("username" . ,username)
			  ("password" . ,password)))))
    (when (and ret (string= (assoc-val 'status ret) "success"))
      (let ((info (assoc-val 'data ret)))
	(make-auth-token :user-id (assoc-val 'userId info)
			 :token (assoc-val 'authToken info))))))

(defun logout (url auth-token)
  "This logout from URL, need AUTH-TOKEN."
  (let ((ret (get-json (url-concat url "/api/v1/logout")
		       (auth-headers auth-token)
		       nil)))
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
  (cl-flet ((alist (reg-info)
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


;; channel

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

(defun* channels-history (url auth-token roomid &key latest oldest inclusive count unreads)
  "This return `message' struct.

URL - server url.
AUTH-TOKEN - token.
ROOMID - channel's room id.
LATEST - The end of time range of messages.
OLDEST - The start of time range of messages.
INCLUSIVE - If set t, fetch all history.
COUNT - The amount of messages.
UNREADS - Whether the amount of unreads should be included."
  (let ((ret (get-json (concat url "/api/v1/channels.history")
		       (auth-headers auth-token)
		       (remove-if #'null
				  (list (cons "roomId" roomid)
					(when latest (cons "latest" latest))
					(when oldest (cons "oldest" oldest))
					;; :TODO need convert t,nil to true,false
					(when inclusive (cons "inclusive" inclusive)) 
					(when count (cons "count" count))
					(when unreads (cons "unreads" unreads)))))))    
    (when (assoc-val 'success ret)
      (map 'list #'json-to-msg (assoc-val 'messages ret)))))

(defun channels-info (url auth-token room-name &optional roomid-p)
  (let ((ret (get-json (concat url "/api/v1/channels.info")
		       (auth-headers auth-token)
		       (list (if roomid-p
				 (cons "roomId" room-name)
			       (cons "roomName" room-name))))))
    (when (assoc-val 'success ret)
      (json-channel (assoc-val 'channel ret)))))

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
    (when (assoc-val 'success ret)
      (map 'list #'json-channel (assoc-val 'channels ret)))))

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

;; group

(defun groups-add-all (url auth-token roomid &optional active-only-p)
  (let ((alist (list (cons :roomID roomid)))
	(ret nil))
    (if active-only-p
	(push (cons :activeUsersOnly "true") alist))
    (post-json (concat url "/api/v1/groups.addAll")
	       (auth-headers auth-token)
	       alist)))

;; :TODO test
(defun groups-add-moderator (url auth-token roomid userid)  
  (let ((ret (post-json (concat url "/api/v1/groups.addModerator")
		(auth-headers auth-token)
		(list (cons :roomId roomid)
		      (cons :userId userid)))))
    (assoc-val 'success ret)))

;; :TODO test
(defun groups-add-owner (url auth-token roomid userid)
  (let ((ret (post-json (cancat url "/api/v1/groups.addOwner")
			(auth-headers auth-token)
			(list (cons :roomId roomid)
			      (cons :userId userid)))))
    (assoc-val 'success ret)))

(defun groups-archive (url auth-token roomid)
  (let ((ret (post-json (concat url "/api/v1/groups.archive")
			(auth-headers auth-token)
			(list (cons :roomId roomid)))))
    (assoc-val 'success ret)))

;; :TODO time-converter
;; (defun groups-clean-history (url auth-token roomid start end &optional inclusive))

(defun groups-close (url auth-token roomid)
  (let ((ret (post-json (concat url "/api/v1/groups.close")
			(auth-headers auth-token)
			(list (cons :roomId roomid)))))
    (assoc-val 'success ret)))

;; :TODO add members
;; :TODO create channel struct
(defun groups-create (url auth-token room-name &optional members)
  (let ((ret (post-json (concat url "/api/v1/groups.create")
			(auth-headers auth-token)
			(list (cons :name room-name)))))
    ret))

(defun groups-get-integrations (url auth-token roomid)
  (let ((ret (get-json (concat url "/api/v1/groups.getIntegrations")
		       (auth-headers auth-token)
		       (list (cons "roomId" roomid)))))
    ret))

;; :TODO make struct
;; (defun groups-history (url auth-toke ))

;; :TODO channel-struct
(defun groups-info (url auth-token roomid &optional roomid-p)
  (let ((ret (get-json (concat url "/api/v1/groups.info")
		       (auth-headers auth-token)
		       (list (if roomid-p
				 (cons "roomId" roomid)
			       (cons "roomName" roomid))))))
    ret))

(defun groups-invite (url auth-token roomid userid)
  (let ((ret (post-json (concat url "/api/v1/groups.invite")
			(auth-headers auth-token)
			(list (cons :roomId roomid)
			      (cons :userId userid)))))
    ret))

(defun groups-kick (url auth-token roomid userid)
  (let ((ret (post-json (concat url "/api/v1/groups.kick")
			(auth-headers auth-token)
			(list (cons :roomId roomid)
			      (cons :userId userid)))))
    ret))

(defun groups-leave (url auth-token roomid)
  (let ((ret (post-json (concat url "/api/v1/groups.leave")
			(auth-headers auth-token)
			(list (cons :roomId roomid)))))
    ret))

(defun groups-list (url auth-token)
  (let ((ret (get-json (concat url "/api/v1/groups.list")
		       (auth-headers auth-token)
		       nil)))
    ret))

(defun groups-open (url auth-token roomid)
  (let ((ret (post-json (concat url "/api/v1/groups.open")
			(auth-headers auth-token)
			(list (cons :roomId roomid)))))
    ret))

(defun groups-remove-moderator (url auth-token roomid userid)
  (let ((ret (post-json (concat url "/api/v1/groups.removeModerator")
			(auth-headers auth-token)
			(list (cons :roomId roomid)
			      (cons :userId userid)))))
    ret))

(defun groups-remove-owner (url auth-token roomid userid)
  (let ((ret (post-json (concat url "/api/v1/groups.removeOwner")
			(auth-headers auth-token)
			(list (cons :roomId roomid)
			      (cons :userId userid)))))
    ret))

(defun groups-rename (url auth-token roomid roomname)
  (let ((ret (post-json (concat url "/api/v1/groups.rename")
			(auth-headers auth-token)
			(list (cons :roomId roomid)
			      (cons :name roomname)))))
    ret))

(defun groups-set-description (url auth-token roomid description)
  (let ((ret (post-json (concat url "/api/v1/groups.setDescription")
			(auth-headers auth-token)
			(list (cons :roomId roomid)
			      (cons :description description)))))
    ret))

(defun groups-set-purpose (url auth-token roomid purpose)
  (let ((ret (post-json (concat url "/api/v1/groups.setPurpose")
			(auth-headers auth-token)
			(list (cons :roomId roomid)
			      (cons :purpose purpose)))))
    ret))

(defun groups-set-read-only (url auth-token roomid readonly)
  (let ((ret (post-json (concat url "/api/v1/groups.setPurpose")
			(auth-headers auth-token)
			(list (cons :roomId roomid)
			      (cons :readOnly (if readonly "true" "false"))))))
    ret))

(defun groups-set-topic (url auth-token roomid topic)
  (let ((ret (post-json (concat url "/api/v1/groups.setTopic")
			(auth-headers auth-token)
			(list (cons :roomId roomid)
			      (cons :topic topic)))))
    ret))

;; type-> c or p
(defun groups-set-type (url auth-token roomid type)
  (let ((ret (post-json (concat url "/api/v1/groups.setType")
			(auth-headers auth-token)
			(list (cons :roomId roomid)
			      (cons :type type)))))
    ret))

(defun groups-unarchive (url auth-token roomid)
  (let ((ret (post-json (concat url "/api/v1/groups.unarchive")
			(auth-headers auth-token)
			(list (cons :roomId roomid)))))
    ret))

;; Im

(defun im-close (url auth-token roomid)
  (let ((ret (post-json (concat url "/api/v1/im.close")
			(auth-headers auth-token)
			(list (cons :roomId roomid)))))
    (assoc-val 'success ret)))

;; (defun im-history (url auth-toke ))

(defun im-list-everyone (url auth-token)
  (let ((ret (get-json (concat url "/api/v1/im.list.everyone")
		       (auth-headers auth-token)
		       nil)))
    ret))

(defun im-list (url auth-token)
  (let ((ret (get-json (concat url "/api/v1/im.list")
		       (auth-headers auth-token)
		       nil)))
    ret))

(defun im-messages-others (url auth-token roomid)
  (let ((ret (get-json (concat url "/api/v1/im.messages.others")
		       (auth-headers auth-token)
		       (list (cons "roomId" roomid)))))
    ret))

(defun im-open (url auth-token roomid)
  (let ((ret (post-json (concat url "/api/v1/im.open")
			(auth-headers auth-token)
			(list (cons :roomId roomid)))))
    ret))

(defun im-set-topic (url auth-token roomid topic)
  (let ((ret (post-json (concat url "/api/v1/im.setTopic")
			(auth-headers auth-token)
			(list (cons :roomId roomid)
			      (cons :topic topic)))))
    ret))

;; chat
(defun chat-delete (url auth-token roomid msgid &optional as-user)
  (let ((ret (post-json (concat url "/api/v1/chat.delete")
			(auth-headers auth-token)
			(list (cons :roomId roomid)
			      (cons :msgId msgid)
			      (if as-user
				  (cons :asUser "true")
				(cons :asUser "false"))))))
    ret))

;; :TODO def struct
(defun chat-post-msg (url auth-token channel text)
  (let ((ret (post-json (concat url "/api/v1/chat.postMessage")
			(auth-headers auth-token)
			(list (cons :roomId (channel-id channel))
			      (cons :channel (channel-name channel))
			      (cons :text text)))))
    (when (assoc-val 'success ret)
      (assoc-val 'ts ret))))

(defun chat-update (url auth-token roomid msgid text)
  (let ((ret (post-json (concat url "/api/v1/chat.update")
			(auth-headers auth-token)
			(list (cons :roomId roomid)
			      (cons :msgId msgid)
			      (cons :text text)))))
    ret))

;; setting
(defun setting-get (url auth-token id)
  (get-json (concat url "/api/v1/settings/" id)
	    (auth-headers auth-token)
	    nil))

(defun setting-update (url auth-token id value)
  (post-json (concat url "/api/v1/settings/" id)
	     (auth-headers auth-token)
	     (list (cons id value))))

;; integration
;; :TODO def struct
;; (defun integrations-create (url auth-token ))

(defun integrations-list (url auth-token)
  (get-json (concat url "/api/v1/integrations.list")
	    (auth-headers auth-token)
	    nil))

(defun integrations-remove (url auth-token type id)
  (post-json (concat url "/api/v1/integrations.list")
	     (auth-headers auth-token)
	     (list (cons :type type)
		   (cons :integrationId id))))

;; livechat
(defun livechat-list-department (url auth-token)
  (get-json (concat url "/api/v1/livechat/department")
	    (auth-headers auth-token)
	    nil))

;; :TODO research
;; (defun livechat-register-department (url auth-token ))

(defun livechat-sms-incoming (url service)
  (post-json (concat url "/api/v1/livechat/sms-incoming/" service)
	     nil
	     nil))

(defun livechat-users (url auth-token type)
  (get-json (concat url "/api/v1/livechat/users/" type)
	    (auth-headers auth-token)
	    nil))

(provide 'rocket-chat-api)
;;; rocket-chat-api.el ends here
