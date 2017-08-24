;;; rocket-chat --- Emacs client for Rocket.chat

;; Copyright 2017 Takagi Seiya
;;
;; Author: meirvg@gmail.com
;; Version: 0.0.1
;; Keywords: Rocket.chat
;; X-URL: not distributed yet

;;; Commentary:
;;; Code:

(require 'request)
(require 'json)
(require 'cl-lib)
(require 'promise)
(require 'async-await)
(require 'time)

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

;;; application

(defgroup rocket-chat nil
  "Major mode for chatting on rocket-chat"
  :prefix "rc-"
  :group 'applications
  :link '(url-link :tag "Github" "https://github.com/4hiziri/rocket-chat"))

(defcustom rocket-chat-mode-hook nil
  "Hook run after `rocket-chat-mode` setup is finished."
  :group 'rocket-chat
  :type 'hook)

(defcustom rc-default-server nil
  "Default accessing Server's url."
  :type 'sexp
  :group 'rocket-chat)

(defcustom rc-default-username nil
  "Default user name."
  :type 'sexp
  :group 'rocket-chat)

(defcustom rc-reading-post-num 100
  "Num of fetching posts."
  :type 'sexp
  :group 'rocket-chat)

(defvar rocket-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'rc-post-line)
    (define-key map "\C-c\C-n" 'rc-update-channel)
    (define-key map "\C-c\C-l" 'rc-show-channels)
    map)
  "Keymap for rocket-chat-mode.")

;; :TODO research
;; unnecessary?
(defvar rocket-chat-mode-abbrev-table nil)
;; :TODO syntax-table?
;; unnecessary?
(define-abbrev-table 'rocket-chat-mode-abbrev-table ())

(defstruct rc-session
  "Information of login session."
  server
  channel
  username
  token)

(defvar rc-current-session nil
  "Information of current login session.")
(make-variable-buffer-local 'rc-current-session)

(defvar rc-buffer-name
  "rc-test")

(defvar rc-buffer
  (get-buffer-create rc-buffer-name)
  "Buffer.")

(defvar rc-insert-marker nil
  "Marker of insert position.")
(make-variable-buffer-local 'rc-insert-marker)

(defvar rc-input-marker nil
  "Inserted position.")
(make-variable-buffer-local 'rc-input-marker)

;; faces
(defgroup rc-faces nil
  "Faces for Rocket.chat-mode"
  :group 'rocket-chat)

(defface rc-username-face '((t (:foreground "Red")))
  "Face for username."
  :group 'rc-faces)

(defface rc-participant-face '((t (:foreground "#3d603d")))
  "Face for participant of channel."
  :group 'rc-faces)

(defface rc-prompt-face '((t (:background "Green")))
  "Face for prompt."
  :group 'rc-faces)

(defun rc-get-server (&optional url)
  "Return a Rocket.chat URL.

This tries to find none nil value.

- URL ARGS
- The `rc-server` option
- The `rc-default-server` var"
  (or url
      rc-default-server))

(defun rc-get-username (&optional username)
  "Return a Rocket.chat username.

This tries to find none nil value.

- USERNAME ARGS
- The `rc-username` option
- The `rc-default-username` var"
  (or username
      rc-default-username))

(defun rc-get-input-args ()
  "Get from minibuffer input.

This prefer default value than input."
  (let ((server (read-from-minibuffer "URL: " (rc-get-server)))
	(user (read-from-minibuffer "USER: " (rc-get-username)))
	(pass (read-passwd "PASSWORD: ")));; :TODO history
    (list :server server :username user :password pass)))

(defun rc-login (server username password)
  "Login to SERVER as USERNAME.

SERVER - this will accessed by user
USERNAME - login user name
PASSWORD - login password"
  (let ((token (login server username password)))
    (when token
      (make-rc-session :server server :username username :token token))))

(defun* rocket-chat (&key server username password)
  "This allow you to login to URL."
  (interactive (rc-get-input-args))
  (setf rc-buffer (get-buffer-create rc-buffer-name))
  (pop-to-buffer rc-buffer)
  (with-current-buffer rc-buffer
    (rocket-chat-mode)
    (add-hook 'pre-command-hook 'rc-set-marker-at-prompt)
    (setq rc-current-session
	  (rc-login server username password))
    (cl-flet ((success ()
		       (goto-char (point-min))		       
		       (message "Successed!")			 
		       (rc-show-channels))
	      (fail ()
		    (setq rc-current-session nil) ;; :TODO clear state function needed
		    (kill-buffer rc-buffer)
		    (message "Failed..")))
      (if (rc-session-token rc-current-session)
	  (success)
	(fail)))))

(defun rc-logout ()
  "Logout from server.

If this success, logout message is printed on echo-area.
rc-current-session - Infomation of logined server"
  (interactive)
  (let ((msg (logout (rc-session-server rc-current-session)
		     (rc-session-token rc-current-session))))
    (when msg
      (setq rc-current-session nil))
    (insert msg)))

(defun rc-set-marker-at-prompt ()
  "If user is not at prompt when user inputs, set marker to prompt."
  (when (and rc-input-marker
	     (< (point) rc-input-marker)
	     (eq 'self-insert-command this-command))
    (deactivate-mark)
    (push-mark)
    (goto-char (point-max))))

(defun rc-show-channels ()
  "Make buffer and write channel-list to that buffer.

Channel-list is text-button.
rc-current-session - Infomation of logined server"
  (interactive)
  (with-current-buffer rc-buffer
    (setf rc-insert-marker nil)
    (save-excursion
      (let ((chs (channels-list (rc-session-server rc-current-session)
				(rc-session-token rc-current-session)))
	    (buffer-read-only nil)
	    (inhibit-read-only t))
	(remove-text-properties (point-min) (point-max) '(read-only t))
	(erase-buffer)
	(mapcan (lambda (x)
		  (insert-text-button (channel-name x)
				      'action (lambda (but)
						(rc-show-channel-contents
						 (button-get but 'channel)))
				      'follow-link t		
				      'help-echo "Join Channel and display."
				      'channel x)
		  (insert "\n"))
		chs)))
    (setf buffer-read-only t)))

(defun rc-insert-text (text)
  "This insert TEXT to buffer as read-only.

TEXT - text that is inserted"
  (save-excursion
    (let ((length (length text))
	  (buffer-read-only nil))
      (goto-char rc-insert-marker)    
      (insert text)
      (add-text-properties rc-insert-marker (point) '(front-sticky t rear-nonsticky t read-only t))
      (set-marker rc-insert-marker (point) rc-buffer))))

(defun rc-user-p (name session)
  "Predicate whether NAME is username in SESSION."
  (string= name (rc-session-username session)))

(defun rc-local-time-to-rc-time (time)
  "This return rc-format TIME."
  (let ((utc-time (decode-time (time-subtract (apply #'encode-time time)
					      (car (current-time-zone))))))
    (format "%d-%02d-%02dT%02d:%02d:%02d.000Z"
	    (nth 5 utc-time)
	    (nth 4 utc-time)
	    (nth 3 utc-time)
	    (nth 2 utc-time)
	    (nth 1 utc-time)
	    (nth 0 utc-time))))

(defun rc-time-to-local-time (time-string)
  "This return local-time converted from TIME-STRING.

TIME-STRING - time represented by rc."
  (cl-flet ((time-parse (time-string)
			(let* ((divided-point (string-match "T" time-string)))
			  (concat (subseq time-string 0 divided-point)
				  " "
				  (subseq time-string (1+ divided-point) (length time-string))))))
    (decode-time (time-add (apply #'encode-time (parse-time-string (time-parse time-string)))
			   (car (current-time-zone))))))

(defun rc-format-time (time)
  "This return format-string of TIME."
  (format "%02d/%02d %02d:%02d" (nth 4 time) (nth 3 time) (nth 2 time) (nth 1 time)))

(rc-format-time (decode-time (current-time)))

;; :TODO show time of the post.
(defun rc-set-msg-to-buffer (msg)
  "Write MSG to buffer.

This writes chat-message to buffer.
MSG - Rocket.chat's msg struct.
`rc-buffer' - buffer for use by this."
  (with-current-buffer rc-buffer
    (save-excursion
      (goto-char rc-insert-marker)
      (let ((name (assoc-val 'username (message-user-info msg)))
	    (time-str (concat "(" (rc-format-time (rc-time-to-local-time (message-time-stamp msg))) ")"))
	    (old-point (point)))
	(rc-insert-text (concat name
				time-str
				"> "
				(message-message msg)
				"\n"))
	(put-text-property old-point
			   (+ old-point (length name))
			   'face
			   (if (rc-user-p name rc-current-session)
			       'rc-username-face
			     'rc-participant-face))
	(put-text-property old-point
			   rc-insert-marker
			   'message-info
			   msg)))))

(defun rc-insert-prompt (&optional prompt)
  "Insert input PROMPT to buffer."
  (with-current-buffer rc-buffer
    (let ((prompt (or prompt "<"))
	  (old-point nil))
      (save-excursion
	(goto-char (point-max))
	(forward-line 0)
	(setf old-point (point))
	(insert (concat prompt " "))
	(set-marker rc-input-marker (point))
	(add-text-properties old-point (point) '(front-sticky t
							      rear-nonsticky t
							      read-only t
							      face rc-prompt-face))))))

(defun rc-show-channel-contents (channel)
  "Write chats in CHANNEL to buffer.

CHANNEL - chat room
`rc-current-session' - Infomation of logined server"
  (with-current-buffer rc-buffer
    (let* ((msgs (channels-history (rc-session-server rc-current-session)
				   (rc-session-token rc-current-session)
				   (channel-id channel)
				   :count rc-reading-post-num))
	   (inhibit-read-only t))
      (setf buffer-read-only nil)
      (setf rc-insert-marker (make-marker))
      (setf rc-input-marker (make-marker))
      (set-marker rc-insert-marker (point-min) rc-buffer)
      (when msgs
	(erase-buffer)
	(setf (rc-session-channel rc-current-session) channel)
	(mapcar #'rc-set-msg-to-buffer (reverse msgs))
	(rc-insert-prompt)
	(goto-char rc-input-marker)
	(rc-async-update-channel rc-current-session)))))

(defun rc-update-channel ()
  "Update displayed channel contents.

`rc-current-session' - Infomation of logined server"
  (interactive)
  (with-current-buffer rc-buffer
    (let* ((last (rc-last-updated-time rc-current-session))
	   (last-msg (get-text-property (1- rc-insert-marker) 'message-info))
	   (msgs (channels-history (rc-session-server rc-current-session)
				    (rc-session-token rc-current-session)
				    (channel-id (rc-session-channel rc-current-session))
				    :oldest (rc-local-time-to-rc-time last)
				    :count rc-reading-post-num))
	   (channel (channels-info (rc-session-server rc-current-session)
				   (rc-session-token rc-current-session)
				   (channel-id (rc-session-channel rc-current-session))
				   t))
	   (inhibit-read-only t))
      (when (and msgs (> (length msgs) 1))
	(setf (rc-session-channel rc-current-session) channel) ;; update-channel-info	
	(mapcar #'rc-set-msg-to-buffer (cdr (reverse msgs)))))))

(defun rc-latest-updated-time (session)
  "This return time of CHANNEL's last post on SESSION."
  ;; :FIXME maybe network process blocking IO.
  (let ((channel (channels-info (rc-session-server session)
				(rc-session-token session)
				(channel-id (rc-session-channel session))
				t)))
    (rc-time-to-local-time (channel-lm channel))))

(defun rc-last-updated-time (session)
  "This return latest time of updated posts in SESSION."
  (rc-time-to-local-time
   (channel-lm (rc-session-channel session))))

(defun rc-need-update-p (session)
  "This return whether need to update or not in SESSION."  
  (let ((latest-msg (rc-latest-updated-time session))
	(last-time (rc-last-updated-time session)))
    (time-less-p last-time latest-msg)))

;; Setting for async-update buffer.
(setq lexical-binding t)
(setf interval 2)
(defun rc-async-update-channel (session)
  "This update posts in channel of SESSION."
  (async-start ;; :FIXME I think this is not efficient way.
   `(lambda ()
      (sleep-for ,interval))
   (lambda (result)     
     (with-local-quit       
       (when (and (buffer-live-p rc-buffer) (buffer-local-value 'rc-insert-marker rc-buffer))
	 (when (rc-need-update-p session)
	   (rc-update-channel)
	   (setf session (buffer-local-value 'rc-current-session (get-buffer rc-buffer-name))))
	 (rc-async-update-channel session))))))

(defun rc-user-input ()
  "This gets user input form input-area.

`rc-input-marker' - beginning of input-area"
  (buffer-substring-no-properties
   rc-input-marker
   (point-max)))

(defun rc-post (text session)
  "This sends TEXT channel on server.

TEXT - Posted text
SESSION - Infomation of logined server"
  (chat-post-msg (rc-session-server session)
		 (rc-session-token session)
		 (rc-session-channel session)
		 text))

(defun rc-post-line ()
  "This posts line at input-area to connected server."
  (interactive)
  (let ((input (rc-user-input)))
    (if (not (string= input ""))
	(progn	  
	  (rc-post (encode-coding-string input 'utf-8) rc-current-session)
	  (delete-region rc-input-marker (point-max))
	  (rc-update-channel))
      (message "Ignoring blank line."))))

(defun rocket-chat-mode ()
  "Major mode for Rocket.chat."
  (kill-all-local-variables)
  (use-local-map rocket-chat-mode-map)
  (setq mode-name "Rocket.chat"
	major-mode 'rocket-chat-mode
	local-addrev-table rocket-chat-mode-abbrev-table)
  ;;(set-syntax-table syntax-table)
  (run-hooks 'rocket-chat-mode-hook))

(provide 'rocket-chat)
;;; rocket-chat ends here
