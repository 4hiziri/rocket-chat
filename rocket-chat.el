;;; rocket-chat.el --- Emacs client for Rocket.chat

;; Copyright 2017 4hiziri
;;
;; Author: meirvg@gmail.com
;; Keywords: Rocket.chat, emacs
;; URL: https://github.com/4hiziri/rocket-chat.git
;; Package-Requires: ((cl-lib "1.0") (promise "20170215.2204") (async-await "20170208.350") (request "20170131.1747"))

;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'cl))
(require 'promise)
(require 'async-await)
(require 'time)

(require 'rocket-chat-api)

(setf lexical-binding t)

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

; :TODO enable mode to be changed
(defvar rocket-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'rc-post-line)
    (define-key map "\C-c\C-n" 'async-rc-update-channel)
    (define-key map "\C-c\C-l" 'rc-show-channels)
    (define-key map "\C-c\C-u" 'rc-show-user-list)
    map)
  "Keymap for rocket-chat-mode.")

;; :FIXME delete this?
(defvar rocket-chat-mode-abbrev-table nil)
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
  "*rocket-chat*")

(defvar rc-buffer
  nil
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

(defface rc-system-face '((t (:foreground "Cyan")))
  "Face for system-message."
  :group 'rc-faces)

(defface rc-user-online-face '((t (:foreground "Green")))'
  "Face for user-status."
  :group 'rc-faces)

(defface rc-user-offline-face '((t (:foreground "Red")))'
  "Face for user-status."
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
  (with-current-buffer rc-buffer
    (rocket-chat-mode)
    (add-hook 'pre-command-hook 'rc-set-marker-at-prompt)
    (setf rc-current-session
	  (rc-login server username password))
    (cl-flet ((success ()
		       (goto-char (point-min))
		       (message "rc: Login Successed")
		       (rc-show-channels))
	      (fail ()
		    (setf rc-current-session nil) ;; :TODO clear state function needed
		    (kill-buffer rc-buffer)
		    (message "rc: Login Failed")))
      (if (rc-session-token rc-current-session)
	  (success)
	  (fail))))
  (pop-to-buffer rc-buffer))

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
      (let ((buffer-read-only nil)
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
		(channels-list (rc-session-server rc-current-session)
			       (rc-session-token rc-current-session)))))
    (setf buffer-read-only t)))

(defun rc-yourself-p (name session)
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

(defun rc-insert (msg &optional proparties)
  "This insert MSG with FACE."
  (with-current-buffer rc-buffer
    (save-excursion
      (goto-char rc-insert-marker)
      (let ((read-only nil)
	    (inhibit-read-only t))
	(insert msg)
	(add-text-properties rc-insert-marker (point) (append proparties '(front-sticky t rear-nonsticky t read-only t)))
	(set-marker rc-insert-marker (point))))))

(defun rc-shrink-name (name)
  "Shrink NAME by LIMIT."
  (if (< (length name) 10)
      name
    (concatenate 'string
		 (subseq name 0 4)
		 "~"
		 (reverse (subseq (reverse name) 0 5)))))

(defun rc-insert-msg (msg)
  "Write MSG to buffer.

This writes chat-message to buffer.
MSG - Rocket.chat's msg struct.
`rc-buffer' - buffer for use by this."
  (with-current-buffer rc-buffer
    (save-excursion
      (goto-char rc-insert-marker)
      (let ((name (rc-shrink-name (rc-user-username (message-user-info msg))))
	    (time-str (concat "(" (rc-format-time (rc-time-to-local-time (message-time-stamp msg))) ")"))
	    (old-point (point))
	    (inhibit-read-only t))
	(rc-insert name (list 'face (if (rc-yourself-p name rc-current-session)
				       'rc-username-face
				     'rc-participant-face)))
	(rc-insert (concat time-str
			   "> "
			   (message-message msg)
			   "\n")
		   (list 'message-info msg))))))

(defun rc-insert-prompt (&optional prompt)
  "Insert input PROMPT to buffer."
  (with-current-buffer rc-buffer
    (let ((prompt (or prompt "<"))
	  (old-point nil)
	  (inhibit-read-only t))
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

(defun rc-insert-system (msg)
  "This insert MSG to `rc-buffer' with FACE-SYMBOL."
  (rc-insert msg (list 'face 'rc-system-face)))

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
	(mapcar #'rc-insert-msg (reverse msgs))
	(rc-insert-prompt)
	(goto-char rc-input-marker)
	(rc-update-channel-daemon)))))

(defun rc-update-channel ()
  "Update displayed channel contents.

`rc-current-session' - Infomation of logined server"
  (interactive)
  (cl-labels ((inner-remove-until (pred list)
				  (if (or (funcall pred (car list))
					  (null list))
				      list
				    (inner-remove-until pred (cdr list)))))
    (with-current-buffer rc-buffer
      (let* ((last (rc-last-updated-time rc-current-session))
	     (last-msg (get-text-property (1- rc-insert-marker) 'message-info))
	     ;; late 0.48
	     (msgs (channels-history (rc-session-server rc-current-session)
				     (rc-session-token rc-current-session)
				     (channel-id (rc-session-channel rc-current-session))
				     ;; TODO: need post-id?
				     :oldest (rc-local-time-to-rc-time last)
				     ;; TODO: resarch count
				     :count rc-reading-post-num))
	     (inhibit-read-only t))
	;; FIXME: If multiple posts exists at the same time, update dosen't work.
	(when (and msgs (> (length msgs) 1))
	  (loop for msg in (cdr (inner-remove-until
				 (lambda (x) (equal (message-id x)
						    (message-id last-msg)))
				 (reverse msgs)))
		do (rc-insert-msg msg)))))))


(defun async-rc-update-channel ()
  "Update async.

`rc-current-session' - Infomation of logined server"
  (interactive)
  (with-current-buffer rc-buffer
    (async-channels-history (rc-session-server rc-current-session)
			    (rc-session-token rc-current-session)
			    (channel-id (rc-session-channel rc-current-session))
			    ;; fetch newer post
			    :oldest (rc-local-time-to-rc-time
				     (rc-last-updated-time rc-current-session))
			    :count rc-reading-post-num
			    :callback
			    (function*
			     (lambda (&key data &allow-other-keys)
			       ;; to remove older post
			       (cl-labels ((inner-remove-until (pred list)
							       (if (or (funcall pred (car list))
								       (null list))
								   list
								 (inner-remove-until pred (cdr list)))))
				 (let ((last-msg (get-text-property (1- rc-insert-marker) 'message-info))
				       (msgs (map 'list #'json-to-msg (assoc-val 'messages data)))
				       (inhibit-read-only nil))
				   (when (assoc-val 'success data)
				     (when (> (length msgs) 1)
				       (dolist (msg (cdr (inner-remove-until
							  (lambda (x)
							    (equal (message-id x)
								   (message-id last-msg)))
							  (reverse msgs))))
					 (rc-insert-msg msg)))))))))))

;; TODO: make configurable
(setf interval 2)
(defun rc-update-channel-daemon ()
  "This update posts in channel of SESSION."
  (interactive)
  (async-start
   `(lambda ()
      (sleep-for ,interval))
   (lambda (result)
     (with-local-quit
       (when (and (buffer-live-p rc-buffer)
		  (buffer-local-value 'rc-insert-marker rc-buffer))
	 (with-current-buffer rc-buffer
	   (async-rc-update-channel))
	 (rc-update-channel-daemon))))))

(defun rc-latest-updated-time (session)
  "This return time of CHANNEL's last post on SESSION."
  (let ((channel (channels-info (rc-session-server session)
				(rc-session-token session)
				(channel-id (rc-session-channel session))
				t)))
    (rc-time-to-local-time (channel-lm channel))))

(defun rc-last-updated-time (session)
  "This return latest time of updated posts in SESSION."
  (rc-time-to-local-time
   (channel-lm (rc-session-channel session))))

(defun rc-get-user-status (url token user-name)
  "This return user's information.

URL - server
TOKEN - token for access
USER-NAME - user's name"
  (users-get-presence url token user-name))

;; :TODO take arg to fetch user-name.
;; name -> show status of that user that is name.
;; all -> show all user's status
(defun rc-show-user-list ()
  "This insert user-list to channel's buffer."
  (interactive)
  (with-current-buffer rc-buffer
    (let ((channel (channels-info (rc-session-server rc-current-session)
				  (rc-session-token rc-current-session)
				  (channel-name (rc-session-channel rc-current-session)))))
      (rc-insert-system "\n")
      (rc-insert-system "* USER LIST *\n")
      (mapc (lambda (x)
	      (rc-insert-system x)
	      (rc-insert-system "@")
	      (rc-insert-system (car (rc-get-user-status (rc-session-server rc-current-session)
							 (rc-session-token rc-current-session)
							 x)))
	      (rc-insert-system "\n"))
	    (channel-usernames channel)))))

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

;; TODO: encode
(defun rc-post-line ()
  "This posts line at input-area to connected server."
  (interactive)
  (let ((input (rc-user-input)))
    (if (not (string= input ""))
	(progn
	  (rc-post (encode-coding-string input 'utf-8) rc-current-session)
	  (delete-region rc-input-marker (point-max))
	  ;; late!! 0.91 sec
	  (rc-update-channel))
      (message "rc: Ignoring blank line."))))

(defun rocket-chat-mode ()
  "Major mode for Rocket.chat."
  (kill-all-local-variables)
  (use-local-map rocket-chat-mode-map)
  (setf mode-name "Rocket.chat"
	major-mode 'rocket-chat-mode
	local-addrev-table rocket-chat-mode-abbrev-table)
  ;;(set-syntax-table syntax-table)
  (run-hooks 'rocket-chat-mode-hook))

;; :TODO override key-map C-a for set-top-to-input-marker

(provide 'rocket-chat)
;;; rocket-chat.el ends here

;; (setf im (im-list url test))
;; im-my
;; (setf im-my(rc-im-id (car im)))

;; (coerce (assoc-val 'usernames (aref (cdar im) 0)) 'list)
;; (assoc-val 'ts (aref (cdar im) 0))

;; (car )
;; (dolist (msg (im-history url test im-my))
;;   (print (message-message msg)))

;; (defun list-direct-msgs (session)
;;   "This return alist of name and id.

;; SESSION - rc session"
;;   (let ((host (rc-session-server session))
;;	(token (rc-session-token session)))
;;     (im-list host token)))

;; (defun open-direct-msg ())
