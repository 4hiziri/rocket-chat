;;; package --- summary
;;; Commentary:
;;; Code:

(load "request.el")
(require 'json)

(defvar test-url "https://rc.net.itc.nagoya-u.ac.jp")
(defvar test "")
(request (concat test-url "/api/v1/info")
	 :parser 'json-read
	 :success (function*
		   (lambda (&key data &allow-other-keys)
		     (setf test data))))

test
(request (concat test-url "/api/v1/info")
	 :parser 'buffer-string
	 :complete (function*
		    (lambda (&key data &allow-other-keys)
		      (switch-to-buffer "*request-result*")
		      (erase-buffer)
		      (insert data))))
(json-read-from-string "{\"info\": {\"version\": \"0.57.1\"}, \"success\": true}")

(provide 'rocket-chat)
;;; rocket-chat ends here
