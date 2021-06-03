(in-package :webdriver-client)

(defun detect-platform-name ()
  (string-downcase (symbol-name (uiop/os:operating-system))))

(defun make-capabilities (always-match &rest first-match)
  "Helper function for creating capabilities.

Category: Capabilities"
  `((:always-match . ,always-match)
    (:first-match . ,first-match)))

(defun merge-capabilities (cap1 cap2)
  "Merge two capabilities

Category: Capabilities"
  `((:always-match . ,(append (aget cap1 :always-match)
			      (aget cap2 :always-match)))
    (:first-match . ,(append (aget cap1 :first-match)
			     (aget cap2 :first-match))))    
  )

;; (defparameter *default-capabilities*
;;   (make-capabilities
;;    `((platform-name . ,(detect-platform-name)))
;;    '(browser-name . "chrome")
;;    '(browser-name . "firefox"))
;;   "The default capabilities.

;; Category: Capabilities")

(defparameter *default-capabilities*
  (make-capabilities
   `((platform-name . ,(detect-platform-name))
     (browser-name . "chrome")))
  "The default capabilities.

Category: Capabilities")

(defun chrome-args (&rest args)
  "Specifies Chrome args"
  `("goog:chromeOptions" . ((:args . ,(coerce args 'vector)))))

;; (defun make-capabilities-for-chrome (&key
;; 				       (headlessp nil)
;; 				       (user-data-dir nil)
;; 				       (user-agent nil))
					
;;   (let* ((sub-obj-args '())
;;          (sub-obj (wd-obj))
;;          (obj (wd-obj
;;                :browser-name "chrome")))
;;     (when w3c-p
;;       (setf (wd-ref sub-obj :w3c) t))
;;     (when headlessp
;;       (push "--headless" sub-obj-args))
;;     (when user-agent
;;       (push (format nil "--user-agent='~a'" (compile-user-agent user-agent)) sub-obj-args))
;;     (when user-data-dir
;;       (push (format nil "--user-data-dir=~a" user-data-dir) sub-obj-args))

;;     (setf (wd-ref sub-obj "args") sub-obj-args
;;           (wd-ref obj "goog:chromeOptions") sub-obj)
;;     obj))

;; (defun make-capabilities-for-firefox (&key
;; 					(headlessp nil)
;; 					(profile nil)
;; 					(user-agent nil))
;;   "you can specify profile name with :profile argument.
;; you can create firefox profile with `firefox-bin -CreateProfile \"profile_name profile_dir\".
;; ref: https://developer.mozilla.org/en-US/docs/Mozilla/Command_Line_Options
;;  "
;;   (let* ((sub-obj-args '())
;;          (sub-obj-prefs (wd-obj))
;;          (sub-obj (wd-obj))
;;          (obj (wd-obj
;;                "browserName" "firefox")))
;;     (when headlessp
;;       (push "-headless" sub-obj-args))
;;     (when profile
;;       ;;(push (format nil "-P \"~s\"" profile) sub-obj-args)
;;       (push (format nil "--profile \"~s\"" profile) sub-obj-args))
;;     (when user-agent
;;       ;;(setf (wd-ref sub-obj-prefs "general.useragent.extra.firefox") (compile-user-agent user-agent)))
;;       (setf (wd-ref sub-obj-prefs "general.useragent.override") (compile-user-agent user-agent)
;;             (wd-ref sub-obj-prefs "general.useragent.updates.enabled") t))


;;     (setf (wd-ref sub-obj "args") sub-obj-args
;;           (wd-ref sub-obj "prefs") sub-obj-prefs
;;           (wd-ref obj "moz:firefoxOptions") sub-obj)
;;     obj))
