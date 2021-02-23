(in-package :web-services)
(export '(*api-keys* get-api-entry get-api-key get-api-keys reset-api-keys set-api-key set-api-keys))

(defparameter *api-keys* nil "Keychain for APIs.")

(defun get-api-entry (service &optional (keychain *api-keys*))
  "Function that knows how to find a service and its API from the keychain."
  (assoc service keychain))

(defun get-api-key (service &optional (keychain *api-keys*))
  "Central function that retrieves API keys using a service name."
  (let ((api-key (rest (get-api-entry service keychain))))
    (if api-key
      api-key
      (error (format nil "No API key was found for the service :~a" service)))))

(defun reset-api-keys ()
  (setf *api-keys* nil))

(defun set-api-key (service api-key)
  (let ((existing-entry (get-api-entry service))
        (new-entry (cons service api-key)))
     (if existing-entry
       (setf *api-keys* (substitute new-entry existing-entry *api-keys* :test #'equal))
       (push new-entry *api-keys*))))

(defun set-api-keys (&rest args)
  (if (null args)
    *api-keys*
    (progn
      (assert (keywordp (first args)))
      (assert (stringp (second args)))
      (set-api-key (first args) (second args))
      (eval (cons 'set-api-keys (subseq args 2))))))
