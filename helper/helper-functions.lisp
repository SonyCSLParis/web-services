;; Copyright (C) Sony Computer Science Laboratories Paris
;;               Remi van Trijp (www.remivantrijp.eu)
;;               Martina Galletti
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;=========================================================================

(in-package :web-services)

(export '(set-api-key set-api-keys get-api-keys reset-api-keys))

;; Interface for handling API-KEYS:
;; -------------------------------------------------------------------------

(defparameter *api-keys* nil "Keychain for APIs.")

(defun get-api-entry (service &optional (keychain *api-keys*))
  "Function that knows how to find a service and its API from the keychain."
  (assoc service keychain))
;; (get-api-entry :google-knowledge-graph)

(defun get-api-key (service &optional (keychain *api-keys*))
  "Central function that retrieves API keys using a service name."
  (let ((api-key (rest (get-api-entry service keychain))))
    (if api-key
      api-key
      (error (format nil "No API key was found for the service :~a" service)))))
;; (get-api-key :google-knowledge-graph)
;; (get-api-key :my-service)

(defun reset-api-keys ()
  (setf *api-keys* nil))

(defun set-api-key (service api-key)
  (let ((existing-entry (get-api-entry service))
        (new-entry (cons service api-key)))
     (if existing-entry
       (setf *api-keys* (substitute new-entry existing-entry *api-keys* :test #'equal))
       (push new-entry *api-keys*))))
;; (set-api-key :google-knowledge-graph "AIzaSyDR9MVg0_Zh6QrKD3M7SzZqQJ9Tn9I7GxY")

(defun set-api-keys (&rest args)
  (if (null args)
    *api-keys*
    (progn
      (assert (keywordp (first args)))
      (assert (stringp (second args)))
      (set-api-key (first args) (second args))
      (eval (cons 'set-api-keys (subseq args 2))))))
