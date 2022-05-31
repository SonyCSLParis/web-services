;; Copyright 2021 Sony CSL Paris

;;; Copyright (C) Sony Computer Science Laboratories Paris
;;;               Remi van Trijp (www.remivantrijp.eu)
;;;               Martina Galletti
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

(export '(request-api))

;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; General Functions for all APIs
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun clean-request (request &optional (replacement-string "%20"))
  (regex-replace-all " " request replacement-string))

(defun request-api (api-url &key 
                            (method :get)
                            (content-type "application/json")
                            additional-headers parameters
                            (lisp-format :alist))
  "Interfacing with an API and encode the result in Lisp List"
  (let ((stream (http-request api-url
                              :additional-headers additional-headers
                              :parameters parameters
                              :method method
                              :content-type content-type
                              :want-stream t)))
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
    (yason:parse stream :object-as lisp-format)))

;; Helper functions:
;; -----------------------------------------------------------------------------------------------------------
(defun handle-parameters (parameters)
  "Helper function for turning correctly formatting keywords into drakma parameters."
  (if (null parameters)
    nil
    (cons (cons (format nil "~(~a~)" (first parameters))
                (second parameters))
          (handle-parameters (cddr parameters)))))
;; (handle-parameters '(:action "search" :language "en"))
