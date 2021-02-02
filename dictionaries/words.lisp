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
;;=========================================================================;; ---------------------------------------------------------------

(in-package :web-services)
(export '(request-words-api))

;; ----------------------------------------------
;;  Words API
;; -------------------------------------------------------------------------------------------------------------

(defun request-api (api-url &key (content-type "application/json")
                          additional-headers parameters)
  "Interfacing with an API and encode the result in Lisp List"
  (let ((stream (http-request api-url
                              :additional-headers additional-headers
                              :parameters parameters
                              :method :get
                              :content-type content-type
                              :want-stream t)))
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
    (yason:parse stream :object-as :alist)))

(defun request-words-api (word &optional (api-key))
  "Search for a particular token in Words API. "
  (let* ((api-key (retrieve-api-key :words))
  (let* ((cleaned-word (clean-request word))
         (url (format nil "https://wordsapiv1.p.rapidapi.com/words/~a/definitions" cleaned-word)))
    (request-api url
               :additional-headers `(,@(when api-key `(("x-rapidapi-key" . ,api-key)))
                                     ("x-rapidapi-host" . "wordsapiv1.p.rapidapi.com")
                                     ("useQueryString" . "true")))))))

(request-words-api "singer")





