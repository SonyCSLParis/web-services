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
;;=========================================================================;;

(in-package :web-services)
(export '(request-words-api))

;; ---------------------------------------------------------------------------------------------------------------------------------------------------
;;  Words API
;; ---------------------------------------------------------------------------------------------------------------------------------------------------

(defun request-words-api (word &optional (x-rapidapi-key *api-key-words*))
  "Search for a particular token in Words API. "
  (let* ((cleaned-word (clean-request word))
         (url (format nil "https://wordsapiv1.p.rapidapi.com/words/~a/definitions" cleaned-word x-rapidapi-key)))
    (request-api url
               :additional-headers `(,@(when x-rapidapi-key `(("x-rapidapi-key" . ,x-rapidapi-key)))
                                     ("x-rapidapi-host" . "wordsapiv1.p.rapidapi.com")
                                     ("useQueryString" . "true")))))





