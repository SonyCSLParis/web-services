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

(defpackage :merriam-webster
  (:use :common-lisp)
  (:export :request-merriam-webster-dictionary
           :request-merriam-webster-thesaurus))
;; -------------------------------------------------------------------------------------------------------------
;; Merriam Webster
;; -------------------------------------------------------------------------------------------------------------

(defun request-merriam-webster-dictionary (word &optional (api-key "8fe92f45-0f31-4ec1-8b3f-c11cb403d657"))
    "Search for a particular token in Merriam-Webster Dictionary API"
  (let ((url (format nil "https://www.dictionaryapi.com/api/v3/references/collegiate/json/~a?key=~a" (clean-request word) api-key)))
    (request-api url)))

(defun request-merriam-webster-thesaurus (word &optional (api-key "da135313-5c0b-4819-91d6-dc4bf0d4d09c"))
     "Search for a particular token in the Merriam-Webster Thesaurus API"
  (let ((url (format nil "https://www.dictionaryapi.com/api/v3/references/thesaurus/json/~a?key=~a" (clean-request word) api-key)))
    (request-api url)))