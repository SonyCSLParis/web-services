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
(export '(request-merriam-webster-collegiate-dictionary request-merriam-webster-collegiate-thesaurus request-merriam-webster-spanish-dictionary request-merriam-webster-doctor-dictionary request-merriam-webster-learners-dictionary request-merriam-webster-elementary-dictionary request-merriam-webster-intermediate-dictionary request-merriam-webster-intermediate-thesaurus request-merriam-webster-school-dictionary))


;; ---------------------------------------------------------------------------------------------------------------------------------------------------
;; Merriam Webster
;; ---------------------------------------------------------------------------------------------------------------------------------------------------


(defun request-merriam-webster-collegiate-dictionary (word &optional (api-key *api-key-merriam*))
    "Search for a particular token in Merriam-Webster Dictionary API"
  (let ((url (format nil "https://www.dictionaryapi.com/api/v3/references/collegiate/json/~a?key=~a" (clean-request word) api-key)))
    (request-api url)))

(defun request-merriam-webster-collegiate-thesaurus (word &optional (api-key *api-key-merriam*))
     "Search for a particular token in the Merriam-Webster Thesaurus API"
  (let ((url (format nil "https://www.dictionaryapi.com/api/v3/references/thesaurus/json/~a?key=~a" (clean-request word) api-key)))
    (request-api url)))

(defun request-merriam-webster-spanish-dictionary (word &optional (api-key *api-key-merriam*))
    "Search for a particular token in Merriam-Webster Spanish Dictionary API"
  (let ((url (format nil "https://www.dictionaryapi.com/api/v3/references/spanish/json/~a?key=~a" (clean-request word) api-key)))
    (request-api url)))

(defun request-merriam-webster-doctor-dictionary (word &optional (api-key *api-key-merriam*))
    "Search for a particular token in Merriam-Webster Medical Dictionary API"
  (let ((url (format nil "https://www.dictionaryapi.com/api/v3/references/medical/json/~a?key=~a" (clean-request word) api-key)))
    (request-api url)))

(defun request-merriam-webster-learners-dictionary (word &optional (api-key *api-key-merriam*))
    "Search for a particular token in Merriam-Webster Learners Dictionary API"
  (let ((url (format nil "https://www.dictionaryapi.com/api/v3/references/learners/json/~a?key=~a" (clean-request word) api-key)))
    (request-api url)))

(defun request-merriam-webster-elementary-dictionary (word &optional (api-key *api-key-merriam*))
    "Search for a particular token in Merriam-Webster Elementary Dictionary API"
  (let ((url (format nil "https://www.dictionaryapi.com/api/v3/references/sd2/json/~a?key=~a" (clean-request word) api-key)))
    (request-api url)))

(defun request-merriam-webster-intermediate-dictionary (word &optional (api-key *api-key-merriam*))
    "Search for a particular token in Merriam-Webster Intermediate Dictionary API"
  (let ((url (format nil "https://www.dictionaryapi.com/api/v3/references/sd3/json/~a?key=~a" (clean-request word) api-key)))
    (request-api url)))

(defun request-merriam-webster-intermediate-thesaurus (word &optional (api-key *api-key-merriam*))
    "Search for a particular token in Merriam-Webster Intermediate Thesaurus API"
  (let ((url (format nil "https://www.dictionaryapi.com/api/v3/references/ithesaurus/json/~a?key=~a" (clean-request word) api-key)))
    (request-api url)))

(defun request-merriam-webster-school-dictionary (word &optional (api-key *api-key-merriam*))
    "Search for a particular token in Merriam-Webster School Dictionary API"
  (let ((url (format nil "https://www.dictionaryapi.com/api/v3/references/sd4/json/~a?key=~a" (clean-request word) api-key)))
    (request-api url)))