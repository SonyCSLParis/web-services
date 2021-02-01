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

;; -------------------------------------------------------------------------------------------------------------
;; Datamuse
;; -------------------------------------------------------------------------------------------------------------

(defun request-datamuse-rhyme (word)
  "Search all the tokens that rhyme with a certain token in Datamuse"
  (let ((url (format nil "https://api.datamuse.com/words?rel_rhy=~a" (clean-request word))))
    (request-api url)))

(defun request-datamuse-related-to (word)
  "Search all tokens semantically related to a particular token"
    (let ((url (format nil "https://api.datamuse.com/words?ml=~a" (clean-request word))))
    (request-api url)))

(defun request-datamuse-rhyme-related-to (word &key related-to)
  "Search a token semantically related to another particular token in Datamuse."
  (request-api "https://api.datamuse.com/words?"
             :parameters `(("rel_rhy" . ,word)
                           ,@(when related-to `(("ml" . ,related-to))))))

(defun request-datamuse-adjectives (word)
    "Search for adjectives mostly used with a particular token"
    (let ((url (format nil "https://api.datamuse.com/words?rel_jjb=~a" (clean-request word))))
    (request-api url)))

(defun request-datamuse-adjectives-related-to (word &key related-to)
    "Search the adjectives mostly used with a particular token-1 and semantically related to another token-2"
  (request-api "https://api.datamuse.com/words?"
             :parameters `(("rel_jjb" . ,word)
                           ,@(when related-to `(("ml" . ,related-to))))))

(defun request-datamuse-nouns (word)
  "Search the most used nouns with a particular adjective"
  (let ((url (format nil "https://api.datamuse.com/words?rel_jja=~a" (clean-request word))))
    (request-api url)))

(defun request-datamuse-often-follow(word)
  "Search all the tokens that more likely follow a token-1"
  (let ((url (format nil "https://api.datamuse.com/words?lc=~a" (clean-request word))))
    (request-api url)))

(defun request-datamuse-often-follow-start-by(word &key start-by)
  "Search  all the tokens that more likely follow a word-1 and start by a particular letter"
  (request-api "https://api.datamuse.com/words?"
             :parameters `(("lc" . ,word)
                           ,@(when start-by `(("sp" . ,start-by))))))
