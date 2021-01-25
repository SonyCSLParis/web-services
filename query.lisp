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
(in-package :apis)

;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Functions for Querying an API
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun clean-query (query)
  (regex-replace-all " " query "%20"))

(defun query-api (api-url &key (content-type "application/json")
                          additional-headers parameters)
  "Interfacing with an API such as Wikidata."
  (let ((stream (http-request api-url
                              :additional-headers additional-headers
                              :parameters parameters
                              :method :get
                              :content-type content-type
                              :want-stream t)))
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
    (yason:parse stream :object-as :alist)))

;; *************************************************************************************************************
;; Querying the Google KB:
;; *************************************************************************************************************

(defun query-google-knowledge-graph (query &key (api-key "AIzaSyAyAJvAjxrryYlHBSwGFoHvKzmJWG2KQOo") ;; Use your own API key
                                           (limit "2") (languages "en") types)
  (let ((cleaned-query (regex-replace-all " " query "+")))
    (query-api "https://kgsearch.googleapis.com/v1/entities:search?"
               :parameters `(("query" . ,cleaned-query)
                             ("key" . ,api-key)
                             ("limit" . ,limit)
                             ("languages" . ,languages)
                             ,@(when types `(("types" . ,types)))))))

;; *************************************************************************************************************
;; Querying Wikipedia and WikiData:
;; *************************************************************************************************************

(defun search-wikipedia (search-string)
  (query-api "https://en.wikipedia.org/w/api.php?"
             :parameters `(("action" . "query")
                           ("list" . "search")
                           ("srsearch" . ,(clean-query search-string))
                           ("format" . "json"))))

(defun search-entity-in-wikidata (entity &key (language "en"))
  "Search for entities using an alias"
  (query-api "https://www.wikidata.org/w/api.php?"
             :parameters `(("action" . "wbsearchentities")
                           ("search" . ,(clean-query entity))
                           ("language" . ,language)
                           ("format" . "json"))))

(defun get-wikidata-entity (ids &key (languages "en"))
  "If you know the entity IDS, get it directly."
  (query-api "https://www.wikidata.org/w/api.php?"
             :parameters `(("action" . "wbgetentities")
                           ("ids" . ,ids)
                           ("languages" . ,languages)
                           ("format" . "json")
                           ("props" . "descriptions"))))


;; *************************************************************************************************************
;; Querying dictionaries and thesauri:
;; *************************************************************************************************************

(defun query-words-api (word &key (x-rapidapi-key "ef0b0b01fbmshe99d52e360999bcp116ad7jsn90dcbb775019"))
  (let* ((cleaned-word (clean-query word))
         (url (format nil "https://wordsapiv1.p.rapidapi.com/words/~a/definitions" cleaned-word)))
    (query-api url
               :additional-headers `(("x-rapidapi-key" . ,x-rapidapi-key)
                                     ("x-rapidapi-host" . "wordsapiv1.p.rapidapi.com")
                                     ("useQueryString" . "true")))))

(defun query-merriam-webster-dictionary (word &optional (api-key "8fe92f45-0f31-4ec1-8b3f-c11cb403d657"))
  (let ((url (format nil "https://www.dictionaryapi.com/api/v3/references/collegiate/json/~a?key=~a" (clean-query word) api-key)))
    (query-api url)))

(defun query-merriam-webster-thesaurus (word &optional (api-key "da135313-5c0b-4819-91d6-dc4bf0d4d09c"))
  (let ((url (format nil "https://www.dictionaryapi.com/api/v3/references/thesaurus/json/~a?key=~a" (clean-query word) api-key)))
    (query-api url)))



