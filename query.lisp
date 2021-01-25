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
;; General Function for Querying an API
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun clean-query (query)
  (regex-replace-all " " query "%20"))

(defun query-api (api-url &key (content-type "application/json")
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

;; *************************************************************************************************************
;; Querying dictionaries and thesaurus in English
;; *************************************************************************************************************

;; -------------------------------------------------------------------------------------------------------------
;; Words API
;; -------------------------------------------------------------------------------------------------------------

(defun query-words-api (word &key (x-rapidapi-key "ef0b0b01fbmshe99d52e360999bcp116ad7jsn90dcbb775019"))
  "Query Words dictionary"
  (let* ((cleaned-word (clean-query word))
         (url (format nil "https://wordsapiv1.p.rapidapi.com/words/~a/definitions" cleaned-word)))
    (query-api url
               :additional-headers `(("x-rapidapi-key" . ,x-rapidapi-key)
                                     ("x-rapidapi-host" . "wordsapiv1.p.rapidapi.com")
                                     ("useQueryString" . "true")))))

;; -------------------------------------------------------------------------------------------------------------
;; Merriam Webster
;; -------------------------------------------------------------------------------------------------------------

(defun query-merriam-webster-dictionary (word &optional (api-key "8fe92f45-0f31-4ec1-8b3f-c11cb403d657"))
    "Query Merriam Webster dictionary"
  (let ((url (format nil "https://www.dictionaryapi.com/api/v3/references/collegiate/json/~a?key=~a" (clean-query word) api-key)))
    (query-api url)))

(defun query-merriam-webster-thesaurus (word &optional (api-key "da135313-5c0b-4819-91d6-dc4bf0d4d09c"))
 "Query Merriam Webster thesaurus"
  (let ((url (format nil "https://www.dictionaryapi.com/api/v3/references/thesaurus/json/~a?key=~a" (clean-query word) api-key)))
    (query-api url)))

;; -------------------------------------------------------------------------------------------------------------
;; Datamuse
;; -------------------------------------------------------------------------------------------------------------

(defun query-rhyme-datamuse (word)
  "Search all the words that rhyme with a certain token in datamuse"
  (let ((url (format nil "https://api.datamuse.com/words?rel_rhy=~a" (clean-query word))))
    (query-api url)))

(defun query-rhyme-related-to-datamuse (word &key related-to)
  "Search all the words that rhyme with a certain word-1 in datamuse and are semantically related to another word-2"
  (query-api "https://api.datamuse.com/words?"
             :parameters `(("rel_rhy" . ,word)
                           ,@(when related-to `(("ml" . ,related-to))))))

(defun query-related-to-datamuse (word)
  "Search for words semantically related to a word-1"
    (let ((url (format nil "https://api.datamuse.com/words?ml=~a" (clean-query word))))
    (query-api url)))

(defun query-adjectives-datamuse (word)
    "Search for adjectives mostly used with a word-1"
    (let ((url (format nil "https://api.datamuse.com/words?rel_jjb=~a" (clean-query word))))
    (query-api url)))

(defun query-adjectives-related-to-datamuse (word &key related-to)
    "Search for adjectives mostly used with a word-1 and semantically related to a word-2"
  (query-api "https://api.datamuse.com/words?"
             :parameters `(("rel_jjb" . ,word)
                           ,@(when related-to `(("ml" . ,related-to))))))

(defun query-nouns-datamuse (word)
  "Search for the most used nouns with an adjective x"
  (let ((url (format nil "https://api.datamuse.com/words?rel_jja=~a" (clean-query word))))
    (query-api url)))

(defun query-often-follow-datamuse(word)
  "Search for all the words that more likely follow a word-1"
  (let ((url (format nil "https://api.datamuse.com/words?lc=~a" (clean-query word))))
    (query-api url)))

(defun query-often-follow-start-by-datamuse(word &key start-by)
  "Search for all the words that more likely follow a word-1 and start by a particular letter"
  (query-api "https://api.datamuse.com/words?"
             :parameters `(("lc" . ,word)
                           ,@(when start-by `(("sp" . ,start-by))))))


;; ***********************************************************************************************************
;; Querying Food API 
;; ***********************************************************************************************************

;; -------------------------------------------------------------------------------------------------------------
;; MealDB
;; -------------------------------------------------------------------------------------------------------------

(defun query-mealDB (meal &optional (api-key "1"))
 "Search for a Recipe in the MealDB"
  (let ((url (format nil "https://www.themealdb.com/api/json/v1/1/search.php?s=~a" (clean-query meal) api-key)))
    (query-api url)))

(defun query-ingredient-mealDB(ingredient &optional (api-key "1"))
  "Search for a recipe in the MealDB with a main ingredient X"
  (let ((url (format nil "https://www.themealdb.com/api/json/v1/1/filter.php?i=~a" (clean-query ingredient))))
    (query-api url)))

(defun query-category-filter-mealDB(category &optional (api-key "1"))
  "Search for all the meals in a particular category"
  (let ((url (format nil "https://www.themealdb.com/api/json/v1/1/filter.php?c=~a" (clean-query category))))
    (query-api url)))

(defun query-country-filter-mealDB(country &optional (api-key "1"))
  "Search for all the meals traditional of a particular country"
  (let ((url (format nil "https://www.themealdb.com/api/json/v1/1/filter.php?a=~a" (clean-query country))))
    (query-api url)))

(defun query-hungry-random-mealDB(&optional (api-key "1"))
  "I am hungy, what should I eat tonight?"
  (let ((url (format nil "https://www.themealdb.com/api/json/v1/1/random.php")))
    (query-api url)))

;; ***********************************************************************************************************
;; Querying KG API 
;; ***********************************************************************************************************

;; -----------------------------------------------------------------------------------------------------------
;; Google KG API
;; -----------------------------------------------------------------------------------------------------------

(defun query-google-knowledge-graph (query &key (api-key "AIzaSyAyAJvAjxrryYlHBSwGFoHvKzmJWG2KQOo") ;; Use your own API key
                                           (limit "2") (languages "en") types)
  (let ((cleaned-query (regex-replace-all " " query "+")))
    (query-api "https://kgsearch.googleapis.com/v1/entities:search?"
               :parameters `(("query" . ,cleaned-query)
                             ("key" . ,api-key)
                             ("limit" . ,limit)
                             ("languages" . ,languages)
                             ,@(when types `(("types" . ,types)))))))

;; -----------------------------------------------------------------------------------------------------------
;; Wikipedia and Wikidata 
;; -----------------------------------------------------------------------------------------------------------

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


;; -----------------------------------------------------------------------------------------------------------
;; Catasto
;; -----------------------------------------------------------------------------------------------------------

(defun vua-kg-api-request (&key (content-type "application/json"))
  "Function to query the VUA knowledge graph API"
  (let* ((http-request (format nil "https://api.druid.datalegend.net/queries/muhaiuser/example-query/run"))
         (stream (drakma:http-request http-request :method :get :content-type content-type 
                                      :want-stream t)))
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8) 
    (yason:parse stream :object-as :alist)))



