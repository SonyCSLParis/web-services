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
;; 1.Querying dictionaries and thesaurus in English
;; *************************************************************************************************************

;; -------------------------------------------------------------------------------------------------------------
;; 1.1. Words API
;; -------------------------------------------------------------------------------------------------------------

(defun request-words-api (word &key (x-rapidapi-key "ef0b0b01fbmshe99d52e360999bcp116ad7jsn90dcbb775019"))
  "Search for a particular token in Words API. "
  (let* ((cleaned-word (clean-query word))
         (url (format nil "https://wordsapiv1.p.rapidapi.com/words/~a/definitions" cleaned-word)))
    (query-api url
               :additional-headers `(("x-rapidapi-key" . ,x-rapidapi-key)
                                     ("x-rapidapi-host" . "wordsapiv1.p.rapidapi.com")
                                     ("useQueryString" . "true")))))

;; -------------------------------------------------------------------------------------------------------------
;; 1.2. Merriam Webster
;; -------------------------------------------------------------------------------------------------------------

(defun request-merriam-webster-dictionary (word &optional (api-key "8fe92f45-0f31-4ec1-8b3f-c11cb403d657"))
    "Search for a particular token in Merriam-Webster Dictionary API"
  (let ((url (format nil "https://www.dictionaryapi.com/api/v3/references/collegiate/json/~a?key=~a" (clean-query word) api-key)))
    (query-api url)))

(defun request-merriam-webster-thesaurus (word &optional (api-key "da135313-5c0b-4819-91d6-dc4bf0d4d09c"))
     "Search for a particular token in the Merriam-Webster Thesaurus API"
  (let ((url (format nil "https://www.dictionaryapi.com/api/v3/references/thesaurus/json/~a?key=~a" (clean-query word) api-key)))
    (query-api url)))

;; -------------------------------------------------------------------------------------------------------------
;; 1.3. Datamuse
;; -------------------------------------------------------------------------------------------------------------

(defun request-rhyme-datamuse (word)
  "Search all the tokens that rhyme with a certain token in Datamuse"
  (let ((url (format nil "https://api.datamuse.com/words?rel_rhy=~a" (clean-query word))))
    (query-api url)))

(defun request-related-to-datamuse (word)
  "Search all tokens semantically related to a particular token"
    (let ((url (format nil "https://api.datamuse.com/words?ml=~a" (clean-query word))))
    (query-api url)))

(defun request-rhyme-related-to-datamuse (word &key related-to)
  "Search a token semantically related to another particular token in Datamuse."
  (query-api "https://api.datamuse.com/words?"
             :parameters `(("rel_rhy" . ,word)
                           ,@(when related-to `(("ml" . ,related-to))))))

(defun request-adjectives-datamuse (word)
    "Search for adjectives mostly used with a particular token"
    (let ((url (format nil "https://api.datamuse.com/words?rel_jjb=~a" (clean-query word))))
    (query-api url)))

(defun request-adjectives-related-to-datamuse (word &key related-to)
    "Search the adjectives mostly used with a particular token-1 and semantically related to another token-2"
  (query-api "https://api.datamuse.com/words?"
             :parameters `(("rel_jjb" . ,word)
                           ,@(when related-to `(("ml" . ,related-to))))))

(defun request-nouns-datamuse (word)
  "Search the most used nouns with a particular adjective"
  (let ((url (format nil "https://api.datamuse.com/words?rel_jja=~a" (clean-query word))))
    (query-api url)))

(defun request-often-follow-datamuse(word)
  "Search all the tokens that more likely follow a token-1"
  (let ((url (format nil "https://api.datamuse.com/words?lc=~a" (clean-query word))))
    (query-api url)))

(defun request-often-follow-start-by-datamuse(word &key start-by)
  "Search  all the tokens that more likely follow a word-1 and start by a particular letter"
  (query-api "https://api.datamuse.com/words?"
             :parameters `(("lc" . ,word)
                           ,@(when start-by `(("sp" . ,start-by))))))


;; ***********************************************************************************************************
;; 2. Open access APIs related to a certain area: 
;; ***********************************************************************************************************

;; ------------------------------------------------------------------------------------------------------------
;; 2.1. MealDB
;; ------------------------------------------------------------------------------------------------------------

(defun request-mealDB (meal &optional (api-key "1"))
 "Search for a Recipe in the MealDB"
  (let ((url (format nil "https://www.themealdb.com/api/json/v1/1/search.php?s=~a" (clean-query meal) api-key)))
    (query-api url)))

(defun request-ingredient-mealDB(ingredient &optional (api-key "1"))
  "Search for a recipe in the MealDB with a main ingredient X"
  (let ((url (format nil "https://www.themealdb.com/api/json/v1/1/filter.php?i=~a" (clean-query ingredient))))
    (query-api url)))

(defun request-category-filter-mealDB(category &optional (api-key "1"))
  "Search for all the meals in a particular category"
  (let ((url (format nil "https://www.themealdb.com/api/json/v1/1/filter.php?c=~a" (clean-query category))))
    (query-api url)))

(defun request-country-filter-mealDB(country &optional (api-key "1"))
  "Search for all the meals traditional of a particular country"
  (let ((url (format nil "https://www.themealdb.com/api/json/v1/1/filter.php?a=~a" (clean-query country))))
    (query-api url)))

(defun request-hungry-random-mealDB(&optional (api-key "1"))
  "I am hungy, what should I eat tonight?"
  (let ((url (format nil "https://www.themealdb.com/api/json/v1/1/random.php")))
    (query-api url)))

;; ------------------------------------------------------------------------------------------------------------
;; 2.2. Mediastack
;; ------------------------------------------------------------------------------------------------------------


(defun request-News-Sources-Mediastack (search &optional (access_key "357acde6d8d40889c97558fc6581649e") search categories languages countries limit)
  "Search for news in the Mediastack APIs"
  (let ((cleaned-search (regex-replace-all " " search "+")))
    (query-api "http://api.mediastack.com/v1/sources/?access_key=~a"
               :parameters `(("search" . ,cleaned-search)
                             ("access_key" . ,access_key)
                             ,@(when categories `(("categories" . ,categories)))
                             ,@(when languages `(("languages" . ,languages)))
                             ,@(when countries `(("countries" . ,countries)))
                             ,@(when limit `(("limit" . ,limit)))))))

(defun request-historical-news-Mediastack (search &optional (access_key "357acde6d8d40889c97558fc6581649e") search date sources categories languages countries keywords limit)
  "Search for news in the Mediastack APIs"
    (query-api "http://api.mediastack.com/v1/sources/?access_key=~a"
               :parameters `(("search" . ,search)
                             ("access_key" . ,access_key)
                             ,@(when date `(("date" . ,date)))
                             ,@(when sources `(("sources" . ,sources)))
                             ,@(when categories `(("categories" . ,categories)))
                             ,@(when languages `(("languages" . ,languages)))
                             ,@(when countries `(("countries" . ,countries)))
                             ,@(when keywords `(("keywords" . ,keywords)))
                             ,@(when limit `(("limit" . ,limit))))))


;; ***********************************************************************************************************
;; 3. KG APIs
;; ***********************************************************************************************************

;; -----------------------------------------------------------------------------------------------------------
;; 3.1. Mediawiki
;; -----------------------------------------------------------------------------------------------------------

(defun search-wikipedia (search-string)
  "Function to search for a token in Wikipedia"
  (query-api "https://en.wikipedia.org/w/api.php?"
             :parameters `(("action" . "query")
                           ("list" . "search")
                           ("srsearch" . ,(clean-query search-string))
                           ("format" . "json"))))

;; -----------------------------------------------------------------------------------------------------------
;; 3.2. Wikidata
;; -----------------------------------------------------------------------------------------------------------

(defun search-entity-in-wikidata (entity &key (language "en"))
  "Search for a token in Wikidata"
  (query-api "https://www.wikidata.org/w/api.php?"
             :parameters `(("action" . "wbsearchentities")
                           ("search" . ,(clean-query entity))
                           ("language" . ,language)
                           ("format" . "json"))))

(defun get-wikidata-entity (ids &key (languages "en"))
  "If you know the entity URI, get it directly."
  (query-api "https://www.wikidata.org/w/api.php?"
             :parameters `(("action" . "wbgetentities")
                           ("ids" . ,ids)
                           ("languages" . ,languages)
                           ("format" . "json")
                           ("props" . "descriptions"))))

;; -----------------------------------------------------------------------------------------------------------
;; 3.4. Google KG API
;; -----------------------------------------------------------------------------------------------------------

(defun request-google-knowledge-graph (query &key (api-key "AIzaSyAyAJvAjxrryYlHBSwGFoHvKzmJWG2KQOo") ;; Use your own API key
                                           (limit "2") (languages "en") types)
  "Search a token in the Google Knowledge Graph APIs"
  (let ((cleaned-query (regex-replace-all " " query "+")))
    (query-api "https://kgsearch.googleapis.com/v1/entities:search?"
               :parameters `(("query" . ,cleaned-query)
                             ("key" . ,api-key)
                             ("limit" . ,limit)
                             ("languages" . ,languages)
                             ,@(when types `(("types" . ,types)))))))


;; -----------------------------------------------------------------------------------------------------------
;; 3.5. Catasto
;; -----------------------------------------------------------------------------------------------------------

(defun catasto-request (&key (content-type "application/json"))
  "Function to query the Catasto knowledge graph API"
  (let* ((http-request (format nil "https://api.druid.datalegend.net/queries/muhaiuser/example-query/run"))
         (stream (drakma:http-request http-request :method :get :content-type content-type 
                                      :want-stream t)))
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8) 
    (yason:parse stream :object-as :alist)))
