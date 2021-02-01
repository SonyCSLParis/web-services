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

(defun clean-request (request)
  (regex-replace-all " " request "%20"))

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

;; *************************************************************************************************************
;; 1.Querying dictionaries and thesaurus in English
;; *************************************************************************************************************

;; -------------------------------------------------------------------------------------------------------------
;; 1.1. Words API
;; -------------------------------------------------------------------------------------------------------------

(defun request-words-api (word &key (x-rapidapi-key "ef0b0b01fbmshe99d52e360999bcp116ad7jsn90dcbb775019"))
  "Search for a particular token in Words API. "
  (let* ((cleaned-word (clean-request word))
         (url (format nil "https://wordsapiv1.p.rapidapi.com/words/~a/definitions" cleaned-word)))
    (request-api url
               :additional-headers `(("x-rapidapi-key" . ,x-rapidapi-key)
                                     ("x-rapidapi-host" . "wordsapiv1.p.rapidapi.com")
                                     ("useQueryString" . "true")))))

;; -------------------------------------------------------------------------------------------------------------
;; 1.2. Merriam Webster
;; -------------------------------------------------------------------------------------------------------------

(defun request-merriam-webster-dictionary (word &optional (api-key "8fe92f45-0f31-4ec1-8b3f-c11cb403d657"))
    "Search for a particular token in Merriam-Webster Dictionary API"
  (let ((url (format nil "https://www.dictionaryapi.com/api/v3/references/collegiate/json/~a?key=~a" (clean-request word) api-key)))
    (request-api url)))

(defun request-merriam-webster-thesaurus (word &optional (api-key "da135313-5c0b-4819-91d6-dc4bf0d4d09c"))
     "Search for a particular token in the Merriam-Webster Thesaurus API"
  (let ((url (format nil "https://www.dictionaryapi.com/api/v3/references/thesaurus/json/~a?key=~a" (clean-request word) api-key)))
    (request-api url)))

;; -------------------------------------------------------------------------------------------------------------
;; 1.3. Datamuse
;; -------------------------------------------------------------------------------------------------------------

(defun request-rhyme-datamuse (word)
  "Search all the tokens that rhyme with a certain token in Datamuse"
  (let ((url (format nil "https://api.datamuse.com/words?rel_rhy=~a" (clean-request word))))
    (request-api url)))

(defun request-related-to-datamuse (word)
  "Search all tokens semantically related to a particular token"
    (let ((url (format nil "https://api.datamuse.com/words?ml=~a" (clean-request word))))
    (request-api url)))

(defun request-rhyme-related-to-datamuse (word &key related-to)
  "Search a token semantically related to another particular token in Datamuse."
  (request-api "https://api.datamuse.com/words?"
             :parameters `(("rel_rhy" . ,word)
                           ,@(when related-to `(("ml" . ,related-to))))))

(defun request-adjectives-datamuse (word)
    "Search for adjectives mostly used with a particular token"
    (let ((url (format nil "https://api.datamuse.com/words?rel_jjb=~a" (clean-request word))))
    (request-api url)))

(defun request-adjectives-related-to-datamuse (word &key related-to)
    "Search the adjectives mostly used with a particular token-1 and semantically related to another token-2"
  (request-api "https://api.datamuse.com/words?"
             :parameters `(("rel_jjb" . ,word)
                           ,@(when related-to `(("ml" . ,related-to))))))

(defun request-nouns-datamuse (word)
  "Search the most used nouns with a particular adjective"
  (let ((url (format nil "https://api.datamuse.com/words?rel_jja=~a" (clean-request word))))
    (request-api url)))

(defun request-often-follow-datamuse(word)
  "Search all the tokens that more likely follow a token-1"
  (let ((url (format nil "https://api.datamuse.com/words?lc=~a" (clean-request word))))
    (request-api url)))

(defun request-often-follow-start-by-datamuse(word &key start-by)
  "Search  all the tokens that more likely follow a word-1 and start by a particular letter"
  (request-api "https://api.datamuse.com/words?"
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
  (let ((url (format nil "https://www.themealdb.com/api/json/v1/1/search.php?s=~a" (clean-request meal) api-key)))
    (request-api url)))

(defun request-mealDB-ingredient(ingredient &optional (api-key "1"))
  "Search for a recipe in the MealDB with a main ingredient X"
  (let ((url (format nil "https://www.themealdb.com/api/json/v1/1/filter.php?i=~a" (clean-request ingredient))))
    (request-api url)))

(defun request-mealDB-category-filter(category &optional (api-key "1"))
  "Search for all the meals in a particular category"
  (let ((url (format nil "https://www.themealdb.com/api/json/v1/1/filter.php?c=~a" (clean-request category))))
    (request-api url)))

(defun request-mealDB-country-filter(country &optional (api-key "1"))
  "Search for all the meals traditional of a particular country"
  (let ((url (format nil "https://www.themealdb.com/api/json/v1/1/filter.php?a=~a" (clean-request country))))
    (request-api url)))

(defun request-mealDB-hungry-random(&optional (api-key "1"))
  "I am hungy, what should I eat tonight?"
  (let ((url (format nil "https://www.themealdb.com/api/json/v1/1/random.php")))
    (request-api url)))

;; ------------------------------------------------------------------------------------------------------------
;; 2.2. Mediastack
;; ------------------------------------------------------------------------------------------------------------

(defun request-live-news-Mediastack (search &key (access_key "357acde6d8d40889c97558fc6581649e") categories countries languages limit sort)
  "Search for Live News in the Mediastack APIs"
  (request-api "http://api.mediastack.com/v1/sources/?access_key=~a"
               :parameters `(("keywords" . ,search)
                             ("access_key" . ,access_key)
                             ,@(when categories `(("categories" . ,categories)))
                             ,@(when languages `(("languages" . ,languages)))
                             ,@(when countries `(("countries" . ,countries)))
                             ,@(when limit `(("limit" . ,limit)))
                             ,@(when sort `(("sort" . ,sort))))))


(defun request-historical-news-Mediastack (search &key (access_key "357acde6d8d40889c97558fc6581649e") date sources categories countries languages limit sort)
  "Search for historical news in the Mediastack APIs"
  (request-api "http://api.mediastack.com/v1/sources/?access_key=~a"
               :parameters `(("keywords" . ,search)
                             ("access_key" . ,access_key)
                             ,@(when date `(("date" . ,date)))
                             ,@(when sources `(("sources" . ,sources)))
                             ,@(when categories `(("categories" . ,categories)))
                             ,@(when countries `(("countries" . ,countries)))
                             ,@(when languages `(("languages" . ,languages)))
                             ,@(when limit `(("limit" . ,limit)))
                             ,@(when sort `(("sort" . ,sort)))))) ;; it won't work for now because we have a free plan --> upgrade to use it. 

;; ***********************************************************************************************************
;; 3. KG APIs
;; ***********************************************************************************************************

;; -----------------------------------------------------------------------------------------------------------
;; 3.1. Mediawiki
;; -----------------------------------------------------------------------------------------------------------

(defun request-wikipedia (search-string)
  "Function to search for a token in Wikipedia"
  (request-api "https://en.wikipedia.org/w/api.php?"
             :parameters `(("action" . "query")
                           ("list" . "search")
                           ("srsearch" . ,(clean-request search-string))
                           ("format" . "json"))))

;; -----------------------------------------------------------------------------------------------------------
;; 3.2. Wikidata
;; -----------------------------------------------------------------------------------------------------------

(defun request-wikidata-entity (entity &key (language "en"))
  "Search for a token in Wikidata"
  (request-api "https://www.wikidata.org/w/api.php?"
             :parameters `(("action" . "wbsearchentities")
                           ("search" . ,(clean-request entity))
                           ("language" . ,language)
                           ("format" . "json"))))

(defun request-wikidata-URI (ids &key (languages "en"))
  "If you know the entity URI, get it directly."
  (request-api "https://www.wikidata.org/w/api.php?"
             :parameters `(("action" . "wbgetentities")
                           ("ids" . ,ids)
                           ("languages" . ,languages)
                           ("format" . "json")
                           ("props" . "descriptions"))))

;; -----------------------------------------------------------------------------------------------------------
;; 3.4. Google KG API
;; -----------------------------------------------------------------------------------------------------------

(defun request-google-knowledge-graph (request &key (api-key "AIzaSyAyAJvAjxrryYlHBSwGFoHvKzmJWG2KQOo") ;; Use your own API key
                                           (limit "2") (languages "en") types)
  "Search a token in the Google Knowledge Graph APIs"
  (let ((cleaned-request (regex-replace-all " " request "+")))
    (request-api "https://kgsearch.googleapis.com/v1/entities:search?"
               :parameters `(("query" . ,cleaned-request)
                             ("key" . ,api-key)
                             ("limit" . ,limit)
                             ("languages" . ,languages)
                             ,@(when types `(("types" . ,types)))))))


;; -----------------------------------------------------------------------------------------------------------
;; 3.5. Catasto
;; -----------------------------------------------------------------------------------------------------------

(defun request-catasto (&key (content-type "application/json"))
  "Function to query the Catasto knowledge graph API"
  (let* ((http-request (format nil "https://api.druid.datalegend.net/queries/muhaiuser/example-query/run"))
         (stream (drakma:http-request http-request :method :get :content-type content-type 
                                      :want-stream t)))
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8) 
    (yason:parse stream :object-as :alist)))
