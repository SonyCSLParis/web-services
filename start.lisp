;; Copyright (C) Sony Computer Science Laboratories Paris
;;               Remi van Trijp (www.remivantrijp.eu)
;;               Martina Galletti
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
(ql:quickload :web-services)
(in-package :web-services)

;; -------------------------------------------------------------------------------------------------------------
;; 1. Some example of searches for dictionaries and thesaurus
;; -------------------------------------------------------------------------------------------------------------

;;merriam-webster
(request-merriam-webster-dictionary "singer")
(request-merriam-webster-thesaurus "singer")
;;words
(request-words-api "singer")
;;datamuse
(request-rhyme-datamuse "vice")
(request-related-to-datamuse "vice")
(request-rhyme-related-to-datamuse "vice" :related-to "food") 
(request-adjectives-datamuse "ocean")
(request-adjectives-related-to-datamuse "ocean" :related-to "temperature")
(request-nouns-datamuse "yellow")
(request-often-follow-datamuse "drink")
(request-often-follow-start-by-datamuse "drink" :start-by "w*")

;; -------------------------------------------------------------------------------------------------------------
;; 2. Some example of searches for Open access APIs related to a certain area
;; -------------------------------------------------------------------------------------------------------------

;; food - Meal DB
(request-mealDB "Carbonara")
(request-ingredient-mealDB "chicken_breast")
(request-category-filter-mealDB "Seafood")
(request-country-filter-mealDB "Italian")
(request-hungry-random-mealDB)

;; newspapers - Mediastack 
(request-live-news-Mediastack "Barcelona" :categories "sport" :languages "it" :countries "it" :limit "2" :sort "published_asc")
(request-historical-news-Mediastack "Barcelona" :date "29-01-20" :sources "cnn" :categories "sport" :countries "us" :languages "en" :limit "2":sort "published_asc") ;; it won't work for now because we have a free plan --> upgrade to use it. 
;; -------------------------------------------------------------------------------------------------------------
;; 2. Some example of searches for KG APIs
;; -------------------------------------------------------------------------------------------------------------

;; Mediawiki
(search-wikipedia "Steve McQueen")
;; Wikidata
(search-entity-in-wikidata "Steve McQueen")
(get-wikidata-entity "Q159347")
;; Google
(request-google-knowledge-graph "Steve McQueen" :types "person")
;; Catasto
(catasto-request)
