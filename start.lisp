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
(request-datamuse-rhyme "vice")
(request-datamuse-related-to "vice")
(request-datamuse-rhyme-related-to "vice" :related-to "food") 
(request-datamuse-adjectives "ocean")
(request-datamuse-adjectives-related-to "ocean" :related-to "temperature")
(request-datamuse-nouns "yellow")
(request-datamuse-often-follow "drink")
(request-datamuse-often-follow-start-by "drink" :start-by "w*")

;; -------------------------------------------------------------------------------------------------------------
;; 2. Some example of searches for Open access APIs related to a certain area
;; -------------------------------------------------------------------------------------------------------------

;; food - Meal DB
(request-mealDB "Carbonara")
(request-mealDB-ingredient "chicken_breast")
(request-mealDB-category-filter "Seafood")
(request-mealDB-country-filter "Italian")
(request-mealDB-hungry-random)

;; newspapers - Mediastack 
(request-Mediastack-live-news "Barcelona" :categories "sport" :languages "it" :countries "it" :limit "2" :sort "published_asc")
(request-Mediastack-historical-news "Barcelona" :date "29-01-20" :sources "cnn" :categories "sport" :countries "us" :languages "en" :limit "2":sort "published_asc") ;; it won't work for now because we have a free plan --> upgrade to use it. 

;; -------------------------------------------------------------------------------------------------------------
;; 2. Some example of searches for KG APIs
;; -------------------------------------------------------------------------------------------------------------

;; Mediawiki
(request-wikipedia "Steve McQueen")
;; Wikidata
(request-wikidata-entity "Steve McQueen")
(request-wikidata-URI "Q159347")
;; Google
(request-google-knowledge-graph "Steve McQueen" :types "person")
;; Catasto
(request-catasto)
