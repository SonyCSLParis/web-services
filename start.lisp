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

(ql:quickload :apis)

(in-package :apis)

;; some example queries for dictionaries and thesaurus for English 
(query-merriam-webster-dictionary "singer")
(query-merriam-webster-thesaurus "singer")
(query-words-api "singer")
(query-words-api "singer")

;; datamuse related queries 
(query-rhyme-datamuse "vice")
(query-related-to-datamuse "vice")
(query-rhyme-related-to-datamuse "vice" :related-to "food") 
(query-related-to-datamuse "vice")
(query-adjectives-datamuse "ocean")
(query-adjectives-related-to-datamuse "ocean" :related-to "temperature")
(query-nouns-datamuse "yellow")
(query-often-follow-datamuse "drink")
(query-often-follow-start-by-datamuse "drink" :start-by"w*")

;; search in KG API : 
(search-wikipedia "Steve McQueen")
(search-entity-in-wikidata "Steve McQueen")
(get-wikidata-entity "Q159347")
(query-google-knowledge-graph "Steve McQueen" :types "person")
(vua-kg-api-request)

;; food API - Meal DB
(query-mealDB "Carbonara")
(query-ingredient-filter-mealDB "chicken_breast")
(query-category-filter-mealDB "Seafood")
(query-country-filter-mealDB "Italian")
(query-hungry-random-mealDB)

;; Newspaper APIs


;; Music APIs 
