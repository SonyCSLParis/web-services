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

;; some example queries : 
(query-merriam-webster-dictionary "singer")
(query-merriam-webster-thesaurus "singer")
(query-words-api "singer")
(query-words-api "singer")
(search-wikipedia "Steve McQueen")
(search-entity-in-wikidata "Steve McQueen")
(get-wikidata-entity "Q159347")
(query-google-knowledge-graph "Steve McQueen" :types "person")
(vua-kg-api-request)

