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

; -----------------------------------------------------------------------------------------------------------
;; Mediawiki
;; -----------------------------------------------------------------------------------------------------------

(defun request-wikipedia (search-string)
  "Function to search for a token in Wikipedia"
  (request-api "https://en.wikipedia.org/w/api.php?"
             :parameters `(("action" . "query")
                           ("list" . "search")
                           ("srsearch" . ,(clean-request search-string))
                           ("format" . "json"))))

;; -----------------------------------------------------------------------------------------------------------
;; Wikidata
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