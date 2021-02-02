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
(export '(request-wikipedia request-wikidata-entity request-wikidata-URI))

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

(defun request-wikidata-entity (entity &key language limit props type)
  "Search for a token in Wikidata"
  (request-api "https://www.wikidata.org/w/api.php?"
             :parameters `(("action" . "wbsearchentities")
                           ("search" . ,(clean-request entity))
                           ,@(when language `(("language" . ,language)))
                           ,@(when limit `(("limit" . ,limit)))
                           ,@(when props `(("props" . ,props)))
                           ,@(when type `(("type" . ,type)))
                           ("format" . "json"))))

(defun request-wikidata-URI (ids &key redirects titles sites language props)
  "If you know the entity URI, get it directly."
  (request-api "https://www.wikidata.org/w/api.php?"
             :parameters `(("action" . "wbgetentities")
                           ("ids" . ,ids)
                           ,@(when redirects `(("redirects" . ,redirects)))
                           ,@(when titles `(("titles" . ,titles)))
                           ,@(when sites `(("sites" . ,sites)))
                           ,@(when language `(("language" . ,language)))
                           ,@(when props `(("props" . ,props)))
                           ("format" . "json"))))

(defun request-wikimedia-categories (search-results)
  (loop for result in search-results
        unless (string= (rest (assoc "snippet" result :test #'string=))
                        "Wikimedia category")
        collect result))

(defun wikidata-search-entity (entity &key (feeling-lucky t) (filter-wikimedia-categories t))
  (let* ((query (wikidata-query entity))
         (search-results (rest (assoc "search" (rest (assoc "query" query :test #'string=))
                              :test #'string=))))
    (when filter-wikimedia-categories
      (setf search-results (filter-wikimedia-categories search-results)))
    (if feeling-lucky
      (wikidata-get-entity (rest (assoc "title" (first search-results) :test #'string=)))
      search-results)))
