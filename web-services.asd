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
(in-package :asdf)
(defsystem :web-services
  :description "A system querying APIs in Common Lisp"
  :depends-on (:utils :drakma :yason :cl-ppcre :test-framework)
  :components ((:file "package")
               (:file "api_keys")
   ;; All files
   (:module helper
    :components ((:file "general_request")
                 (:file "helper-functions")))           
   ;; Helper functions
   (:module dictionaries
    :components ((:file "datamuse")
                 (:file "merriam_webster")
                 (:file "words")))
   ;; All Dictionaries and Thesaurus api
   (:module food
    :components ((:file "meal_db")))
   ;; All food-related Api
   (:module kg
    :components ((:file "catasto")
                 (:file "google_kg")
                 (:file "wikipedia_wikidata")))
   ;; All Knowledge Graph Api
   (:module news
    :components ((:file "mediastack")))
   ;; All news
   ))

