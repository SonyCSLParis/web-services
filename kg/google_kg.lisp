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
(export 'request-google-knowledge-graph)

;; --------------------------------------------------------------------------------------------------------------------------------------------------
;; Google KG API
;; --------------------------------------------------------------------------------------------------------------------------------------------------

(defun request-google-knowledge-graph (request &key (api-key *api-key-google*) ;; Use your own API key
                                           indent limit languages types)
  "Search a token in the Google Knowledge Graph APIs"
  (let ((cleaned-request (regex-replace-all " " request "+")))
    (request-api "https://kgsearch.googleapis.com/v1/entities:search?"
               :parameters `(("query" . ,cleaned-request)
                             ("key" . ,api-key)
                             ,@(when indent `(("indent" . ,indent)))
                             ,@(when limit `(("limit" . ,limit)))
                             ,@(when languages `(("languages" . ,languages)))
                             ,@(when types `(("types" . ,types)))))))