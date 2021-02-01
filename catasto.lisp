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

;; -----------------------------------------------------------------------------------------------------------
;; Catasto
;; -----------------------------------------------------------------------------------------------------------

(defun request-catasto (&key (content-type "application/json"))
  "Function to search in the Catasto knowledge graph API, given a saved SPARQL query"
  (let* ((http-request (format nil "https://api.druid.datalegend.net/queries/muhaiuser/example-query/run"))
         (stream (drakma:http-request http-request :method :get :content-type content-type 
                                      :want-stream t)))
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8) 
    (yason:parse stream :object-as :alist)))
