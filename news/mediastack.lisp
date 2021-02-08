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

;; ------------------------------------------------------------------------------------------------------------
;; Mediastack
;; ------------------------------------------------------------------------------------------------------------

(defun request-Mediastack-live-news (search &key categories countries languages limit sort)
  "Search for Live News in the Mediastack APIs"
  (request-api "http://api.mediastack.com/v1/sources/?access_key=~a"
               :parameters `(("keywords" . ,search)
                             ("access_key" . ,(get-api-key :mediastack))
                             ,@(when categories `(("categories" . ,categories)))
                             ,@(when languages `(("languages" . ,languages)))
                             ,@(when countries `(("countries" . ,countries)))
                             ,@(when limit `(("limit" . ,limit)))
                             ,@(when sort `(("sort" . ,sort))))))


(defun request-Mediastack-historical-news (search &key date sources categories countries languages limit sort)
  "Search for historical news in the Mediastack APIs"
  (request-api "http://api.mediastack.com/v1/sources/?access_key=~a"
               :parameters `(("keywords" . ,search)
                              ("access_key" . ,(get-api-key :mediastack))
                             ,@(when date `(("date" . ,date)))
                             ,@(when sources `(("sources" . ,sources)))
                             ,@(when categories `(("categories" . ,categories)))
                             ,@(when countries `(("countries" . ,countries)))
                             ,@(when languages `(("languages" . ,languages)))
                             ,@(when limit `(("limit" . ,limit)))
                             ,@(when sort `(("sort" . ,sort)))))) ;; it won't work for now because we have a free plan --> upgrade to use it. 
