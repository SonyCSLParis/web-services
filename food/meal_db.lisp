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
(export '(request-mealDB request-mealDB-filter request-mealDB-hungry-random))
;; ------------------------------------------------------------------------------------------------------------
;; MealDB
;; ------------------------------------------------------------------------------------------------------------

(defun request-mealDB (meal &optional (api-key "1"))
 "Search for a Recipe in the MealDB"
  (let ((url (format nil "https://www.themealdb.com/api/json/v1/1/search.php?s=~a" meal api-key)))
    (request-api url)))

(defun request-mealDB-filter (&key (api-key "1") ingredient category country)
 "Search for a Recipe in the MealDB on the basis of some constraints"
  (let ((url (format nil "https://www.themealdb.com/api/json/v1/1/filter.php?s=~a" api-key)))
    (request-api url
                 :parameters `(("api-key" . ,api-key)
                              ,@(when ingredient `(("i" . ,ingredient)))
                              ,@(when category `(("c" . ,category)))
                              ,@(when country `(("a" . ,country)))))))

(defun request-mealDB-hungry-random(&optional (api-key "1"))
  "I am hungy, what should I eat tonight?"
  (let ((url (format nil "https://www.themealdb.com/api/json/v1/1/random.php")))
    (request-api url)))

