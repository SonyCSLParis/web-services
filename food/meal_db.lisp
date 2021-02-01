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
;; MealDB
;; ------------------------------------------------------------------------------------------------------------

(defun request-mealDB (meal &optional (api-key "1"))
 "Search for a Recipe in the MealDB"
  (let ((url (format nil "https://www.themealdb.com/api/json/v1/1/search.php?s=~a" (clean-request meal) api-key)))
    (request-api url)))

(defun request-mealDB-ingredient(ingredient &optional (api-key "1"))
  "Search for a recipe in the MealDB with a main ingredient X"
  (let ((url (format nil "https://www.themealdb.com/api/json/v1/1/filter.php?i=~a" (clean-request ingredient))))
    (request-api url)))

(defun request-mealDB-category-filter(category &optional (api-key "1"))
  "Search for all the meals in a particular category"
  (let ((url (format nil "https://www.themealdb.com/api/json/v1/1/filter.php?c=~a" (clean-request category))))
    (request-api url)))

(defun request-mealDB-country-filter(country &optional (api-key "1"))
  "Search for all the meals traditional of a particular country"
  (let ((url (format nil "https://www.themealdb.com/api/json/v1/1/filter.php?a=~a" (clean-request country))))
    (request-api url)))

(defun request-mealDB-hungry-random(&optional (api-key "1"))
  "I am hungy, what should I eat tonight?"
  (let ((url (format nil "https://www.themealdb.com/api/json/v1/1/random.php")))
    (request-api url)))
