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

(export '(request-mealDB request-mealDB-search request-mealDB-filter
                         request-mealDB-categories request-mealDB-random-meal
                         request-mealDB-lookup request-mealDB-list))

;; ------------------------------------------------------------------------------------------------------------
;; MealDB
;; https://www.themealdb.com/api.php
;; ------------------------------------------------------------------------------------------------------------

(defun request-mealDB (api-method &key parameters additional-headers)
 "Search for a Recipe in the MealDB"
 (let ((uri (format nil "https://www.themealdb.com/api/json/v1/~a/~a.php" (get-api-key :mealdb) api-method)))
    (request-api uri
                 :parameters parameters
                 :additional-headers additional-headers)))

(defun request-mealDB-search (name-or-first-letter &key parameters additional-headers)
  "Search for a meal by name or first letter."
  (assert (stringp name-or-first-letter))
  (request-mealdb "search" :parameters `(,@(when name-or-first-letter
                                             (if (= 1 (length name-or-first-letter))
                                               `(("f" . ,name-or-first-letter))
                                               `(("s" . ,name-or-first-letter))))
                                         ,@parameters)
                  :additional-headers additional-headers))

(defun request-mealDB-categories ()
  "Return all categories of MealDB."
  (request-mealDB "categories"))

(defun request-mealdb-random-meal ()
  "I'm feeling hungry and lucky."
  (request-mealDB "random"))

(defun request-mealdb-lookup (id)
  "Lookup a meal by ID."
  (assert (stringp id))
  (request-mealDB "lookup" :parameters `(("i" . ,id))))

(defun request-mealDB-filter (&key ingredients category country parameters additional-headers)
 "Search for a Recipe in the MealDB on the basis of some constraints"
 (request-mealDB "filter" :parameters `(,@(cond
                                           (ingredients `(("i" . ,(if (stringp ingredients)
                                                                   (clean-request ingredients "_")
                                                                   (format nil "~{~a~^,~}" (mapcar #'(lambda(x)
                                                                                                       (clean-request x "_"))
                                                                                                   ingredients))))))

                                           (category `(("c" . ,category)))
                                           (country `(("a" . ,country)))
                                           (t
                                            nil))
                                        ,@parameters)
                 :additional-headers additional-headers))

;; (request-mealdb-filter :ingredients '("salt" "garlic")) ;; Doesn't work? Problem with the comma?

(defun request-mealDB-list (categories-countries-or-ingredients)
  "Request a list of all categories, countries or ingredients."
  (request-mealdb "list" :parameters `((,(case categories-countries-or-ingredients
                                           (category "c")
                                           (categories "c")
                                           (country "a")
                                           (countries "a")
                                           (ingredient "i")
                                           (ingredients "i")
                                           (t
                                            (error "Please specify whether you want to list categories, countries or ingredients.")))
                                        . "list"))))


