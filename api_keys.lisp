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
(in-package :web-services)

(defparameter *api-keys* nil "List you API-keys here.")

(defun store-api-key (name key &key (store *api-keys*))
  (let ((new-key (list name key))
        (old-key (assoc name store))) ;; Check whether one already exists
    ;; If so, substitute it:
    (if old-key
      (setf store (substitute new-key old-key store :test #'equal))
      ;; If not, simply cons it:
      (setf store (cons new-key store)))
    store))

(defun retrieve-api-key (name key &key (store *api-keys*) key)
  (let ((key (assoc name store)))
             (assert key)
    key))

(store-api-key :google "AIzaSyDR9MVg0_Zh6QrKD3M7SzZqQJ9Tn9I7GxY") ;; Martina's API key Google 
(store-api-key :merriam  "8fe92f45-0f31-4ec1-8b3f-c11cb403d657") ;; Remi's API key Merriam Webster
(store-api-key :words "ef0b0b01fbmshe99d52e360999bcp116ad7jsn90dcbb775019") ;; Remi's API key words
(store-api-key :meal "1") ;; test API key meal DB 
(store-api-key :mediastack "357acde6d8d40889c97558fc6581649e") ;; Martina's API key mediastack

;;test
(retrieve-api-key :google *api-keys*)


