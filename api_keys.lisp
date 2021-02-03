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

;; Please replace these API keys with your own:
(set-api-keys :google-knowledge-graph "AIzaSyDR9MVg0_Zh6QrKD3M7SzZqQJ9Tn9I7GxY"
              :merriam-webster "8fe92f45-0f31-4ec1-8b3f-c11cb403d657"
              :words "ef0b0b01fbmshe99d52e360999bcp116ad7jsn90dcbb775019"
              :mediastack "357acde6d8d40889c97558fc6581649e"
              :DBMeal "1")


;; Old: to be replaced:
;; ---------------------
(defparameter *api-key-google* nil "Set your own Google Knowledge Graph API key.")
(setf *api-key-google* "AIzaSyDR9MVg0_Zh6QrKD3M7SzZqQJ9Tn9I7GxY") ;; Please insert here your API key

(defparameter *api-key-merriam* nil "Set your own Merriam-Webster API key.")
(setf *api-key-merriam* "8fe92f45-0f31-4ec1-8b3f-c11cb403d657") ;; Please insert here your API key

(defparameter *api-key-words* nil "Set your own Words Dictionary API key.")
(setf *api-key-words* "ef0b0b01fbmshe99d52e360999bcp116ad7jsn90dcbb775019") ;; Please insert here your API key

(defparameter *api-key-mediastack* nil "Set your own Mediastack API key.")
(setf *api-key* "357acde6d8d40889c97558fc6581649e") ;; Please insert here your API key

(defparameter *api-key-meal* nil "Set your own MealDB API key.")
(setf *api-key-meal* "1") ;; Please insert here your API key
