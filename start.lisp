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

(ql:quickload :web-services)
(in-package :web-services)

;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; 1. Introduction and Installation.
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; The goal of this package is to interface Babel with different web services APIs. The different APIs can be queried using some specific functions which send a request to the APIs specified and encode the results into a Lisp list.

;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; 1. Getting Started
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; To use the package, you would need to : 
  
    ;; -  Open the "web-services.asd" file in your editor and evaluate it, starting by (in-package :asdf) and later by (defsystem :web-services).

    ;; -  open the "package.lisp" file in your editor and evaluate it, making sure to starting by loading the dependencies of this packages by evaluate (ql:quickload :drakma) and (ql:quickload :yason).

    ;; Please note, in order to use some functionalities of this package, you would need to have some API keys. This would be necessary to request data from the Words API, the Merriam-Webster Dictionary API, the MealDB API, the Mediastack API and the Google Knowledge Graph APIs. For these APIs you will you have to specify your personal "API key" to send the request.

;; For information about they can be obtained, please see :

       ;; For the Words API: https://www.wordsapi.com for the Words Dictionary API.

       ;; For the Merriam-Webster Dictionary: https://dictionaryapi.com/register/index

       ;; For the Mediastack API: https://mediastack.com/quickstart for the Mediastack API.

       ;; For the MealDB API:  https://www.themealdb.com/api.php for the MealDB API.

       ;; For the Google Knowledge Graph API: https://developers.google.com/knowledge-graph/how-tos/authorizing

;; Once you obtained them, you need to copy paste them in the api_keys.lisp file that you can find in the main directory and save your edits.

;; Now you are ready to go.
;; You can start by evaluating the different example functions contained in the start.lisp file, such as (request-words-api "singer") for example, and try to play around and change the different request you want to make. 

;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; 2. Functionalities
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; Here, you can find a description of the different functions available in the package and how you could use them with some specific examples. If you comments, remarks, please feel free to reach out to martina.galletti@sony.com

;; Please replace these API keys with your own:
(set-api-keys :google-knowledge-graph "AIzaSyDR9MVg0_Zh6QrKD3M7SzZqQJ9Tn9I7GxY"
              :merriam-webster "8fe92f45-0f31-4ec1-8b3f-c11cb403d657"
              :words "ef0b0b01fbmshe99d52e360999bcp116ad7jsn90dcbb775019"
              :mediastack "357acde6d8d40889c97558fc6581649e"
              :MealDB "1")

;; -------------------------------------------------------------------------
;; 3.1. Some example of searches for dictionaries and thesaurus
;; -------------------------------------------------------------------------


;; 3.1.1. Words API (https://www.wordsapi.com/docs/)

(request-words-api "singer") ;; request a definition from the words API

;; 3.1.2. Merriam Webster APIs (https://www.dictionaryapi.com/products/json, https://dictionaryapi.com/products/api-collegiate-thesaurus)

(request-merriam-webster "collegiate" "singer") ;;request a definition from the Merriam-Webster Collegiate dictionary 


;; 3.1.3. Datamuse API (http://www.datamuse.com/api/?ref=producthunt)

(request-datamuse :related-to "duck" :start-by "b*" :limit "2") ;;  Search all tokens semantically related to a particular token and start by a given letter in Datamuse. Return a fixed number of results.  

(request-datamuse :rhyme "vice") ;; Search all tokens semantically related to a particular token in Datamuse.

(request-datamuse :rhyme "vice" :related-to "food") ;; Search a token semantically related to another particular token in Datamuse.

(request-datamuse :frequent-adj "ocean") ;; Search for adjectives mostly used with a particular token in Datamuse.

(request-datamuse :frequent-adj "ocean" :related-to "temperature") ;; Search the adjectives mostly used with a particular token-1 and semantically related to another token-2 in Datamuse.

(request-datamuse :frequent-noun "yellow") ;; Search the most used nouns with a particular adjective in Datamuse

(request-datamuse :spelled-similarly "yellow") ;; Search the most words spelled similarly to another one in Datamuse

(request-datamuse :frequent-follow "drink") ;; Search all the tokens that more likely follow a token-1 in Datamuse

(request-datamuse :frequent-follow "drink" :start-by "w*") ;; Search  all the tokens that more likely follow a word-1 and start by a particular letter in Datamuse.


;; -------------------------------------------------------------------------------------------------------------
;; 3.2. Some example of searches for APIs related to a certain area
;; -------------------------------------------------------------------------------------------------------------


;; 3.2.1. MealDB API (https://www.themealdb.com/api.php)

(request-mealDB-s "Carbonara") ;; Search for a Recipe in the MealDB

(request-mealDB-filter :category "Seafood") ;; Search for all the meals in a particular category

(request-mealDB-filter :country "Italian") ;;Search for all the meals traditional of a particular country

(request-mealDB-random-meal) ;; Search for a random recipes to have some inspiration 


;; 3.2.2. Mediastack API (https://mediastack.com/documentation)

(request-Mediastack-live-news "Barcelona" :categories "sport" :languages "it" :countries "it" :limit "2" :sort "published_asc") ;; it won't work for now because we have a free plan --> upgrade to use it
;; Search for all the News related to the Barcelona footbal team published across Italian news outlets in the category sport and which has as language italian. For efficiency, you can limit your request to a certain number of results (for example, here the :limit is set at 2).
(request-Mediastack-historical-news "Barcelona" :date "29-01-21" :sources "cnn" :categories "sport" :countries "us" :languages "en" :limit "2":sort "published_asc") ;; it won't work for now because we have a free plan --> upgrade to use it.
;; Search for all the News related to the Barcelona footbal team published by the CNN at a certain date (here 29 January 2021) in the category sport and which has as language english. For efficiency, you can limit your request to a certain number of results. 


;; -------------------------------------------------------------------------------------------------------------
;; 3.3. Some example of searches for KG APIs
;; -------------------------------------------------------------------------------------------------------------


;; 3.3.1. MediaWiki API (https://www.wikidata.org/wiki/Wikidata:WikiProject_Documentation)

(request-wikipedia "Steve McQueen") ;; Function to search for a token in Wikipedia. To query just insert as an argument the string you are looking for. 


;; 3.3.2. Wikidata API (https://www.wikidata.org/wiki/Wikidata:WikiProject_Documentation)

(request-wikidata-entity "Steve McQueen" :language "en") ;; Function to search for a token in Wikidata. To use it, just insert as an argument the string you are looking for. 

(request-wikidata-URI "Q159347") ;; Function to get all statements for a particular URI in Wikidata. To use it, just insert as an argument the URI you are looking for. For more information about URI in Wikidata, please see https://www.wikidata.org/wiki/Help:Statements 


;; 3.3.3. Google Knowledge Graph API (https://developers.google.com/knowledge-graph)

(request-google-knowledge-graph "Steve McQueen" :types "person") ;;Function to search a token in the Google Knowledge Graph APIs while specifying the type. To use it, just insert as an argument the string you are looking for.


;; 3.3.4. Catasto Datastories (https://stories.datalegend.net/catasto/)

(request-catasto) ;; Function to access a saved SPARQL query on the Catasto dataset. The endpoint to call is shown under "API Variable".
