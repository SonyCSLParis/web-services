;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Babel package to access APIs
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; Copyright 2021 Sony CSL Paris

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;=========================================================================


                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                     ;;                        by  Remi, Martina                             ;;
                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; This file explains step-by-step the content of the package to query different APIs to load Knowledge Graphs encoded in Json file into Lisp List. 
;;
;; Content:                                         | What's discussed
;; -------------------------------------------------+------------------------------------------------------
;; Part 1: Load the packages                        | Use Quicklisp to load the packages
;; Part 2: Query an API Load the KG as a Lisp List  | How to query APIs to load data into a Lisp list
;;   Part 2.1: Getting access to the APIs           | Defining how to access each APIs 
;;      Part 2.1.1: Getting Google's API access     | Getting a key's API for Google's KG API
;;      Part 2.1.2: Getting Spotify's API access    | Getting a token's API for Spotify's KG API
;;   Part 2.2: The Generic Function                 | Defining a generic function to query all APIs
;;      Part 2.2.1: Query Google's KG API           | Function to query Google's KG API
;;      Part 2.2.2: Query VUA's KG API              | Function to query Google's KG API
;;      Part 2.2.3: Query Wikidata's KG API         | Function to query Google's KG API
;;      Part 2.2.3.1: Get Descriptions              | Get Wikidata Descriptions
;;      Part 2.2.3.2: Get statements                | Get Wikidata Statements
;;      Part 2.2.4: Query Spotify's API             | Function to query Spotify's API



;; #######################################################################################################
;; Part 1. Load the packages needed
;; #######################################################################################################

(ql:quickload '(:drakma :yason))
(ql:quickload :irl)
(in-package :irl)


;; #########################################################################################################
;; Part 2. Query an API (Google, Spotify)
;; and decode the KG (a Json file) into Lisp as a list
;; #########################################################################################################

;; ----------------------------------------------------------------------------------------------------------
;; 2.1. Getting and Storing your Google's Knowledge Graph API
;; ----------------------------------------------------------------------------------------------------------

;; The first thing you need to do to access Google's Knowledge Graph API is to get a Google API key.
;; How?
;; 1. Visit this page https://developers.google.com/knowledge-graph/prereq.
;; 2. Goes to the section "Create a project for your client"
;; 3. Click on the link "use the setup tool".
;; 4. Log in to your Google account
;; 5. Create a project and get a unique API Key on the "credentials section".
;; 6. Please note the "restrictions" section  -->  You can restrict the key before using it in production by
;;                                                 clicking the Restrict key and selecting one of the Restriction
;;                                                 To keep your API keys secure, follow the best practices at
;;                                                 this page https://cloud.google.com/docs/authentication/api-key 

(defparameter *api-key-google* nil "Set your own Google Knowledge Graph API key.")
(setf *api-key-google* "") ;; Please insert here your API key

;; ----------------------------------------------------------------------------------------------------------
;; 2.1.5. Getting and Storing your Spotify's Knowledge Graph Key 
;; ----------------------------------------------------------------------------------------------------------

;; The same as for Google's API, a number of steps are required to get a Spotify API's credentials
;; 1. Start by creating a Spotify user account (Premium or Free).
;;    To do that, simply sign up at www.spotify.com.
;; 2. When you have a user account, go to the Dashboard page (https://developer.spotify.com/dashboard/login)
;;    at and log in. Accept the latest Developer Terms of Service to complete your account set up.
;; 3. Click on "Create an APP" and register your application.
;; 4. Click on your new APP and you will see "Client ID" and "Client Secret"
;; 5. Copy  and load these two codes below.

(defparameter *client-id* nil "Set your own Spotify Knowledge Graph API Client ID.")
(defparameter *client-secret* nil "Set your own Spotify Knowledge Graph API Client Secret.")

(setf *client-id* "") ;; 
(setf *client-secret* "") ;; 


;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Part 2.2. : A generic function for all APIs 
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defgeneric query-knowledge-graph (knowledge-graph &key &allow-other-keys))
;;
(defmethod query-knowledge-graph ((knowledge-graph t) 
                            &key &allow-other-keys)
  (error (format nil "Please implement a method DO-HTTP-REQUEST for the knowledge-graph ~a" knowledge-graph)))

;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Part 2.2.1. Query Google's Knowledge Graph API
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defmethod query-knowledge-graph ((knowledge-graph (eql :google))
                            &key (query "luc+steels") (limit 1)
                            (api-key-google *api-key-google*) (content-type "application/json"))
    (let* ((http-request (format nil "https://kgsearch.googleapis.com/v1/entities:search?query=~a&key=~a&limit=~a&indent=True&languages=en&types=person"
                               query api-key-google limit)) ;; define your http request 
         (stream (drakma:http-request http-request :method :get :content-type content-type 
                                      :want-stream t))) ;; use drakma and the REST method get to get the KG 
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8) ;; 
    (yason:parse stream :object-as :alist)))

;; Now you are ready to test and to load Google's KG about Luc Steels as Lisp List ! 

(query-knowledge-graph :google :query "luc+steels" :api-key-google *api-key-google* :limit 1)

;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Part 2.2.2. Query VUA's Knowledge Graph API
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; username "muhaiuser" password "muhai2021"

;; Log in, make a SPARQL query and save it by clicking on the disk on the top right of the editor.
;; Your query will be stored here : https://druid.datalegend.net/muhaiuser/-/queries/ .
;; Once it is saved, you can access it directly from your code via REST calls
;; See the saved example query Ilaria made : https://druid.datalegend.net/muhaiuser/-/queries/example-query/
;; (the API to call is shown under "API Variable", i.e.  https://api.druid.datalegend.net/queries/muhaiuser/example-query/run )

(defmethod query-knowledge-graph ((knowledge-graph (eql :vua))
                            &key (content-type "application/json"))
    (let* ((http-request (format nil "https://api.druid.datalegend.net/queries/muhaiuser/example-query/run" limit))  
         (stream (drakma:http-request http-request :method :get :content-type content-type 
                                      :want-stream t))) ;; use drakma and the REST method get to get the KG 
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8) ;; 
    (yason:parse stream :object-as :alist)))

(defvar test (query-knowledge-graph :vua))

test

;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Part 2.2.3. Query Wikidata's Knowledge Graph API
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; -----------------------------------------------------------------------------------------------------------
;; Part 2.2.3.1. Search by URI and get descriptions
;; -----------------------------------------------------------------------------------------------------------

(defmethod query-knowledge-graph ((knowledge-graph (eql :wikidata-1))
                            &key (ids "Q159347") (content-type "application/json")) ;; specify URIs
    (let* ((http-request (format nil "https://www.wikidata.org/w/api.php?action=wbgetentities&languages=en&ids=~a&format=json&props=descriptions" ids)) ;; wikidata APIs, we specify for the description
         (stream (drakma:http-request http-request :method :get :content-type content-type 
                                      :want-stream t))) ;; use drakma and the REST method get to get the KG 
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8) 
    (yason:parse stream :object-as :alist)))

(defvar descriptions (query-knowledge-graph :wikidata-1 :ids "Q159347"))

;; test
descriptions

;; -----------------------------------------------------------------------------------------------------------
;; Part 2.2.3.2. Search by URI and get all the statements
;; -----------------------------------------------------------------------------------------------------------

(defmethod query-knowledge-graph ((knowledge-graph (eql :wikidata-2))
                            &key (entity "Q7186") (content-type "application/json")) ;; specify URIs
    (let* ((http-request (format nil "https://www.wikidata.org/w/api.php?action=wbgetclaims&entity=~a&format=json&property=P106" entity)) ;; wikidata APIs, notice the changes in entity,action and property
         (stream (drakma:http-request http-request :method :get :content-type content-type 
                                      :want-stream t))) ;; use drakma and the REST method get to get the KG 
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8) 
    (yason:parse stream :object-as :alist)))

;;encode results
(defvar statements (query-knowledge-graph :wikidata-2 :entity "Q7186"))

;;test
statements

;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Part 2.2.4. Query Spotify's Knowledge Graph API
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                     ;;                       Work in progress                               ;;
                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod query-knowledge-graph-spotify ((knowledge-graph (eql :spotify))
                            &key (query "hey you") 
                                 (search-type "track")
                                 (limit 3)
                                 (content-type "application/json"))
   (let* ((http-request (format nil "https://accounts.spotify.com/api/token?") ;; define your http request 
          (stream (drakma:http-request http-request :method :post :content-type content-type :parameters '(("grant_type" . "client_credentials"))
                                       :additional-headers '(("Authorization" . "Basic ZWY0ZDI0Y2UwYTk1NGZlNzgxODVjY2Y3ODA2MDczZDQ6N2FmZmQwMzgzZDZmNDBmY2EzMWI5ZTExZjc2Nzc1MGM=")
                                                             :want-stream t))))) ;; use drakma and the REST method get to get the KG 
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8) 
    (yason:parse stream :object-as :alist))
    (let* ((http-request-get (format nil "https://api.spotify.com/v1/search"
                               query limit)) ;; define your http request 
         (stream (drakma:http-request http-request-get :method :get :content-type content-type 
                                      :want-stream t))) ;; use drakma and the REST method get to get the KG 
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8) 
    (yason:parse stream :object-as :alist)))

(query-knowledge-graph-spotify :spotify :query "hey you" :search-type "track" :limit 3)


;;(drakma:http-request :method :post
           ;;          :parameters '(("grant_type" . "client_credentials"))
           ;;          :additional-headers '(("Authorization" . "Basic ZWY0ZDI0Y2UwYTk1NGZlNzgxODVjY2Y3ODA2MDczZDQ6N2FmZmQwMzgzZDZmNDBmY2EzMWI5ZTExZjc2Nzc1MGM=")))


