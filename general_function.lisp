;; Copyright 2020 Sony CSL Paris

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
                     ;;                              DRAFT                                   ;;
                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; This file explains step-by-step how to query an API to load Knowledge Graphs encoded in Json file
;; into Lisp List. The final goal will be building the bridge between the newly created Lisp List
;; and IRL.
;;
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
;;      Part 2.2.5: Query Spotify's API             | Function to query Spotify's API



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
;; Note: the &allow-other-keys parameter allows you to add keys in the specialized method.
;;
;; Step 2: We write a defmethod for the "default" case:
(defmethod query-knowledge-graph ((knowledge-graph t) ;; Any value allowed
                            &key &allow-other-keys)
  (error (format nil "Please implement a method DO-HTTP-REQUEST for the knowledge-graph ~a" knowledge-graph)))
;; (do-http-request :vua)


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

;; Before testing, please note several things :

;; First have a look at the http request you are about to send : https://kgsearch.googleapis.com/v1/entities:search?query=~a&key=~a&limit=~a&indent=True&languages=en&types=person
;; 1. https://kgsearch.googleapis.com/v1/entities:search? -- > this is the  minimal part you need to put when ;;
;;                                                             sending an HTPP request to the Google KG API
;; You can change the other parts (please note that they are separated by "&")
;; 2. query=~a --> define a character string to look for in Google Knowledge Graph. 
;; 3. key=~a --> Google API key.
;; 4. limit=~a --> A numeric value limiting the number of entities to be returned. The maximum is 500. Defaults to 20 
;;                 Please note that requests with high limits have a higher chance of timing out.
;; 5. indent=True --> A logical argument enabling indenting of JSON results. Defaults to NULL.
;; 6. languages=en --> A character argument defining the language filter. The list of language codes (defined
;;                    in ISO 639) to run the query with, for instance 'en', but it could be 'nl'.
;;                    Default to NULL.
;; 7. types=person --> A character argument restricting returned entities to those of the specified types. See
;;                     schema.org for valid types (e.g. 'Person' as defined in http://schema.org/Person restri
;;                     cts the results to entities representing people).
;;                     If multiple types are specified, returned entities will contain one or more of these types.
;;                    Defaults to NULL.  

;; If you are interested in digging into REST methods other than (:method :get) please check the section
;; "Learn Rest Basics" at https://developers.google.com/knowledge-graph/prereqs#learn-rest-basics

;; If you are interested in the different keys possible for the "http-request" method, please check the
;; The Drakma dictionary at https://edicl.github.io/drakma/#arg-want-stream at the section "Requests"

;; Now you are ready to test and to load Google's KG about Luc Steels as Lisp List ! 

;;(query-knowledge-graph :google :query "luc+steels" :api-key-google *api-key-google* :limit 1)

;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Part 2.2.2. Query Spotify's Knowledge Graph API
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; You can now specialize one for Spotify: 
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


