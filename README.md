# Web-Services

A Package to search for data in different open-access APIs in Common Lisp. 

## Contributors:

    Martina Galletti - martina.galletti@sony.com 
    Dr. Remi van Trijp - remi.vantrijp@sony.com

## Introduction 

The goal of this package is to interface Babel with different web services APIs. The different APIs can be queried using some specific functions which send a request to the APIs specified and encode the results into a Lisp list. 

The APIs accessible with this package are (for now) : 

      1. Dictionaries and Thesaurus:
        * 1.1. Words API (https://www.wordsapi.com/),
        * 1.2. Merriam-Webster Dictionary API (https://dictionaryapi.com/),
        * 1.3. Datamuse API (https://www.datamuse.com/api/).
         
      2. Domain Specific API: 
        * 2.1. For Food : Meal DB API (https://www.themealdb.com/api.php),
        * 2.2. For News : Mediastack (https://mediastack.com/).

      3. KG APIs: 
        * 3.1. MediaWiki API (https://www.mediawiki.org/wiki/API:Main_page), 
        * 3.2. Wikidata API (https://www.wikidata.org/wiki/Wikidata:Data_access),
        * 3.3. Google Knowledge Graph API (https://developers.google.com/knowledge-graph), 
        * 3.4. VUA Catasto Datastories (https://stories.datalegend.net), 

## Dependencies

This package depends on:

    * Drakma (can be installed using Quicklisp, or see https://edicl.github.io/drakma/)
    * Yason (can be installed using Quicklisp, or see https://github.com/phmarek/yason)
    * A running internet connection

## Installation 

### Getting your own API Keys

Please note, to use some functionalities of this package, you would need to have some API keys. This would be necessary to request data from the Words API, the Merriam-Webster Dictionary API, the MealDB API, the Mediastack API, and the Google Knowledge Graph APIs. For these APIs, you will have to specify your personal "API key" to send the request.  

For information about they can be obtained, please see :

       * For the Words API: https://www.wordsapi.com for the Words Dictionary API.

       * For the Merriam-Webster Dictionary: https://dictionaryapi.com/register/index

       * For the Mediastack API: https://mediastack.com/quickstart for the Mediastack API. 
         For using all the functionalities of this API, the free basic plan is not enough. 
         Please make sure you are using the Standard Plan or higher.

       * For the MealDB API:  https://www.themealdb.com/api.php for the MealDB API.

       * For the Google Knowledge Graph API: https://developers.google.com/knowledge-graph/how-tos/authorizing

Once you obtained them, you will need to copy-paste them in the "api_keys.lisp" file in the main directory of this package (at "set-api-keys") and save them.

### Getting Started

To use the package, you would need to : 
  
    * Open the "web-services.asd" file in your editor. 

    Start by evaluating (in-package :asdf). 
    If it works, you should see "=> #<The ASDF/INTERFACE package, 23/32 internal, 230/256 external>" 
    at the bottom of your screen.  

    Later evaluate the (defsystem :web-services). 
    If it works, you should see "=> #<ASDF/SYSTEM:SYSTEM "web-services">" 
    at the bottom of your screen

    * Open the "package.lisp" file in your editor. 

    Start by loading the dependencies of this package.
    You can do that by evaluating (ql:quickload :drakma), 
    (ql:quickload :yason), (ql:quickload :utils). 
    If it works, you should see respectively "=> (:DRAKMA)", "=> (:YASON)" and "=> (:UTILs)"
    at the bottom of your screen. 

    Secondly, evaluate (in-package :common-lisp-user). 
    If it works, you should see "=> #<The COMMON-LISP-USER package, 74/128 internal, 1/4 external>" 
    at the bottom of your screen.

    Finally, evaluate (defpackage :web-services).

    * Open the "start.lisp" file in your editor. 

    First, evaluate the (ql:quickload :web-services). 
    If it works, you should see ":=> (:WEB-SERVICES)"" at the bottom of your screen. 

    Secondly, evaluate (in-package :web-services).
    If it works, you should see "=> #<The WEB-SERVICES package, 105/128 internal, 18/64 external>"
    at the bottom of your screen. 

Now you are ready to go. You can try out the different functions available in the package.

## Functionalities

Here, you can find a description of the different functions available in the package and how you could use them with some specific examples. If you have suggestions or remarks, please feel free to contact us directly at martina.galletti@sony.com or remi.vantrijp@sony.com 

#### 1.Dictionaries and Thesaurus:

Relevant module: "dictionaries"

#####  1.1. Words API (https://www.wordsapi.com/docs/): 

It's an API for the English Language which enables you to find definitions, related words, and more, with a simple RESTful API. 

    * (request-words-api "singer") 
    Function to search for a particular token in Words API. 
    To search, just insert as an argument the token whose definition you are looking for. 
 
##### 1.2. Merriam-Webster Dictionary and Thesaurus API (https://www.dictionaryapi.com/products/json, https://dictionaryapi.com/products/api-collegiate-thesaurus):   

The Merriam-Webster Dictionary API gives developers access to a comprehensive resource of dictionary and thesaurus content as well as 
specialized medical, Spanish, ESL, and student-friendly vocabulary. 

    * (request-merriam-webster "collegiate" "singer") 
    Request a definition from the Merriam-Webster Collegiate dictionary 

##### 1.3. Datamuse API (http://www.datamuse.com/api/?ref=producthunt):
 
The Datamuse API is a word-finding query engine for developers. You can use it in your apps to find words that match a given set of constraints and that are likely in a given context. You can specify a wide variety of constraints on meaning, spelling, sound, and vocabulary in your queries, in any combination. Search all the tokens that rhyme with a certain token in Datamuse.

    * (request-datamuse :related-to "duck" :start-by "b*" :limit "2")
      Search all tokens semantically related to a particular token and start with a given letter in Datamuse. 
      Return a fixed number of results. 
     
    * (request-datamuse :rhyme "vice")
      Search all tokens semantically related to a particular token in Datamuse.
    
    * (request-datamuse :rhyme "vice" :related-to "food") 
      Search a token semantically related to another particular token in Datamuse.
    
    * (request-datamuse :frequent-adj "ocean")
      Search for adjectives mostly used with a particular token in Datamuse.
    
    * (request-datamuse :frequent-adj "ocean" :related-to "temperature")
       Search the adjectives mostly used with a particular token-1 and semantically related to another token-2 in Datamuse.
    
    * (request-datamuse :frequent-noun "yellow")
       Search the most used nouns with a particular adjective in Datamuse
 
     * (request-datamuse :spelled-similarly "yellow")
       Search the most words spelled similarly to another one in Datamuse
   
    * (request-datamuse :frequent-follow "drink")
      Search all the tokens that more likely follow a token-1 in Datamuse
    
    * (request-datamuse :frequent-follow "drink" :start-by "w*")
       Search for all the tokens that more likely follow a word-1 and start with a particular letter in Datamuse.


### 2. Open access APIs related to a certain area: 

#### 2.1. Food : Meal DB API (https://www.themealdb.com/api.php)

Relevant module: "food".
The MealDB API is an open, crowd-sourced database of Recipes from around the world.

    * (request-mealDB "Carbonara")
      Search for a Recipe in the MealDB
    
    * (request-mealDB-filter :ingredient "Tuna")
      Search for a recipe in the MealDB with an ingredient X
    
    * (request-mealDB-filter :category "Seafood")
      Search for all the meals in a particular category
    
    * (request-mealDB-filter :country "Italian")
      Search for all the meals traditional of a particular country

    * (request-mealDB-hungry-random) 
     Search for random recipes to have some inspiration        

#### 2.2. News : Mediastack API (https://mediastack.com/documentation)

Relevant module: "news".
The Mediastack API is a Free, Simple REST API for Live News & Blog Articles. It offers scalable JSON API delivering worldwide news, headlines and blog articles in real-time

    * (request-Mediastack-live-news "Barcelona" :categories "sport" :languages "it" :countries "it" :limit "2" :sort "published_asc") 
      Search for all the News related to the Barcelona football team published across Italian news outlets 
      in the category sport and which has as language Italian. 
      For efficiency, you can limit your request to a certain number of results 
      (for example, here the :limit is set at 2).

    * (request-Mediastack-historical-news "Barcelona" :date "29-01-20" :sources "cnn" :categories "sport" :countries "us" :languages "en" :limit "2":sort "published_asc")
     Search for all the News related to the Barcelona football team published by CNN at a certain date (here 29 January 2021) 
     in the category sport and which has as language English. 
     For efficiency, you can limit your request to a certain number of results. 


### 3. KG APIs :

Relevant module: "kg"

#### 3.1. MediaWiki API (https://www.mediawiki.org/wiki/API:Main_page): 

The MediaWiki Action API is a web service that allows access to some wiki-features like authentication, page operations, and search. It can provide meta information about the wiki and the logged-in user. 

    * (search-wikipedia "Steve McQueen")
      Function to search for a token in Wikipedia. To query just insert as an argument the string you are looking for. 
 
#### 3.2. Wikidata API (https://www.wikidata.org/wiki/Wikidata:WikiProject_Documentation):

Wikidata is a free and open knowledge base that can be read and edited by both humans and machines. Wikidata acts as central storage for the structured data of its Wikimedia sister projects including Wikipedia, Wikivoyage, Wiktionary, Wikisource, and others. 

    * (search-entity-in-wikidata "Steve McQueen" ) 
      Function to search for a token in Wikidata. To use it, just insert as an argument the string you are looking for. 
    
    * (get-wikidata-entity "Q159347")
      Function to get all statements for a particular URI in Wikidata. 
      To use it, just insert as an argument the URI you are looking for. 
      For more information about URI in Wikidata, please see https://www.wikidata.org/wiki/Help:Statements 
    
#### 3.3. Google Knowledge Graph API (https://developers.google.com/knowledge-graph): 

The Knowledge Graph Search API lets you find entities in the Google Knowledge Graph. The API uses standard schema.org types and is compliant with the JSON-LD specification.

    * (request-google-knowledge-graph "Steve McQueen" :types "person")
      Function to search a token in the Google Knowledge Graph APIs while specifying the type. 
      To use it, just insert as an argument the string you are looking for.

#### 3.4. Catasto Datastories (https://stories.datalegend.net/catasto/): 

Data that are accessible in the linked data version of the Florentine Catasto of 1427 available on Druid. 

    * (request-catasto)
       Function to access a saved SPARQL query on the Catasto dataset. 
       The endpoint to call is shown under "API Variable".
