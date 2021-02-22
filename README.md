# Web-Services

A Package to search for particular entities or URI in different open access APIs in Common Lisp. 

The APIs accessible with this package are : 

      1. Dictionaries and Thesaurus:
        * 1.1. Words API (https://www.wordsapi.com/),
        * 1.2. Merriam-Webster Dictionary API (https://dictionaryapi.com/)
        * 1.3. Datamuse API (https://www.datamuse.com/api/).
         
      2. Domain Specific API: 
        * 2.1. For Food : Meal DB API (https://www.themealdb.com/api.php),
        * 2.2. For News : Mediastack (https://mediastack.com/).

      3. KG APIs: 
        * 3.1. MediaWiki API(https://www.mediawiki.org/wiki/API:Main_page), 
        * 3.2. Wikidata API(https://www.wikidata.org/wiki/Wikidata:Data_access),
        * 3.3. Google Knowledge Graph API (https://developers.google.com/knowledge-graph), 
        * 3.4. VUA Catasto Datastories (https://stories.datalegend.net), 

## Contributors:

    Martina Galletti - martina.galletti@sony.com 
    Dr. Remi van Trijp - remi.vantrijp@sony.com

    
## Introduction and Installation

The goal of this package is to interface Babel with different web services APIs. The different APIs can be queried using some specific functions which send a request to the APIs specified and encode the results into a Lisp list. 

### Dependencies

To use the package, you would need to install the three dependencies of this package:

  * Drakma (can be installed using Quicklisp, see https://edicl.github.io/drakma/)
  * Yason (can be installed using Quicklisp, see https://github.com/phmarek/yason)
  * Cl-ppcre (can be installed using Quicklisp, see http://edicl.github.io/cl-ppcre/)

### Getting your own API Keys

The different APIs can be queried using some specific helper functions which send a request to the APIs specified and encode the results into a Lisp list. 

Please note, in order to use some functionalities of this package, you would need to have some API keys. This would be necessary to request data from the Words API, the Merriam-Webster Dictionary API, the MealDB API, the Mediastack API and the Google Knowledge Graph APIs. For these APIs you will you have to specify your personal "API key" to send the request. 

For information about they can be obtained, please see :

For the Words API: https://www.wordsapi.com for the Words Dictionary API.

       * For the Merriam-Webster Dictionary: https://dictionaryapi.com/register/index

       * For the Mediastack API: https://mediastack.com/quickstart for the Mediastack API.

       * For the MealDB API:  https://www.themealdb.com/api.php for the MealDB API.

       * For the Google Knowledge Graph API: https://developers.google.com/knowledge-graph/how-tos/authorizing

Once you obtained them, you need to copy paste them in the api_keys.lisp file that you can find in the main directory and evaluate them by pressing ctrl+x+e.

## Functionalities

Here, you can find a description of the different functions available in the package and how you could use them with some specific examples. If you comments, remarks, please feel free to reach out to martina.galletti@sony.com

#### 1.Dictionaries and Thesaurus:

#####  1.1. Words API (https://www.wordsapi.com/docs/): 

It's an API for the English Language which enables you to find definitions, related words, and more, with a simple RESTful API. 

    * (request-words-api "singer") 

      function to search for a particular token in Words API. To search, just insert as argument the token whose definition you are looking for. 
 
##### 1.2. Merriam-Webster Dictionary and Thesaurus API (https://www.dictionaryapi.com/products/json, https://dictionaryapi.com/products/api-collegiate-thesaurus):   

The Merriam-Webster Dictionary API gives developers access to a comprehensive resource of dictionary and thesaurus content as well as 
specialized medical, Spanish, ESL, and student-friendly vocabulary. 

    * (request-merriam-webster-dictionary "singer") 
      function to search for a particular token in Merriam-Webster Collegiate Dictionary API. To search, just insert as argument the token whose definition you are looking for. 
    
    * (request-merriam-webster-thesaurus "singer") 
      function to search for a particular token in the Merriam-Webster Collegiate Thesaurus API. To search, just insert as argument the token whose definition you are looking for. 

    * (request-merriam-webster-spanish-dictionary "casa") 
      function to search for a particular token in the Merriam-Webster in the Spanish Dictionary API. To search, just insert as argument the token whose definition you are looking for. 

    * (request-merriam-webster-doctor-dictionary "doctor") 
      function to search for a particular token in the Merriam-Webster in the Medical Dictionary API. To search, just insert as argument the token whose definition you are looking for. 

    * (request-merriam-webster-learners-dictionary "singer") 
      function to search for a particular token in the Merriam-Webster Learners Dictionary API. To search, just insert as argument the token whose definition you are looking for. 

    * (request-merriam-webster-elementary-dictionary "singer") 
      function to search for a particular token in the Merriam-Wbster Elementary Dictionary API. To search, just insert as argument the token whose definition you are looking for. 

    * (request-merriam-webster-intermediate-thesaurus "singer") 
      function to search for a particular token in the Merriam-Webster Intermediate Thesaurus API. To search, just insert as argument the token whose definition you are looking for. 

    * (request-merriam-webster-school-dictionary "singer") 
      function to search for a particular token in the Merriam-Webster School Dictionary API. To search, just insert as argument the token whose definition you are looking for. 
    
##### 1.3. Datamuse API (http://www.datamuse.com/api/?ref=producthunt):
 
The Datamuse API is a word-finding query engine for developers. You can use it in your apps to find words that match a given set of constraints and that are likely in a given context. You can specify a wide variety of constraints on meaning, spelling, sound, and vocabulary in your queries, in any combination. Search all the tokens that rhyme with a certain token in Datamuse.

    * (request-datamuse :related-to "duck" :start-by "b*" :limit "2")
      Search all tokens semantically related to a particular token and start by a given letter in Datamuse. Return a fixed number of results. 
     
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
       Search  all the tokens that more likely follow a word-1 and start by a particular letter in Datamuse.

### 2. Open access APIs related to a certain area: 

#### 2.1. Meal DB API (https://www.themealdb.com/api.php)

    * (request-mealDB "Carbonara")
      Search for a Recipe in the MealDB
    
    * (request-mealDB-filter :ingredient "Tuna")
      Search for a recipe in the MealDB with a main ingredient X
    
    * (request-mealDB-filter :category "Seafood")
      Search for all the meals in a particular category
    
    * (request-mealDB-filter :country "Italian")
      Search for all the meals traditional of a particular country

    * (request-mealDB-hungry-random) 
     Search for a random recipes to have some inspiration
        
#### 2.2. Mediastack API (https://mediastack.com/documentation)


### 3. KG APIs :

#### 3.1. MediaWiki API(https://www.wikidata.org/wiki/Wikidata:WikiProject_Documentation): 

    * (search-wikipedia "Steve McQueen")
      Function to search for a token in Wikipedia. To query just insert as an argument the string you are looking for. 
 
#### 3.2. Wikidata API(https://www.wikidata.org/wiki/Wikidata:WikiProject_Documentation):

    * (search-entity-in-wikidata "Steve McQueen" ) 
      Function to search for a token in Wikidata. To use it, just insert as an argument the string you are looking for. 
    
    * (get-wikidata-entity "Q159347")
      Function to get all statements for a particular URI in Wikidata. To use it, just insert as an argument the URI you are looking for. For more information about URI in Wikidata, please see https://www.wikidata.org/wiki/Help:Statements 
    
#### 3.3. Google Knowledge Graph API (https://developers.google.com/knowledge-graph): 

    * (request-google-knowledge-graph "Steve McQueen" :types "person")
      Function to search a token in the Google Knowledge Graph APIs while specifying the type. To use it, just insert as an argument the string you are looking for.

#### 3.4. Catasto Datastories (https://stories.datalegend.net/catasto/): 

Data accessible in in the linked data version of the Florentine Catasto of 1427 available on druid. 

    * (request-catasto)
       Function to access a saved SPARQL query on the Catasto dataset. The endpoint to call is shown under "API Variable".
