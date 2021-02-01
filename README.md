# Web-Services

A Package to search for particular entities or URI in different open access APIs in Common Lisp. 

The APIs accessible with this package are : 

      1. Dictionaries and Thesaurus:
         1.1. Words API (https://www.wordsapi.com/),
         1.2. Merriam-Webster Dictionary API (https://dictionaryapi.com/)
         1.3. Datamuse API (https://www.datamuse.com/api/).
         
      2. Open access APIs related to a certain area: 
         2.1. Meal DB API (https://www.themealdb.com/api.php),
         2.2. Mediastack (https://mediastack.com/).
         
      3. KG APIs: 
         3.1. MediaWiki API(https://www.mediawiki.org/wiki/API:Main_page), 
         3.2. Wikidata API(https://www.wikidata.org/wiki/Wikidata:Data_access),
         3.3. Google Knowledge Graph API (https://developers.google.com/knowledge-graph), 
         3.4. VUA API (https://stories.datalegend.net), 
       
## Contributors:

    Dr. Remi van Trijp
    Martina Galletti
    
## Dependencies

The Lisp-APIs code depends on:

    Drakma (can be installed using Quicklisp, see https://edicl.github.io/drakma/)
    Yason (can be installed using Quicklisp, see https://github.com/phmarek/yason)
    Cl-ppcre (can be installed using Quicklisp, see http://edicl.github.io/cl-ppcre/)

## 1. Interfacing Common Lisp with Web Services

### 1.1 Package

The different APIs can be queried using some specific helper functions which send a request to the APIs specified and encode the results into a Lisp list. 

Please note, for the Google Knowledge Graph APIs, the Mediastack APIs and the Merriam-Webster Dictionary API, you have to specify your personal "API key" 
to send the request. For information about they can be obtained, please see : 

- https://dictionaryapi.com/register/index for the Merriam-Webster Dictionary,
- https://developers.google.com/knowledge-graph/how-tos/authorizing for the Google Knowledge Graph API,
- https://mediastack.com/quickstart for the Mediastack API.

#### 1.Dictionaries and Thesaurus:

#####  1.1. Words API (https://www.wordsapi.com/): 

It's an API for the English Language which enables you to find definitions, related words, and more, with a simple RESTful API. 

    (request-words-api "singer") : function to search for a particular token in Words API. 
    To search, just insert as argument the token whose definition you are looking for. 
 
##### 1.2. Merriam-Webster Dictionary API (https://dictionaryapi.com/):   

The Merriam-Webster Dictionary API gives developers access to a comprehensive resource of dictionary and thesaurus content as well as specialized medical, Spanish, ESL, and student-friendly vocabulary. 

    (request-merriam-webster-dictionary "singer") : function to search for a particular token in Merriam-Webster Dictionary 
    API. To search, just insert as argument the token whose definition you are looking for. 
    
    (request-merriam-webster-thesaurus "singer") : function to search for a particular token in the Merriam-Webster Thesaurus 
    API.To search, just insert as argument the token whose definition you are looking for. 
    
##### 1.3. Datamuse API (https://www.datamuse.com/api/):
 
The Datamuse API is a word-finding query engine for developers. You can use it in your apps to find words that match a given set 
of constraints and that are likely in a given context. You can specify a wide variety of constraints on meaning, spelling, sound, 
and vocabulary in your queries, in any combination. 

    (request-rhyme-datamuse "vice"): Search all the tokens that rhyme with a certain token in Datamuse.
    
    (request-related-to-datamuse "vice"): Search all tokens semantically related to a particular token in Datamuse.
    
    (request-rhyme-related-to-datamuse "vice" :related-to "temperature"): Search a token semantically related to another particular 
    token in Datamuse.
    
    (request-adjectives-datamuse "ocean"): Search for adjectives mostly used with a particular token in Datamuse.
    
    (request-adjectives-related-to-datamuse "ocean" :related-to "temperature"): Search the adjectives mostly used with a particular 
    token-1 and semantically related to another token-2 in Datamuse.
    
    (request-nouns-datamuse "yellow"): Search the most used nouns with a particular adjective in Datamuse
    
    (request-often-follow-datamuse "drink"): Search all the tokens that more likely follow a token-1 in Datamuse
    
    (request-often-follow-start-by-datamuse "drink" :start-by "w*"): Search  all the tokens that more likely follow a word-1 
    and start by a particular letter in Datamuse.

### 2. Open access APIs related to a certain area: 

#### 2.1. Meal DB API (https://www.themealdb.com/api.php)

    (request-mealDB "Carbonara"): Search for a Recipe in the MealDB
    
    (request-ingredient-filter-mealDB "chicken_breast"): Search for a recipe in the MealDB with a main ingredient X
    
    (request-category-filter-mealDB "Seafood"): Search for all the meals in a particular category
    
    (request-country-filter-mealDB "Italian"): Search for all the meals traditional of a particular country
    
    (request-hungry-random-mealDB): I am hungry, what should I eat tonight?
    
#### 2.2. Mediastack (https://mediastack.com/quickstart)

    (request-live-news-Mediastack "Barcelona" :categories "sport" :languages "it" :countries "it" :limit "2" :sort "published_asc"): 
    search for the full set of available real-time news articles can be accessed using a simple API request to the mediastack API's 
    news endpoint. Please note that account subscribed to the Free Plan will receive live news only with a 30-minute delay.
    
    (request-historical-news-Mediastack "Barcelona" :date "29-01-20" :sources "cnn" :categories "sport" :countries "us" :languages "en" 
    :limit "2":sort "published_asc"): if you are subscribed to the Standard Plan or higher, you will be able to access historical news data 
    by specifying a historical date using the API's date parameter in YYYY-MM-DD format.

### 3. KG APIs :

#### 3.1. MediaWiki API(https://www.mediawiki.org/wiki/API:Main_page): 

The MediaWiki Action API is a web service that allows access to some wiki-features like authentication, page operations, and search. 
It can provide meta information about the wiki and the logged-in user. 

    (search-wikipedia "Steve McQueen"): function to search for a token in Wikipedia. 
    To query just insert as an argument the string you are looking for. 
 
#### 3.2. Wikidata API(https://www.wikidata.org/wiki/Wikidata:Data_access):

Wikidata is a free and open knowledge base that can be read and edited by both humans and machines. Wikidata acts as central storage 
for the structured data of its Wikimedia sister projects including Wikipedia, Wikivoyage, Wiktionary, Wikisource, and others.  
The content of Wikidata is available under a free license, exported using standard formats, and can be interlinked to other open data 
sets on the linked data web.

    (search-entity-in-wikidata "Steve McQueen" ): function to search for a token in Wikidata. 
    To use it, just insert as an argument the string you are looking for. 
    
    (get-wikidata-entity "Q159347"): function to get all statements for a particular URI in Wikidata. 
    To use it, just insert as an argument the URI you are looking for. 
    For more information about URI in Wikidata, please see https://www.wikidata.org/wiki/Help:Statements 
    
#### 3.3. Google Knowledge Graph API (https://developers.google.com/knowledge-graph): 
 
The Knowledge Graph Search API lets you find entities in the Google Knowledge Graph. The API uses standard schema.org types and 
is compliant with the JSON-LD specification.
 
    (request-google-knowledge-graph "Steve McQueen" :types "person"): function to search a token in the Google Knowledge Graph APIs 
    while specifying the type. To use it, just insert as an argument the string you are looking for.

#### 3.4. Catasto Datastories (https://stories.datalegend.net/catasto/): 

Data accessible in in the linked data version of the Florentine Catasto of 1427 available on druid. 

    (vua-kg-api-request): function to access a saved SPARQL query on the Catasto dataset 
    https://druid.datalegend.net/muhaiuser/-/queries/example-query/ (the API to call is shown under "API Variable", 
    i.e.  https://api.druid.datalegend.net/queries/muhaiuser/example-query/run )
   
