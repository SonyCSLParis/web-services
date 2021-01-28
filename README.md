# Lisp-APIs

A Package to query different APIs in Common Lisp. The APIs accessible with this package are : 
      1. Dictionaries and Thesaurus such as 
         1.1. Words API (https://www.wordsapi.com/),
         1.2. Merriam-Webster Dictionary API (https://dictionaryapi.com/).
      2. Datamuse API (https://www.datamuse.com/api/)
      3. MediaWiki API(https://www.mediawiki.org/wiki/API:Main_page), 
      4. Wikidata API(https://www.wikidata.org/wiki/Wikidata:Data_access),
      5. Google Knowledge Graph API (https://developers.google.com/knowledge-graph), 
      6. VUA API (https://stories.datalegend.net), 
      7. Meal DB API (https://www.themealdb.com/api.php)
       
## Contributors:

    Dr. Remi van Trijp
    Martina Galletti
    
## Dependencies

The Lisp-APIs code depends on:

    Drakma (can be installed using Quicklisp, see https://edicl.github.io/drakma/)
    Yason (can be installed using Quicklisp, see https://github.com/phmarek/yason)
    Cl-ppcre (can be installed using Quicklisp, see http://edicl.github.io/cl-ppcre/)

## 1. Interfacing Common Lisp with APIs

### 1.1 Package

The different APIs can be queried using some specific helper functions which send a request to the APIs specified and encode the results. 

Please note, for the Google Knowledge Graph APIs and the Merriam-Webster Dictionary API, you have to specify your personal "API key" to send the request. For information about they can be obtained, please see https://dictionaryapi.com/register/index for the Merriam-Webster Dictionary and https://developers.google.com/knowledge-graph/how-tos/authorizing for the Google Knowledge Graph API. 

1. Dictionaries and Thesaurus such as 

  1.1. Words API (https://www.wordsapi.com/): 
    
    (query-words-api) : function to query Words API. 
    To query just insert as argument the definition you are looking for. 
 
 1.2. Merriam-Webster Dictionary API (https://dictionaryapi.com/):   
 
    (query-merriam-webster-dictionary) : function to query Merriam-Webster Dictionary API. 
    To query just insert as argument the definition you are looking for. 
    
    (query-merriam-webster-thesaurus) : function to query Merriam-Webster Thesaurus API. 
    To query just insert as argument the definition you are looking for. 
    
 2. Datamuse API (https://www.datamuse.com/api/):
    
    (query-rhyme-datamuse): Search all the words that rhyme with a certain token in datamuse.
    
    (query-related-to-datamuse):  function to query the Datamuse API and find a token semantically related for a particular token.
    
    (query-rhyme-related-to-datamuse): Search all the words that rhyme with a certain word-1 in datamuse and are semantically related to another word-2
    
    (query-adjectives-datamuse): Search for adjectives mostly used with a word-1.
    
    (query-adjectives-related-to-datamuse): Search for adjectives mostly used with a word-1 and semantically related to a word-2.
    
    (query-nouns-datamuse): Search for the most used nouns with an adjective.
    
    (query-often-follow-datamuse): Search for all the words that more likely follow a word-1.
    
    (query-often-follow-start-by-datamuse): Search for all the words that more likely follow a word-1 and start by a particular letter.
    
3. MediaWiki API(https://www.mediawiki.org/wiki/API:Main_page): 

    (search-wikipedia): function to query wikipedia. 
    To query just insert as argument the string you are looking for. 
 
4. Wikidata API(https://www.wikidata.org/wiki/Wikidata:Data_access):

    (search-entity-in-wikidata): function to query wikidata. 
    To query just insert as argument the string you are looking for. 
    
    (get-wikidata-entity): function to get all statements for a particular URI in Wikidata. 
    To request just insert as argument the URI you are looking for. 
    For more information about URI in Wikidata, please see https://www.wikidata.org/wiki/Help:Statements 
    
 5. Google Knowledge Graph API (https://developers.google.com/knowledge-graph): 
 
    (query-google-knowledge-graph): function to query Google Knowledge Graph APIs. 
    To request just insert as argument the string you are looking for.

 6. VUA API (https://stories.datalegend.net): 
 
    (vua-kg-api-request): function to access a saved SPARQL query on the Catasto dataset https://druid.datalegend.net/muhaiuser/-  /queries/example-query/ (the API to call is shown under "API Variable", i.e.  https://api.druid.datalegend.net/queries/muhaiuser/example-query/run )
    
 7. Meal DB API (https://www.datamuse.com/api/)

    (query-mealDB): Search for a Recipe in the MealDB
    
    (query-ingredient-filter-mealDB): Search for a recipe in the MealDB with a main ingredient X
    
    (query-category-filter-mealDB): Search for all the meals in a particular category
    
    (query-country-filter-mealDB): Search for all the meals traditional of a particular country
    
    (query-hungry-random-mealDB): I am hungy, what should I eat tonight?
