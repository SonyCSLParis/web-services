# Lisp-APIs

A Package to query different APIs in Common Lisp. The APIs accessible with this package are : 

      1. Google Knowledge Graph API (https://developers.google.com/knowledge-graph), 
      2. MediaWiki API(https://www.mediawiki.org/wiki/API:Main_page), 
      3. Wikidata API(https://www.wikidata.org/wiki/Wikidata:Data_access),
      3. Dictionaries and Thesaurus such as 
         3.1. Words API (https://www.wordsapi.com/),
         3.2. Merriam-Webster Dictionary API (https://dictionaryapi.com/).

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

    (query-google-knowledge-graph) : function to query Google Knowledge Graph APIs. 
    To request just insert as argument the string you are looking for.
    
    (search-wikipedia) : function to query wikipedia. 
    To query just insert as argument the string you are looking for. 
    
    (search-entity-in-wikidata) : function to query wikidata. 
    To query just insert as argument the string you are looking for. 
    
    (get-wikidata-entity) : function to get all statements for a particular URI in Wikidata. 
    To request just insert as argument the URI you are looking for. 
    For more information about URI in Wikidata, please see https://www.wikidata.org/wiki/Help:Statements 
    
    (query-words-api) : function to query Words API. 
    To query just insert as argument the definition you are looking for. 
    
    (query-merriam-webster-dictionary) : function to query Merriam-Webster Dictionary API. 
    To query just insert as argument the definition you are looking for. 
    
    (query-merriam-webster-thesaurus) : function to query Merriam-Webster Thesaurus API. 
    To query just insert as argument the definition you are looking for. 
    



