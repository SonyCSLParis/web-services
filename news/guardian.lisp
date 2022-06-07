;;;;;
;;;;; (c) Sony Computer Science Laboratories Paris
;;;;;
;;;;; File authored by Remi van Trijp (remi.vantrijp@sony.com)

(in-package :web-services)

;;;;; ======================================================================================
;;;;; GUARDIAN API
;;;;; - Get your API key here: https://open-platform.theguardian.com/access/
;;;;; - Learn more about the API here: https://open-platform.theguardian.com/documentation/
;;;;; - Find out what content is available: https://open-platform.theguardian.com/explore/
;;;;; ======================================================================================

(export '(guardian-api))

(defmacro guardian-api (api-endpoint &rest parameters
                                     &key &allow-other-keys)
  `(news-api (format nil "~a~a" (get-base-uri :guardian) ,api-endpoint)
             ,@parameters :api-key (get-api-key :guardian))) 

;;;;; Examples:
;;;;; --------------------------------------------------------------------------------------

;;;;; Results are returned as a paginated list (by default 10 results). In order to page through
;;;;; the results, you can add the "page" parameter to your query:

; (guardian-api "search" :q "debate" :page "2")

;;;;; "search" returns all pieces of content in the API:
; (guardian-api "search" :q "debate")

;;;;; So many results! Let's filter them for only those of 2022:
; (guardian-api "search" :q "debate" :from-date "2022-01-01")

;;;;; You can also use logical operators AND, OR, and NOT for queries and filter for the tag "politics/politics"
; (guardian-api "search" :q "debate AND economy" :tag "politics/politics")

;;;;; AND has precedence over OR but you can use brackets to disambiguate:
; (guardian-api "search" :q "debate AND (economy OR education)")

;;;;; Use quotation marks for exact matches for phrases:
; (guardian-api "search" :q "\"mitochondrial donation\"")


;;;; Some filters support logic operators and grouping of expressions through the symbols 
;;;; , (and) | (or) and - (not). Please see the Guardian API documentation for more.

;;;;; TAGS
;;;;; --------------------------------------------------------------------------------------
;;;;;
;;;;; Obtain all tags through the endpoint "tags":
;;;;; (guardian-api "tags")

;;;;; You can search for all content tagged in a certain way:
; (guardian-api "search" :tag "environment/recycling")
; (guardian-api "search" :tag "environment/recycling,environment/plasticbags")

;;;;; You can also search for any tag that contains a particular query:
; (guardian-api "tags" :q "green")

;;;;; Tags have different types:
;;;;; - keyword (a word describing what the content is about)
;;;;; - series (a regular feature, e.g. "Band of the Week")
;;;;; - contributor (author(s) of content)
;;;;; - tone (the intent of the article, such as "feature" or "obituary")
;;;;; - type (the media type, such as article, video, blog)
;;;;; - blog (the name on of the Guardian's blogs)


;;;;; SECTIONS
;;;;; --------------------------------------------------------------------------------------
;;;;; Sections endpoint contains all of the Guardian's sections:
; (guardian-api "sections")


;;;;; EDITIONS
;;;;; --------------------------------------------------------------------------------------
;;;;; Editions endpoint contains all of the Guardian's editions:
; (guardian-api "editions")

;;;;; SINGLE ITEMS
;;;;; --------------------------------------------------------------------------------------
;;;;; You can also look directly for information if you have their ID:
; A piece of content: (guardian-api "technology/2014/feb/18/doge-such-questions-very-answered") 
; A tag: (guardian-api "world/france")
; A section: (guardian-api "lifeandstyle")
