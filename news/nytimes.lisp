;;;;;
;;;;; (c) Sony Computer Science Laboratories Paris
;;;;;
;;;;; File authored by Remi van Trijp (remi.vantrijp@sony.com)

(in-package :web-services)

;;;;; ======================================================================================
;;;;; New York Times API
;;;;; - Get your API key here: https://developer.nytimes.com/
;;;;; ======================================================================================

(export '(nytimes-semantic-api nytimes-search-api))

;;;;; --------------------------------------------------------------------------------------
;;;;; Semantic API: https://developer.nytimes.com/docs/semantic-api-product/1/overview
;;;;; --------------------------------------------------------------------------------------

(defun concept-type-p (concept-type)
  (assert (stringp concept-type))
  (member concept-type
          '("nytd_geo" "nytd_per" "nytd_org" "nytd_des" "nytd_ttl" "nytd_topic" "nytd_porg")
          :test #'string=))

(defmacro nytimes-concept (concept-type concept &rest parameters
                                        &key &allow-other-keys)
  `(progn
     (assert (concept-type-p ,concept-type))
     (news-api (format nil "~asemantic/v2/concept/name/~a/~a.json" 
                       (get-base-uri :nytimes) ,concept-type (clean-request ,concept))
               ,@parameters :api-key (get-api-key :nytimes))))
;; (nytimes-concept "nytd_geo" "France" :fields "all")
;; (nytimes-concept "nytd_per" "Obama, Barack" :fields "all")

(defmacro nytimes-search-concept (query &rest parameters
                                        &key &allow-other-keys)
  `(news-api (format nil "~asemantic/v2/concept/search.json?query=~a" 
                     (get-base-uri :nytimes) (clean-request ,query))
             ,@parameters :api-key (get-api-key :nytimes)))
;; (nytimes-search-concept "Barack" :concept-type "nytd_per")

;;;;; --------------------------------------------------------------------------------------
;;;;; Article API: https://developer.nytimes.com/docs/articlesearch-product/1/overview
;;;;; --------------------------------------------------------------------------------------

(defmacro nytimes-article-api (query &rest parameters
                                    &key &allow-other-keys)
  `(news-api (format nil "~asearch/v2/articlesearch.json?" (get-base-uri :nytimes))
             :q ,query
             ,@parameters :api-key (get-api-key :nytimes)))
;; (nytimes-article-api "election")
;; (nytimes-article-api "how to get your mind to read" :sort "relevance")
