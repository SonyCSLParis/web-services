;;;;;
;;;;; (c) Sony Computer Science Laboratories Paris
;;;;;
;;;;; File authored by Remi van Trijp (remi.vantrijp@sony.com)

(in-package :web-services)

(export '(news-api))

(defun get-base-uri (key)
  (case key
    (:guardian "https://content.guardianapis.com/")
    (:nytimes "https://api.nytimes.com/svc/")
    (t
     (error (format nil "No base-uri found for ~a" key)))))
;; (get-base-uri :nytimes)

(defun news-api (uri &rest parameters
                     &key &allow-other-keys)
  "General function for making http-requests to news media such as The Guardian or the NY Times."
  ;; Get the keywords that are not part of the request:
  (destructuring-bind (&whole whole
                              &key (method :get method-p)
                              (content-type "application/json" content-type-p)
                              (lisp-format :alist lisp-format-p)
                              (api-key nil api-key-p)
                              &allow-other-keys)
      parameters
    (unless api-key (error "Missing API key."))
    ;; Remove them from the http-request:
    (dolist (indicator '(:method :content-type :lisp-format :api-key))
      (remf whole indicator))
    ;; Perform the request:
    (request-api uri
                 :parameters (handle-parameters (append whole (list :api-key api-key)))
                 :method method
                 :content-type content-type
                 :lisp-format lisp-format)))
