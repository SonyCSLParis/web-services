;; Copyright 2021 Sony CSL Paris

;;; Copyright (C) Sony Computer Science Laboratories Paris
;;;               Remi van Trijp (www.remivantrijp.eu)
;;;               Martina Galletti
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;=========================================================================

(in-package :web-services)

(export '(request-merriam-webster-collegiate-dictionary
          request-merriam-webster-collegiate-thesaurus
          request-merriam-webster-spanish-dictionary
          request-merriam-webster-medical-dictionary
          request-merriam-webster-learners-dictionary
          request-merriam-webster-elementary-dictionary
          request-merriam-webster-intermediate-dictionary
          request-merriam-webster-intermediate-thesaurus
          request-merriam-webster-school-dictionary))

;; ---------------------------------------------------------------------------------------------------------------------------------------------------
;; Merriam Webster
;; https://www.dictionaryapi.com/
;; ---------------------------------------------------------------------------------------------------------------------------------------------------

;; Remi: 1/ I noticed that all of the merriam-webster functions used the same URI but only a different ressource.
;;          I therefore created a general function to which you can pass the ressource.
;;       2/ Instead of pointing directly to the API-key variable, I used the helper function (get-api-key :merriam-webster).
;;          Since the key is a parameter in the Merriam-Webster API, I put it in the parameters.
;;       3/ I leave the possibility open to pass parameters and additional-headers.
(defun request-merriam-webster (ressource word &key additional-headers parameters)
  "General function for requesting information from the Merriam Webster API."
  (let ((uri (format nil "https://www.dictionaryapi.com/api/v3/references/~a/json/~a"
                     ressource (clean-request word))))
    (request-api uri
                 :additional-headers additional-headers
                 :parameters `(("key" . ,(get-api-key :merriam-webster))
                               ,@parameters))))
;; Example:
;; --------
;; (request-merriam-webster "collegiate" "singer")

;; Remi: All of the helper functions were doing the exact same thing but only changed which ressource was
;;       used. So I defined the following macro, which generates the defuns so we don't have to write them
;;       separately anymore.
;;       The macro does not only avoid me to copy-paste and change each function, but also gives me a central
;;       location if I want to change the definitions, which will make the code more adaptive.
;;
;;       Two warnings, however: if you change the macro, you might have to re-compile everything or do a "clean"
;;       of the code if you see the code doesn't behave as expected. Secondly, you cannot jump to its source
;;       definition.

(defmacro define-merriam-webster-helper-functions (list-of-names-and-ressources)
  `(progn
     ,@(loop for lst in list-of-names-and-ressources
             collect `(defun ,(first lst) (word &key additional-headers parameters)
                        (request-merriam-webster ,(second lst) word
                                                 :additional-headers additional-headers
                                                 :parameters parameters)))))

;; Here I call the macro. A macro is a code-creation tool: it "expands" into code that only then gets
;; evaluated by Lisp. So for each pair, it creates a defun with that name and with that ressource to
;; pass to the general request-merriam-webster function.
(define-merriam-webster-helper-functions ((request-merriam-webster-collegiate-dictionary "collegiate")
                                          (request-merriam-webster-collegiate-thesaurus "thesaurus")
                                          (request-merriam-webster-spanish-dictionary "spanish")
                                          (request-merriam-webster-medical-dictionary "medical")
                                          (request-merriam-webster-learners-dictionary "learners")
                                          (request-merriam-webster-elementary-dictionary "sd2")
                                          (request-merriam-webster-intermediary-dictionary "sd3")
                                          (request-merriam-webster-intermediate-thesaurus "ithesaurus")
                                          (request-merriam-webster-school-dictionary "sd4")))

;; Here is an example of how a created function looks like:
;;; (defun request-merriam-webster-collegiate-dictionary (word &key additional-headers parameters)
;;;     (request-merriam-webster "collegiate" word :additional-headers additional-headers :parameters parameters))


;; And you can always test what a macro creates by calling macroexpand-1 as follows:
;; (macroexpand-1 '(define-merriam-webster-helper-functions ((request-merriam-webster-collegiate-dictionary "collegiate"))))
