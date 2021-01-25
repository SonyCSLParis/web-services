;; Copyright (C) Sony Computer Science Laboratories Paris
;;               Remi van Trijp (www.remivantrijp.eu)
;;               Martina Galletti
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
(ql:quickload :utils)
(ql:quickload :fcg)
(ql:quickload :irl)

(in-package :asdf)

(defsystem :apis
  :description "A system querying KG APIs in Common Lisp"
  :depends-on (:drakma :yason :cl-user :common-lisp :cl-ppcre :test-framework :monitors :network)
  :components ((:file "package")
               (:file "query")))
   