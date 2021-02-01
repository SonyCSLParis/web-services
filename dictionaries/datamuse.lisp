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
(export 'request-datamuse)
;; -------------------------------------------------------------------------------------------------------------
;; Datamuse Function
;; -------------------------------------------------------------------------------------------------------------

(defun request-datamuse (&key rhyme related-to frequent-adj frequent-noun frequent-follow frequent-preceed strongly-assoc synonyms antonyms kind-of gen com part-of frequent-follower frequent-pred approx-rhy homophone cons-match topic spelled-similarly suggestions vocab limit metadata start-by)
    "Find words that match a given set of constraints and that are likely in a given context"
    (request-api "https://api.datamuse.com/words"
               :parameters `(,@(when rhyme `(("rel_rhy" . ,rhyme)))
                             ,@(when related-to `(("ml" . ,related-to)))
                             ,@(when frequent-adj `(("rel_jjb" . ,frequent-adj)))
                             ,@(when frequent-noun `(("rel_jja" . ,frequent-noun)))
                             ,@(when frequent-follow `(("lc" . ,frequent-follow)))
                             ,@(when frequent-preceed `(("rc" . ,frequent-preceed)))
                             ,@(when strongly-assoc `(("rel_trg" . ,strongly-assoc)))
                             ,@(when synonyms `(("rel_syn" . ,synonyms)))
                             ,@(when antonyms `(("rel_ant" . ,antonyms)))
                             ,@(when kind-of `(("rel_spc" . ,kind-of)))
                             ,@(when gen `(("rel_gen" . ,gen)))
                             ,@(when com `(("rel_com" . ,com)))
                             ,@(when part-of `(("rel_par" . ,part-of)))
                             ,@(when frequent-follower `(("rel_bga" . ,frequent-follower)))
                             ,@(when frequent-pred `(("rel_bgb" . ,frequent-pred)))
                             ,@(when approx-rhy `(("rel_nry" . ,approx-rhy)))
                             ,@(when homophone `(("hom" . ,homophone)))
                             ,@(when cons-match `(("cns" . ,cons-match)))
                             ,@(when topic `(("topics" . ,topic)))
                             ,@(when spelled-similarly `(("sp" . ,spelled-similarly)))
                             ,@(when suggestions `(("s" . ,suggestions)))
                             ,@(when vocab `(("v" . ,vocab)))
                             ,@(when limit `(("max" . ,limit)))
                             ,@(when metadata `(("md" . ,metadata)))
                             ,@(when start-by `(("sp" . ,start-by))))))

