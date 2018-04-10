;; Quest's Emacs Reference
;;   This page records various useful keybindings and tips.
;;   It's a "living document", meaning that I update it upon mastery of a given tip.
;;   Written in Clojure for edn highlighting; Emacs never loads this file itself

;; TODO: git porcelain bindings

;; -------~~~=== INVESTIGATORY ===~~~-------
;;     ACTIONS WHICH REVEAL INFORMATION

(def investigatory-bindings
  {"C-h C-h"            "Help about help"
   "C-h k cmd-name"     "Find doc for command with given name"
   "C-h a cmd-name-rgx" "Find doc for command name matching regex"
   "C-h c cmd-name"     "Find doc for command by name"
   "C-h f fn-name"      "Find doc for function by name (helm autocomplete enabled)"
   "C-h i"              "Open top level of info mode tree"
   "M-x sp-cheat-sheet" "Show smartparens cheat sheet"})

(def google-bindings
  {"C-x g g query"      "Googles input string (defaults to point)"
   "C-x g s"           g "Googles string under point"})

;; -------~~~=== TRANSFORMATIVE ===~~~-------
;;     ACTIONS WHICH ALTER SUBJECT DATA
(def line-bindings
  {"C-k" "Kill to end of line"})

;; -------~~~=== PRESENTATION ===~~~-----------
;;        ACTIONS WHICH CHANGE VIEW



;; -------~~~=== NAVIGATIONAL ===~~~-----------
;;        ACTIONS WHICH CHANGE LOCATION
(def mark-bindings
  {"C-u C-Spc" "Jump to previous mark"})

(def etags-bindings
  {"M-."           "Find a tag"
   "M-,"           "Jump to previous tag"
   "M-x list-tags" "List all tags defined in source file"})

;; -------~~~=== SYNTACTICAL ===~~~--------
;;      ACTIONS WHICH AFFECT SEMANTICS
(def meta-bindings
  {"C-x Z" "Repeat last Emacs command"})

(def macro-bindings
  {"C-x (" "Start macro"
   "C-x )" "End macro"
   "C-x e" "Run last defined macro"
   "M-x apply-macro-to-region-lines" "Applies macro to defined region"})
