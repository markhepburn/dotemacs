;; Very simple mode for editing wsml files.  Basic syntax highlighting
;; at the moment, that's it.

;; Mark Hepburn, 2008

;; TODO:
;; * fix " as part of a word thing (so that forward-sexp does the right thing over strings)
;; * simple indentation

(defvar wsml-mode-keywords-1
  '("wsmlVariant"
    "namespace"
    "ontology"
    "webService"
    "goal")
  "Top-level keywords for font-lock in `wsml-mode'") ; nominally 'bold

(defvar wsml-mode-keywords-2
  '("capability"
    "interface"
    "choreography"
    "nfp" "endnfp"
    "nonFunctionalProperties" "endNonFunctionalProperties"
    "concept" "instance" "subConceptOf"
    "importsOntology"

    "transitionRules"
    "stateSignature"
    "precondition" "postcondition")
  "Second-level keywords for font-lock in `wsml-mode'") ; nominally 'bold-italic

(defvar wsml-mode-keywords-3
  '("ofType" "impliesType"
    "in" "out" "shared" "controlled" "withGrounding"
    "definedBy"
    "sharedVariables"
    "hasValue"
    "memberOf"
    "forall" "with" "do" "endForall"
    "and" "or")
  "Third-level keywords for font-lock in `wsml-mode'") ; nominally 'italic

(defvar wsml-mode-syntax-table
  (let ((wsml-mode-syntax-table (make-syntax-table)))
    ;; underscore is part of a word:
    (modify-syntax-entry ?_ "w" wsml-mode-syntax-table)
    ;; so is " for now (not sure that this is a great idea; hack to get around \<" not matching in a regexp because " isn't part of a word):
    (modify-syntax-entry ?\" "w" wsml-mode-syntax-table)
    (modify-syntax-entry ?\? "_" wsml-mode-syntax-table) ; ? is a symbol constituent
    ;; emulate c++ multiline comments (note that // comments are handled separately) :
    (modify-syntax-entry ?/ ". 14b" wsml-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" wsml-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" wsml-mode-syntax-table)
    wsml-mode-syntax-table)
  "Syntax table for wsml-mode (basically just copies c++ style comments)")

(defvar wsml-mode-font-lock-keywords
  `(("\\(\\(^\\|[ \t]\\)//\\|comment\\).*$" . font-lock-comment-face) ; // is a comment, only if it's at the start of a line or after space, ie /not/ in http://asdf!
    ("\\<_\\(\"[^\"]+\"\\|\\w+\\)\\>" . font-lock-type-face)
    ("\"[^\"]*\"" . font-lock-string-face) ; strings.  do this manually so it isn't overridden by eg "ontology" appearing in a string
    (,(concat "\\<\\(?:" (regexp-opt wsml-mode-keywords-1) "\\)\\>") . 'bold-italic)
    (,(concat "\\<\\(?:" (regexp-opt wsml-mode-keywords-2) "\\)\\>") . 'bold)
    (,(concat "\\<\\(?:" (regexp-opt wsml-mode-keywords-3) "\\)\\>") . 'font-lock-keyword-face)

    ("\\_<\\?\\w+\\>" . font-lock-variable-name-face)
    ("\\<\"[^\"]*\"\\>" . font-lock-string-face))
  "Font lock keywords for `wsml-mode'")

(define-derived-mode wsml-mode text-mode "WSML"
  "Simple major mode for editing WSML documents."
  (set (make-local-variable 'font-lock-defaults)
       '(wsml-mode-font-lock-keywords))
  (set-syntax-table wsml-mode-syntax-table)
  (require 'font-lock)
  ;; make sure we don't automatically wrap lines (hmm, looks like this
  ;; has to be run after the derived mode somehow..):
  (auto-fill-mode -1)
  (if font-lock-mode
      (font-lock-fontify-buffer)))

(provide 'wsml-mode)