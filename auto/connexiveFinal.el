(TeX-add-style-hook
 "connexiveFinal"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("beamer" "9pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("fontenc" "T1") ("inputenc" "utf8") ("babel" "english") ("ulem" "normalem")))
   (add-to-list 'LaTeX-verbatim-environments-local "semiverbatim")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "latex/judgements"
    "beamer"
    "beamer10"
    "fontenc"
    "inputenc"
    "lmodern"
    "babel"
    "stmaryrd"
    "verbatim"
    "geometry"
    "setspace"
    "latex/agda"
    "unicode-math"
    "newunicodechar"
    "xcolor"
    "ulem"
    "soul"
    "amsmath"
    "amssymb"
    "multirow"
    "multicol"
    "caption"
    "bussproofs"
    "tikz-cd")
   (TeX-add-symbols
    '("id" ["argument"] 2)
    '("opp" 1)
    '("indid" 1)
    '("ind" 1)
    '("define" 1)
    '("refl" 1)
    '("equivalenceH" 2)
    '("appH" 2)
    '("arrowH" 2)
    '("comprehensionH" 3)
    '("equalH" 2)
    "jdeq"
    "defeq"
    "blank"
    "UU"
    "rev"
    "bbU"
    "type")
   (LaTeX-add-labels
    "lem:opp")
   (LaTeX-add-environments
    "oldquote")
   (LaTeX-add-amsthm-newtheorems
    "definition"
    "lem"
    "proof"
    "thm")
   (LaTeX-add-xparse-environments
    '("quote" "om")))
 :latex)

