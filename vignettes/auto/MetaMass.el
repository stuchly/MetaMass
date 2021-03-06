(TeX-add-style-hook
 "MetaMass"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "a4paper")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "graphicx"
    "verbatim"
    "inputenc"
    "amsmath"
    "amssymb"
    "amsthm"
    "mathrsfs"
    "amsfonts"
    "epigraph"
    "wasysym"
    "Sweave"
    "hyperref")
   (LaTeX-add-labels
    "opt1"
    "opt2"
    "opt3"
    "opt4"
    "opt5"
    "Examps"
    "ex1"
    "ex2"
    "ex3"
    "ex4"
    "ex5")))

