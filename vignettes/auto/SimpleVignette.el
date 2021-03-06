(TeX-add-style-hook
 "SimpleVignette"
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
    "opt1")))

