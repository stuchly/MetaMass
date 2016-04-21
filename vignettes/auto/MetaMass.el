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
    "epigraph"
    "amsmath"
    "wasysym"
    "Sweave")
   (LaTeX-add-labels
    "fig1"
    "fig2"
    "fig3")))

