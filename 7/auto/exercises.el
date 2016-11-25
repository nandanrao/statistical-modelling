(TeX-add-style-hook
 "exercises"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "a4paper" "12pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("roboto" "sfdefault" "light")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art12"
    "mathtools"
    "amsfonts"
    "amssymb"
    "bm"
    "cmbright"
    "commath"
    "multicol"
    "fancyhdr"
    "sansmath"
    "roboto")
   (LaTeX-add-environments
    '("proof" LaTeX-env-args ["argument"] 0)
    '("prop" LaTeX-env-args ["argument"] 0)))
 :latex)

