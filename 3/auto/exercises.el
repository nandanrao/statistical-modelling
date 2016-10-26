(TeX-add-style-hook
 "exercises"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "a4paper" "12pt")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art12"
    "mathtools"
    "amsfonts"
    "amssymb"
    "cmbright"
    "bm"
    "commath")
   (TeX-add-symbols
    "argmin"))
 :latex)

