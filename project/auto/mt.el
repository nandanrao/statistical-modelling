(TeX-add-style-hook
 "mt"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "a4paper" "12pt")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art12"
    "graphicx"
    "color"
    "framed"
    "alltt"
    "mathtools"
    "amsfonts"
    "amssymb"
    "cmbright"
    "bm"
    "commath"
    "multicol"
    "fancyhdr"
    "upquote")
   (TeX-add-symbols
    '("hlkwd" 1)
    '("hlkwc" 1)
    '("hlkwb" 1)
    '("hlkwa" 1)
    '("hlstd" 1)
    '("hlopt" 1)
    '("hlcom" 1)
    '("hlstr" 1)
    '("hlnum" 1)
    "maxwidth"
    "hlipl"
    "FrameCommand")
   (LaTeX-add-environments
    "kframe"
    "knitrout"))
 :latex)

