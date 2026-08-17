; crew/vi/config.l -- the THEME: molokayo, as plain data the painter reads.
; crew/lux/config.l is the shape -- a body-LESS `:`, values not code.
;
; the theme is keyed by VIM HIGHLIGHT GROUP, not by syntax class. crew/vi/hue.l
; says "a comment is a Comment"; this file says "in molokayo a Comment is 59 /
; #75715E". that split is what lets the generated vim syntax file emit plain
; `hi def link` lines -- a syntax file that hardcoded colours would fight the
; reader's own colorscheme -- while pulchritude, which has no colorscheme, still
; gets the same molokayo out of the same table. swap the numbers below for
; another scheme's and only the editor follows.
;
; every value is lifted VERBATIM from awesome-vim-colorschemes: molokayo.vim is
; molokai.vim with `molokai_original = 1` plus its own `hi!` lines, and molokayo
; WINS where it defines a group (Identifier, Special). nothing here is
; approximated by us -- the cterm numbers are molokai's own 256-colour block, so
; a terminal that cannot do 24-bit still shows the author's fallback.
;
; a row is `(group cterm256 "#rrggbb" faces)`, faces drawn from (bold italic).
(use 'kore) ; the text/u floors ride module 'kore now; ambient here until this app wraps
(:
   hue-theme
    '((Comment    59  "#75715E" ())
      (String     144 "#E6DB74" ())
      (Number     135 "#AE81FF" ())
      (Constant   135 "#AE81FF" (bold))
      (Statement  161 "#F92672" (bold))
      (Function   118 "#A6E22E" ())
      (Identifier 214 "#ffb700" ())          ; molokayo over molokai's #FD971F/208
      (Special    81  "#66d0ef" (italic))    ; molokayo over molokai's #66D9EF
      (Operator   161 "#F92672" ())
      (Delimiter  241 "#8F8F8F" ()))

   ; the user's own rows, read as DATA (never evaled) at editor birth and folded
   ; over the above -- kore ships as a baked image, so editing this file is not a
   ; knob anyone downstream has. same row shape; a file with no rows in it means
   ; no overrides, which is a perfectly good thing for a theme file to mean.
   hue-theme-file "/.love/hue.l")
