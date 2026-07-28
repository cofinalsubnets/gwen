" vim syntax for love (.l)
" based on lisp.vim by Charles E Campbell <http://www.drchip.org/astronaut/vim/index.html#SYNTAX_LISP>
if exists("b:current_syntax")
  finish
endif

" symbol-constituent chars (the reader ends a token only on whitespace and
" ( ) " ' ` , # ; ). operators @ # $ are excluded so they highlight standalone;
" % is a plain dyadic infix now (mod is gone).
"  33 !  37 %  38 &  42 *  43 +  45 -  46 .  47 /  48-57 digits  58 :
"  60-63 < = > ?  92 \  94 ^  95 _  124 |  126 ~   (@ = alphabetics)
syn iskeyword @,33,37,38,42-43,45-47,48-57,58,60-63,92,94-95,124,126

" The three special forms: : (letrec*/seq), ? (cond), \ (lambda/quote)
syn keyword LoveForm : ? \\

" The surface vocabulary, curated from (names ()) -- probe the binary and
" re-curate on a rename. bare punct operators (+ - * / < <= ...) are NOT
" keywords: the sigil matches below own every punct run, so the glued/spaced
" valence shows in the colour.
" chains & lists
syn keyword LoveFunc link cap cup caap caup cuap cuup
syn keyword LoveFunc caaap caaup cauap cauup cuaap cuaup cuuap cuuup
syn keyword LoveFunc rest rev init last take drop part zip cat revcat catmap
syn keyword LoveFunc map filter each all any foldl foldr foldl1 foldr1
syn keyword LoveFunc merge sort sortby jot iota unfold gather spread assoc member?
syn keyword LoveFunc ring stack monoid inner outer
" combinators & application
syn keyword LoveFunc id co const flip compose ap apof apover tap wrap ev call-cc
" numbers & math (power IS application; sine/cosine/log the transcendental nifs)
syn keyword LoveFunc abs gcd modpow int negate reciprocal fraction min max
syn keyword LoveFunc sine cosine tangent log re im conj arg twin gem num-ap
" the two measures & truth (net how much, tally how many; sat the one clamp)
syn keyword LoveFunc net prod tally sat saturate bit
syn keyword LoveFunc nil? zero? one? two? empty? whole?
" the celestial predicates
syn keyword LoveFunc charm? sun? big? gem? twin? star? galaxy? constellation?
syn keyword LoveFunc atom? nom? name? string? tray? tablet? hot? lit? id? coin? cue? back?
" randomness
syn keyword LoveFunc rand randf random seed coin rng-get rng-set
" trays & galaxies
syn keyword LoveFunc array rank shape tier aall gem-tray star-tray twin-tray top-tray
syn keyword LoveFunc blit blitrow pour shore gauge
" strings & mints
syn keyword LoveFunc string snip intern nom mint slurp show sip parse parsed reads
" books & tablets
syn keyword LoveFunc tablet keys dig pin pull peep missing names pinw peepw
syn keyword LoveFunc cask jug freeze var mind
" modules (the registry: enter/leave seals a layer, use splices, from reaches in)
syn keyword LoveFunc enter leave seal use from
" control (help/welp, missing, apcap)
syn keyword LoveFunc welp scare scare? more? apcap ufail ufail? err die-of catch quit
" tasks & sound
syn keyword LoveFunc spawn twirl wait await still fires fired? reply hear listen sound
" i/o & ports
syn keyword LoveFunc read getc putc puts putn putx putbn put print say dot in out
syn keyword LoveFunc flush open close openfd fdclose lseek pipe eof?
syn keyword LoveFunc mapfd mapfdo mapin mapout
" the posix crew
syn keyword LoveFunc run exec getenv setenv environ argv cmdline clock
syn keyword LoveFunc chdir cwd mkdir rmdir unlink rename symlink hardlink readlink readdir
syn keyword LoveFunc stat chmod chown umask utime mount newns getpid
syn keyword LoveFunc signal sigfd sigtake ioctl winsize setwinsize ttyfg ptyecho spawnio
syn keyword LoveFunc memfd sha256 accept connect connectu
syn keyword LoveFunc udp-bind udp-send udp-recv wl-send wl-recv
" display & the shell core
syn keyword LoveFunc see unsee gaze glass screen font scribe raw wet swig runt unmap
syn keyword LoveFunc edraw edln shell bao
" logic (kanren)
syn keyword LoveFunc kanren unify var
" tools & images
syn keyword LoveFunc glaze bake load uu overlay overlay-set overlay-off
syn keyword LoveFunc love-version love-arch love-tco
" cuda
syn keyword LoveFunc cuda-avail cuda-gemm cuda-ew cuda-transp cuda-reduce

" Macros (head-symbol rewrites installed with ::)
syn keyword LoveMacro :: L list do begin progn let if cond quote tuple hash pins
syn keyword LoveMacro assert suite && \|\| :- ?- >>= <=< zz et vel

" Constants (the exact circle constants; born the hatch time;
" max-charm/min-charm the fixnum rails)
syn keyword LoveConst true false e pi tau i born max-charm min-charm

" Quoted atoms: 'foo   (' is one-operand \ = quote)
syn match LoveAtomMark "'"
syn match LoveAtom "'[^ \t\n()`',;#\"]\+" contains=LoveAtomMark

" Reader mark: ` is the list ctor (evaluates each element)
syn match LoveListCtor "`"

" Operator sigils -- a punct run that LEADS a token. The reader splits only a
" leading run; punct inside a name is just the name (turn, done?, a*b
" stay plain -- the lookbehind enforces it). The valence law gives a leading
" run two lives, and the colour shows which:
"   SPACED -- dyadic/infix, or the operator as a value: (+ 1 2), a + b, (+)
"   GLUED  -- monadic, fused to its datum: $x  !x  <>v  +'(1 2 3)  ~(0 0)
" glued means the FULL run touches a datum start (anything but whitespace,
" the closers, and more sigil chars -- the last so the greedy run can't
" backtrack and split itself: spaced != stays one dyad). the mono match is
" defined after the dyad so it wins when glued, and LoveNumber/LoveFloat --
" later still -- keep the sign/decimal of -3, .5.
" (the runtime tables: love/prel.l operators/monadics, book-private post-egg.)
syn match LoveSigilDyad "[A-Za-z0-9_]\@<![@#$~.!?%^*+/<>=|&-]\+"
syn match LoveSigilMono "[A-Za-z0-9_]\@<![@#$~.!?%^*+/<>=|&-]\+\ze[^ \t),;@#$~.!?%^*+/<>=|&-]"

" Numbers (integer / bignum literals, possibly negative)
syn match LoveNumber "\<-\?\d\+\>"

" Floating point literals: 1.5  -1.5  .5  1.  1e10  1.5e-3  (a point and/or exponent)
" Defined after LoveNumber so a float wins over the integer match at a shared start.
syn match LoveFloat "\<-\?\d\+\.\d*\([eE][-+]\?\d\+\)\?\>"
syn match LoveFloat "\<-\?\.\d\+\([eE][-+]\?\d\+\)\?\>"
syn match LoveFloat "\<-\?\d\+[eE][-+]\?\d\+\>"

" Strings
syn region LoveString start='"' skip='\\\\\|\\"' end='"'

" Comments — ; to end of line, #! shebang; with TODO/FIXME highlighting inside
syn match LoveCommentTodo /\<\(TODO\|FIXME\|NOTE\|XXX\|HACK\)\>/ contained
syn match LoveComment ";.*$" contains=LoveCommentTodo
syn match LoveComment "#!.*$" contains=LoveCommentTodo

" Unmatched close paren is an error
syn match LoveParenError ")"

syn sync lines=100

hi def link LoveAtomMark       Delimiter
hi def link LoveSigilDyad      Operator
hi def link LoveSigilMono      Special
hi def link LoveAtom           Identifier
hi def link LoveComment        Comment
hi def link LoveCommentTodo    Todo
hi def link LoveForm           Statement
hi def link LoveFunc           Function
hi def link LoveMacro          Operator
hi def link LoveConst          Constant
hi def link LoveListCtor       Special
hi def link LoveNumber         Number
hi def link LoveFloat          Float
hi def link LoveParenError     Error
hi def link LoveString         String
hi def link LoveBool           Boolean

" Rainbow parentheses — each nesting level gets its own colour.
" Each region contains the cluster plus the next level; level 9 wraps to 0.
" Toggle with \r (or :LoveRainbow) — controlled by g:love_rainbow (default: 1).
syn cluster LoveListCluster contains=LoveAtom,LoveAtomMark,LoveConst,LoveComment,LoveCommentTodo,LoveFunc,LoveNumber,LoveFloat,LoveForm,LoveString,LoveMacro,LoveListCtor,LoveSigilDyad,LoveSigilMono

if !exists("g:love_rainbow")
  let g:love_rainbow = 0
endif

if g:love_rainbow
  syn region LoveList0 matchgroup=LoveLevel0 start="(" end=")" contains=@LoveListCluster,LoveList1
  syn region LoveList1 matchgroup=LoveLevel1 start="(" end=")" contains=@LoveListCluster,LoveList2
  syn region LoveList2 matchgroup=LoveLevel2 start="(" end=")" contains=@LoveListCluster,LoveList3
  syn region LoveList3 matchgroup=LoveLevel3 start="(" end=")" contains=@LoveListCluster,LoveList4
  syn region LoveList4 matchgroup=LoveLevel4 start="(" end=")" contains=@LoveListCluster,LoveList5
  syn region LoveList5 matchgroup=LoveLevel5 start="(" end=")" contains=@LoveListCluster,LoveList6
  syn region LoveList6 matchgroup=LoveLevel6 start="(" end=")" contains=@LoveListCluster,LoveList7
  syn region LoveList7 matchgroup=LoveLevel7 start="(" end=")" contains=@LoveListCluster,LoveList8
  syn region LoveList8 matchgroup=LoveLevel8 start="(" end=")" contains=@LoveListCluster,LoveList9
  syn region LoveList9 matchgroup=LoveLevel9 start="(" end=")" contains=@LoveListCluster,LoveList0

  if &background ==# "dark"
    hi def LoveLevel0 ctermfg=red         guifg=red1
    hi def LoveLevel1 ctermfg=yellow      guifg=orange1
    hi def LoveLevel2 ctermfg=green       guifg=yellow1
    hi def LoveLevel3 ctermfg=cyan        guifg=greenyellow
    hi def LoveLevel4 ctermfg=magenta     guifg=green1
    hi def LoveLevel5 ctermfg=red         guifg=springgreen1
    hi def LoveLevel6 ctermfg=yellow      guifg=cyan1
    hi def LoveLevel7 ctermfg=green       guifg=slateblue1
    hi def LoveLevel8 ctermfg=cyan        guifg=magenta1
    hi def LoveLevel9 ctermfg=magenta     guifg=purple1
  else
    hi def LoveLevel0 ctermfg=red         guifg=red3
    hi def LoveLevel1 ctermfg=darkyellow  guifg=orangered3
    hi def LoveLevel2 ctermfg=darkgreen   guifg=orange2
    hi def LoveLevel3 ctermfg=blue        guifg=yellow3
    hi def LoveLevel4 ctermfg=darkmagenta guifg=olivedrab4
    hi def LoveLevel5 ctermfg=red         guifg=green4
    hi def LoveLevel6 ctermfg=darkyellow  guifg=paleturquoise3
    hi def LoveLevel7 ctermfg=darkgreen   guifg=deepskyblue4
    hi def LoveLevel8 ctermfg=blue        guifg=darkslateblue
    hi def LoveLevel9 ctermfg=darkmagenta guifg=darkviolet
  endif
else
  syn region LoveList matchgroup=LoveParen start="(" end=")" contains=@LoveListCluster,LoveList
  hi def link LoveParen Delimiter
endif

let b:current_syntax = "love"
