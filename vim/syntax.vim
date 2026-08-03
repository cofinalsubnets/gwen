" vim syntax for love (.l) -- GENERATED. do not edit.
"
"   cat crew/vi/config.l crew/vi/hue.l tools/hue2vim.l | out/host/love > vim/syntax.vim
"
" the source is crew/vi/hue.l's class table -- the same table pulchritude's own
" painter reads, so the editor in the tree and the editor outside it agree by
" construction -- plus the vocabulary the running binary answers to. `make -C
" tools test_hue` regenerates this file and diffs it, so a rename that moves
" (names ()) fails the gate until it is re-run.
" based on lisp.vim by Charles E Campbell <http://www.drchip.org/astronaut/vim/index.html#SYNTAX_LISP>
if exists("b:current_syntax")
  finish
endif

" symbol-constituent chars (the reader ends a token only on whitespace and
" ( ) " ' ` , # ; ). operators @ # $ are excluded so they highlight standalone.
syn iskeyword @,33,37,38,42-43,45-47,48-57,58,60-63,92,94-95,124,126

" vim's one nicety with no counterpart in the painter: TODO words inside a
" comment. contained, so it fires only inside the comment lane.
syn match LoveTodo /\<\(TODO\|FIXME\|NOTE\|XXX\|HACK\)\>/ contained

" the classes, in REVERSE table order: vim's last match wins, hue-lex's first
" row wins, and reversing makes the two agree.
syn match LoveDyad "[A-Za-z0-9_]\@<![@#$~.!?%\^*+/<>=|&\-]\+"
syn match LoveMono "[A-Za-z0-9_]\@<![@#$~.!?%\^*+/<>=|&\-]\+\ze[^ \t),;@#$~.!?%\^*+/<>=|&\-]"
syn keyword LoveBook :: BRIDGE CMEM GLOB HELPC SRC aall abs
syn keyword LoveBook accept all any ap apcap apof apover arg
syn keyword LoveBook array assoc atom? await bake bao big? bit
syn keyword LoveBook blit blitrow bound? bridge caaap caap caaup call-cc
syn keyword LoveBook calloutdrive calloutresume cap capp cask cat catch catmap
syn keyword LoveBook cauap caup cauup charm? chdir chk chmod chown
syn keyword LoveBook clock close co coin coin? compose conj connect
syn keyword LoveBook connectu const constellation? conv conv-args cosine cuaap cuap
syn keyword LoveBook cuaup cuda-avail cuda-ew cuda-gemm cuda-reduce cuda-transp cue? cup
syn keyword LoveBook cuuap cuup cuuup cwd dict_has dict_set die-of dig
syn keyword LoveBook dobr docr doer dojr donr dop1 dop2 dot
syn keyword LoveBook dour drink drop dup dup2 each edln edlnc
syn keyword LoveBook edraw environ err est ev exec ext_s fdclose
syn keyword LoveBook fdopen filter fired? fires fixity flip flow flush
syn keyword LoveBook foldl foldl1 foldr foldr1 font fork forms fraction
syn keyword LoveBook freeze from g-die galaxy? gather gauge gaze gcd
syn keyword LoveBook gem gem-tray gem? getc getenv getpid getuid glass
syn keyword LoveBook glean gulp hardlink hark has_s hear heard herald
syn keyword LoveBook hot? id? im in inf inf-spine inf-sym init
syn keyword LoveBook inner int intern ioctl iota jot jug kbrec
syn keyword LoveBook kchain kconst kcrec keys ki1 ki2 knrec kp1
syn keyword LoveBook kp2 krefl kseq ksucc landed? last link listen
syn keyword LoveBook lit? load lof log lseek macro-names map mapfd
syn keyword LoveBook mapfdo mapin mapout max member? memfd merge min
syn keyword LoveBook mint mintp mkdir modpow monofix monoid mount name?
syn keyword LoveBook names natjit nclock negate net newns nil? nom
syn keyword LoveBook nom? num-ap once one? open openfd opfix out
syn keyword LoveBook outer ov-check ov-eng ov-hook ov-slot overlay overlay-off overlay-set
syn keyword LoveBook part peep peepw pin pinw pipe please port?
syn keyword LoveBook pour powover print prod ptyecho pull put putbn
syn keyword LoveBook putc putn puts putx q qlit query quit
syn keyword LoveBook rand randf random rank raw re read readdir
syn keyword LoveBook readlink reads reciprocal rejects rel rename reply rest
syn keyword LoveBook rev revcat rewrite ring rmdir rng-box rng-get rng-set
syn keyword LoveBook sX s_plus s_star sat saturate say scare scare?
syn keyword LoveBook scoop screen scribe seal see setenv setwinsize sfold
syn keyword LoveBook sha256 shape shell shore show sigfd signal sigtake
syn keyword LoveBook sine sip slist slurp smap snip sno sort
syn keyword LoveBook sortby sound sound0 spawn spawnio spawnmap spread stack
syn keyword LoveBook stake star-tray star? stat still string string? subst
syn keyword LoveBook sun sun? swig symlink tablet tablet? take tally
syn keyword LoveBook tangent tap tether tier top-tray tray? trickle ttyfg
syn keyword LoveBook turn turnf twin twin-tray twin? twirl two? u2
syn keyword LoveBook u2app udp-bind udp-recv udp-send ufail? uglo umask unfold
syn keyword LoveBook unify unlink unmap unsee use utime uu uu2l
syn keyword LoveBook vapp var var? vof vof-spine vof-sym vsucc wait
syn keyword LoveBook walk welp wet wheel whole? winsize wl-recv wl-send
syn keyword LoveBook wrap zero? zip
syn keyword LoveConst C Q born e false i id love-arch
syn keyword LoveConst love-tco love-version max-charm min-charm pi tau torn true
syn keyword LoveConst ufail
syn keyword LoveMacro && , :- <=< >>= ?- L \\\\
syn keyword LoveMacro assert axm begin cond defn defq do et
syn keyword LoveMacro hash if let list pins progn quote suite
syn keyword LoveMacro tuple vel zz \|\|
syn keyword LoveForm : ? \\
syn match LoveNumber "\<-\?\d\+\>"
syn match LoveNumber "\<-\?\d\+\.\d*\([eE][-+]\?\d\+\)\?\>"
syn match LoveNumber "\<-\?\.\d\+\([eE][-+]\?\d\+\)\?\>"
syn match LoveNumber "\<-\?\d\+[eE][-+]\?\d\+\>"
syn match LoveCtor "`"
syn match LoveAtom "'[^ \t\n()[\]{}\"'`;,]*"
syn region LoveString start='"' skip='\\\\\|\\"' end='"'
syn match LoveComment ";.*$" contains=LoveTodo
syn match LoveComment "#!.*$" contains=LoveTodo

" a close paren with nothing open is an error -- vim-only; lib/lint.l is where
" the tree actually answers that question.
syn match LoveParenError ")"

syn sync lines=100

hi def link LoveComment    Comment
hi def link LoveString     String
hi def link LoveAtom       Identifier
hi def link LoveCtor       Special
hi def link LoveParen      Delimiter
hi def link LoveNumber     Number
hi def link LoveForm       Statement
hi def link LoveMacro      Operator
hi def link LoveConst      Constant
hi def link LoveBook       Function
hi def link LoveMono       Special
hi def link LoveDyad       Operator
hi def link LoveTodo       Todo
hi def link LoveParenError Error

" Rainbow parentheses -- each nesting level gets its own colour, level 9 wraps
" to 0. Toggle with g:love_rainbow (default: off, one flat Delimiter).
syn cluster LoveListCluster contains=LoveComment,LoveString,LoveAtom,LoveCtor,LoveNumber,LoveForm,LoveMacro,LoveConst,LoveBook,LoveMono,LoveDyad,LoveTodo

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
    hi def LoveLevel0 ctermfg=red          guifg=red1
    hi def LoveLevel1 ctermfg=yellow       guifg=orange1
    hi def LoveLevel2 ctermfg=green        guifg=yellow1
    hi def LoveLevel3 ctermfg=cyan         guifg=greenyellow
    hi def LoveLevel4 ctermfg=magenta      guifg=green1
    hi def LoveLevel5 ctermfg=red          guifg=springgreen1
    hi def LoveLevel6 ctermfg=yellow       guifg=cyan1
    hi def LoveLevel7 ctermfg=green        guifg=slateblue1
    hi def LoveLevel8 ctermfg=cyan         guifg=magenta1
    hi def LoveLevel9 ctermfg=magenta      guifg=purple1
  else
    hi def LoveLevel0 ctermfg=red          guifg=red3
    hi def LoveLevel1 ctermfg=darkyellow   guifg=orangered3
    hi def LoveLevel2 ctermfg=darkgreen    guifg=orange2
    hi def LoveLevel3 ctermfg=blue         guifg=yellow3
    hi def LoveLevel4 ctermfg=darkmagenta  guifg=olivedrab4
    hi def LoveLevel5 ctermfg=red          guifg=green4
    hi def LoveLevel6 ctermfg=darkyellow   guifg=paleturquoise3
    hi def LoveLevel7 ctermfg=darkgreen    guifg=deepskyblue4
    hi def LoveLevel8 ctermfg=blue         guifg=darkslateblue
    hi def LoveLevel9 ctermfg=darkmagenta  guifg=darkviolet
  endif
else
  syn region LoveList matchgroup=LoveParen start="(" end=")" contains=@LoveListCluster,LoveList
endif

let b:current_syntax = "love"
