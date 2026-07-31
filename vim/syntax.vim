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
syn keyword LoveBook accept all alt any ap apcap apof apover
syn keyword LoveBook arg array assoc atom? await back? bake bao
syn keyword LoveBook big? bit blit blitrow bound? bridge caaap caap
syn keyword LoveBook caaup call-cc calloutdrive calloutresume cap capp cask cat
syn keyword LoveBook catch catmap cauap caup cauup charm? chdir chk
syn keyword LoveBook chmod chown clock close co coin coin? compose
syn keyword LoveBook conj connect connectu const constellation? conv conv-args cosine
syn keyword LoveBook cuaap cuap cuaup cuda-avail cuda-ew cuda-gemm cuda-reduce cuda-transp
syn keyword LoveBook cue? cup cuuap cuup cuuup cwd dict_has dict_set
syn keyword LoveBook die-of dig dobr docr doer dojr donr dop1
syn keyword LoveBook dop2 dot dour drink drop dup dup2 each
syn keyword LoveBook edln edlnc edraw empty? environ eps err est
syn keyword LoveBook ev exec ext_s fdclose fdopen filter fired? fires
syn keyword LoveBook fixity flip flow flush foldl foldl1 foldr foldr1
syn keyword LoveBook font fork forms fraction freeze from g-die galaxy?
syn keyword LoveBook gather gauge gaze gcd gem gem-tray gem? getc
syn keyword LoveBook getenv getpid getuid glass glaze gulp hardlink has_s
syn keyword LoveBook hear help hot? id? im in inf inf-spine
syn keyword LoveBook inf-sym init inner int intern ioctl iota jot
syn keyword LoveBook jug kbrec kchain kconst kcrec keys ki1 ki2
syn keyword LoveBook knrec kp1 kp2 krefl kseq ksucc last link
syn keyword LoveBook listen lit lit? load lof log lseek macro-names
syn keyword LoveBook many map mapfd mapfdo mapin mapout max member?
syn keyword LoveBook memfd merge min mind mint mintp missing mkdir
syn keyword LoveBook modpow monofix monoid mount name? names natjit nclock
syn keyword LoveBook negate net newns nil? nom nom? num-ap once
syn keyword LoveBook one one? open openfd opfix opt out outer
syn keyword LoveBook ov-check ov-eng ov-hook ov-slot overlay overlay-off overlay-set parse
syn keyword LoveBook part peep peepw pin pinw pipe please pmap
syn keyword LoveBook pour powover print prod ptyecho pull put putbn
syn keyword LoveBook putc putn puts putx q qlit query quit
syn keyword LoveBook rand randf random rank raw re read readdir
syn keyword LoveBook readlink reads reciprocal rejects rel rename reply rest
syn keyword LoveBook rev revcat rewrite ring rmdir rng-box rng-get rng-set
syn keyword LoveBook run runp runt sX s_plus s_star sat saturate
syn keyword LoveBook say scare scare? screen scribe seal see seq
syn keyword LoveBook seqs setenv setwinsize sfold sha256 shape shell shore
syn keyword LoveBook show sigfd signal sigtake sine sip skip slist
syn keyword LoveBook slurp smap snip sno sort sortby sound sound0
syn keyword LoveBook span spawn spawnio spawnmap spread stack stake star-tray
syn keyword LoveBook star? stat still string string? subst sun sun?
syn keyword LoveBook swig symlink tablet tablet? take tally tangent tap
syn keyword LoveBook tier top-tray tray? ttyfg turn turnf twin twin-tray
syn keyword LoveBook twin? twirl two? u2 u2app udp-bind udp-recv udp-send
syn keyword LoveBook ufail? uglo umask unfold unify unlink unmap unsee
syn keyword LoveBook use utime uu uu2l vapp var var? vof
syn keyword LoveBook vof-spine vof-sym vsucc wait walk welp wet wheel
syn keyword LoveBook whole? winsize wl-recv wl-send wrap zero? zip
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
