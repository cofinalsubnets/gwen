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
syn keyword LoveBook BRIDGE C CMEM GLOB HELPC Q SRC aall
syn keyword LoveBook abs accept all alt any ap apcap apof
syn keyword LoveBook apover arg argv array assoc atom? await axm
syn keyword LoveBook back? bake bao big? bit blit blitrow bound?
syn keyword LoveBook bridge caaap caap caaup call-cc calloutdrive calloutresume cap
syn keyword LoveBook capp cask cat catch catmap cauap caup cauup
syn keyword LoveBook charm? chdir chk chmod chown clock close cmdline
syn keyword LoveBook co coin coin? compose conj connect connectu const
syn keyword LoveBook constellation? conv conv-args cosine cuaap cuap cuaup cuda-avail
syn keyword LoveBook cuda-ew cuda-gemm cuda-reduce cuda-transp cue? cup cuuap cuup
syn keyword LoveBook cuuup cwd defn defq dict_has dict_set die-of dig
syn keyword LoveBook dobr docr doer dojr donr dop1 dop2 dot
syn keyword LoveBook dour drop dup dup2 each edln edlnc edraw
syn keyword LoveBook empty? empty_dict environ eof? eps err est ev
syn keyword LoveBook exec ext_s fdclose fdopen filter fired? fires fixity
syn keyword LoveBook flip flush foldl foldl1 foldr foldr1 font fork
syn keyword LoveBook fraction freeze from g-die galaxy? gather gauge gaze
syn keyword LoveBook gcd gem gem-tray gem? getc getenv getpid getuid
syn keyword LoveBook glass glaze hardlink has-top-op? has_s hear hot? id
syn keyword LoveBook id? im in inf inf-spine inf-sym init inner
syn keyword LoveBook int intern ioctl iota jot jug kbrec kchain
syn keyword LoveBook kconst kcrec keys ki1 ki2 knrec kp1 kp2
syn keyword LoveBook krefl kseq ksucc last link listen lit lit?
syn keyword LoveBook load lof log love-arch love-tco love-version lseek many
syn keyword LoveBook map mapfd mapfdo mapin mapout max member? memfd
syn keyword LoveBook merge min mind mint mintp missing mkdir modpow
syn keyword LoveBook monofix monoid more? mount name? names natjit nclock
syn keyword LoveBook negate net newns nil? nom nom? num-ap one
syn keyword LoveBook one? op-span open openfd opfix opt out outer
syn keyword LoveBook ov-both ov-check ov-eng ov-hook ov-slot overlay overlay-off overlay-set
syn keyword LoveBook parse part peep peepw pin pinw pipe please
syn keyword LoveBook pmap pour powover print prod ptyecho pull put
syn keyword LoveBook putbn putc putn puts putx q qlit query
syn keyword LoveBook quit rand randf random rank raw re read
syn keyword LoveBook readdir readlink reads reciprocal rejects rel rename reply
syn keyword LoveBook rest rev revcat rewrite ring rmdir rng-box rng-get
syn keyword LoveBook rng-set run runp runt sX s_plus s_star sat
syn keyword LoveBook saturate say scare scare? screen scribe seal see
syn keyword LoveBook seq seqs setenv setwinsize sfold sha256 shape shell
syn keyword LoveBook shore show sigfd signal sigtake sine sip skip
syn keyword LoveBook slist slurp smap snip sno sort sortby sound
syn keyword LoveBook span spawn spawnio spawnmap spread stack stake star-tray
syn keyword LoveBook star? stat still string string? subst sun sun?
syn keyword LoveBook swig symlink tablet tablet? take tally tangent tap
syn keyword LoveBook tier top-tray tray? ttyfg turn turnf twin twin-tray
syn keyword LoveBook twin? twirl two? u2 u2app udp-bind udp-recv udp-send
syn keyword LoveBook ufail ufail? uglo umask unfold unify unlink unmap
syn keyword LoveBook unsee use utime uu uu2l vapp var var?
syn keyword LoveBook verbs vof vof-spine vof-sym vsucc wait walk welp
syn keyword LoveBook wet wheel whole? winsize wl-recv wl-send wrap zero?
syn keyword LoveBook zip
syn keyword LoveConst true false e pi tau i born max-charm
syn keyword LoveConst min-charm
syn keyword LoveMacro :: L list do begin progn let if
syn keyword LoveMacro cond quote tuple hash pins assert suite &&
syn keyword LoveMacro \|\| :- ?- >>= <=< zz et vel
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
