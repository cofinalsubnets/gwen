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
syn keyword LoveBook bound? bridge caaap caap caaup call-cc calloutdrive calloutresume
syn keyword LoveBook cap capp cask cat catch catmap cauap caup
syn keyword LoveBook cauup chacha20 charm? chdir chk chmod chown chug
syn keyword LoveBook clock close co coin coin? compose conj connect
syn keyword LoveBook connectu const constellation? conv conv-args cosine cuaap cuap
syn keyword LoveBook cuaup cue? cup cuuap cuup cuuup cwd dict_has
syn keyword LoveBook dict_set die-of dig dobr docr doer dojr donr
syn keyword LoveBook dop1 dop2 dot dour drop dup dup2 each
syn keyword LoveBook edln edlnc edraw environ err est ev exec
syn keyword LoveBook ext_s fdclose fdopen filter fired? fires fixity flip
syn keyword LoveBook flow flush foldl foldl1 foldr foldr1 fork forms
syn keyword LoveBook fraction freeze from g-die galaxy? gather gauge gaze
syn keyword LoveBook gcd gem gem-tray gem? getc getenv getpid getuid
syn keyword LoveBook glass glean hardlink hark has_s hear heard herald
syn keyword LoveBook hot? id? im in inf inf-spine inf-sym init
syn keyword LoveBook inner int intern iota jot jug kbrec kchain
syn keyword LoveBook kconst kcrec keys ki1 ki2 knrec kp1 kp2
syn keyword LoveBook krefl kseq ksucc landed? last link link? listen
syn keyword LoveBook lit? load lof log lseek macro-names map max
syn keyword LoveBook member? merge min mint mint? mintp mkdir modpow
syn keyword LoveBook monofix monoid mount name name? names natjit nclock
syn keyword LoveBook negate net newns nil? nom nom? num-ap once
syn keyword LoveBook one? open openfd opfix out outer ov-check ov-eng
syn keyword LoveBook ov-hook ov-slot overlay overlay-off overlay-set part peep peepw
syn keyword LoveBook pin pinw pipe please poly1305 port? pour powover
syn keyword LoveBook print prod ptyecho pull put putbn putc putn
syn keyword LoveBook puts putx px q qlit query quit rand
syn keyword LoveBook randf random rank raw re read readdir readlink
syn keyword LoveBook reads reciprocal rejects rel rename reply rest rev
syn keyword LoveBook revcat rewrite ring rmdir rng-box rng-get rng-set sX
syn keyword LoveBook s_plus s_star sat saturate say scare scoop screen
syn keyword LoveBook scribe seal see setenv setwinsize sfold sha256 shape
syn keyword LoveBook shell shore show sigfd signal sigtake sine slist
syn keyword LoveBook slurp smap snip sno sort sortby sound sound0
syn keyword LoveBook spawn spawnio spawnmap spread stack stake star-tray star?
syn keyword LoveBook stat still string string? subst sun sun? swig
syn keyword LoveBook symlink tablet tablet? take tally tangent tap tether
syn keyword LoveBook tier top-tray trap tray? trickle ttyfg turn turnf
syn keyword LoveBook twin twin-tray twin? twirl two? u2 u2app udp-bind
syn keyword LoveBook udp-recv udp-send ufail? uglo umask unfold unify unlink
syn keyword LoveBook unsee use utime uu uu2l vapp var var?
syn keyword LoveBook vof vof-spine vof-sym vsucc wait walk welp wet
syn keyword LoveBook wheel whole? winsize wrap zero? zip
syn keyword LoveConst C Q born e false i id love-arch
syn keyword LoveConst love-tco love-version max-charm min-charm pi tau true ufail
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
