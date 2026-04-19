" vim syntax for gwen lisp (.g)
" based on lisp.vim by Charles E Campbell <http://www.drchip.org/astronaut/vim/index.html#SYNTAX_LISP>
if exists("b:current_syntax")
  finish
endif

syn iskeyword @,!,37-38,42-57,:,60-63,\,`,|,~,^

" The four special forms: cond, let, quote, lambda
syn keyword PForm ? : \\ `

" Built-in functions
syn keyword PFunc X A B AA AB BA BB car cdr cons
syn keyword PFunc caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr
syn keyword PFunc foldl foldr foldl1 foldr1 map filter id const cat each
syn keyword PFunc all any init last rev take drop catmap
syn keyword PFunc inc dec flip diag part len ldel puts zip lidx memq assq
syn keyword PFunc twop nump symp nomp tblp strp nilp homp ev not atomp
syn keyword PFunc str scat ssub sym nom putc co
syn keyword PFunc tnew thas tkeys tdel set put get new
syn keyword PFunc :: < <= = >= > != + - ~ ! * / % .
syn keyword PFunc assert

" Macros
syn keyword PMacro L list vprintf >>= >=> <=< :- ?- ,
syn keyword PMacro && \|\| \| & ^ << >>

" Boolean constants
syn keyword PBool true false

" Wildcard / throwaway binding
syn match PWild /\<_\>/

" Quoted atoms: 'foo
syn match PAtomMark "'"
syn match PAtom "'[^ \t()]\+" contains=PAtomMark

" Numbers (integer literals, possibly negative)
syn match PNumber "\(-\d\+\|\d\+\)"

" Strings
syn region PString start='"' skip='\\\\\|\\"' end='"'

" Comments — with TODO/FIXME highlighting inside
syn match PCommentTodo /\<\(TODO\|FIXME\|NOTE\|XXX\|HACK\)\>/ contained
syn match PComment ";.*$" contains=PCommentTodo

" Unmatched close paren is an error
syn match PParenError ")"

syn sync lines=100

hi def link PAtomMark       Delimiter
hi def link PAtom           Identifier
hi def link PComment        Comment
hi def link PCommentTodo    Todo
hi def link PForm           Statement
hi def link PFunc           Function
hi def link PMacro          Operator
hi def link PNumber         Number
hi def link PParenError     Error
hi def link PString         String
hi def link PBool           Boolean
hi def link PWild           Special

" Rainbow parentheses — each nesting level gets its own colour.
" Each region contains the cluster plus the next level; level 9 wraps to 0.
" Toggle with \r (or :GwRainbow) — controlled by g:gw_rainbow (default: 1).
syn cluster PListCluster contains=PAtom,PAtomMark,PBool,PWild,PComment,PCommentTodo,PFunc,PNumber,PSymbol,PForm,PString,PMacro

if !exists("g:gw_rainbow")
  let g:gw_rainbow = 0
endif

if g:gw_rainbow
  syn region PList0 matchgroup=PLevel0 start="(" end=")" contains=@PListCluster,PList1
  syn region PList1 matchgroup=PLevel1 start="(" end=")" contains=@PListCluster,PList2
  syn region PList2 matchgroup=PLevel2 start="(" end=")" contains=@PListCluster,PList3
  syn region PList3 matchgroup=PLevel3 start="(" end=")" contains=@PListCluster,PList4
  syn region PList4 matchgroup=PLevel4 start="(" end=")" contains=@PListCluster,PList5
  syn region PList5 matchgroup=PLevel5 start="(" end=")" contains=@PListCluster,PList6
  syn region PList6 matchgroup=PLevel6 start="(" end=")" contains=@PListCluster,PList7
  syn region PList7 matchgroup=PLevel7 start="(" end=")" contains=@PListCluster,PList8
  syn region PList8 matchgroup=PLevel8 start="(" end=")" contains=@PListCluster,PList9
  syn region PList9 matchgroup=PLevel9 start="(" end=")" contains=@PListCluster,PList0

  if &background ==# "dark"
    hi def PLevel0 ctermfg=red         guifg=red1
    hi def PLevel1 ctermfg=yellow      guifg=orange1
    hi def PLevel2 ctermfg=green       guifg=yellow1
    hi def PLevel3 ctermfg=cyan        guifg=greenyellow
    hi def PLevel4 ctermfg=magenta     guifg=green1
    hi def PLevel5 ctermfg=red         guifg=springgreen1
    hi def PLevel6 ctermfg=yellow      guifg=cyan1
    hi def PLevel7 ctermfg=green       guifg=slateblue1
    hi def PLevel8 ctermfg=cyan        guifg=magenta1
    hi def PLevel9 ctermfg=magenta     guifg=purple1
  else
    hi def PLevel0 ctermfg=red         guifg=red3
    hi def PLevel1 ctermfg=darkyellow  guifg=orangered3
    hi def PLevel2 ctermfg=darkgreen   guifg=orange2
    hi def PLevel3 ctermfg=blue        guifg=yellow3
    hi def PLevel4 ctermfg=darkmagenta guifg=olivedrab4
    hi def PLevel5 ctermfg=red         guifg=green4
    hi def PLevel6 ctermfg=darkyellow  guifg=paleturquoise3
    hi def PLevel7 ctermfg=darkgreen   guifg=deepskyblue4
    hi def PLevel8 ctermfg=blue        guifg=darkslateblue
    hi def PLevel9 ctermfg=darkmagenta guifg=darkviolet
  endif
else
  syn region PList matchgroup=PParen start="(" end=")" contains=@PListCluster,PList
  hi def link PParen Delimiter
endif

let b:current_syntax = "gl"
