" vim syntax for p
" based on lisp.vim by Charles E Campbell <http://www.drchip.org/astronaut/vim/index.html#SYNTAX_LISP>
if exists("b:current_syntax")
  finish
endif
syn match PSymbol contained ![^()"; \t]\+!
syn match PAtomMark "'"
syn match PAtom "'[^ \t()]\+" contains=PAtomMark
syn iskeyword @,!,37-38,42-47,:,60-63,\,`,|,~,^
syn keyword PForm ? : \\ , ` :-
syn keyword PFunc < <= = >= > !=
syn keyword PFunc + - ~ ! * / % .
syn keyword PFunc X A B AA AB BA BB car cdr cons caar cadr cdar cddr
syn keyword PFunc foldl foldr map filter id const cat each all any init last rev
syn keyword PFunc && \|\| \| & ^ << >>
syn keyword PFunc :: inc dec flip diag part llen iota puts
syn keyword PFunc twop nump symp tblp strp nilp homp ev not
syn keyword PFunc str slen sget scat ssub ystr sym putc co atomp
syn keyword PFunc tnew tget tset tlen thas tkeys tdel memq assq
syn keyword PMacro L vprintf >>= \|> >=> <=<
syn keyword PBool true false

syn region PString start='"' skip='\\\\\|\\"' end='"'

syn match PParenError ")"

syn match PComment ";.*$"
syn match PTodo "#.*$"
syn match PNumber "\(-\d\+\|\d\+\)"

syn sync lines=100

hi def link PAtomMark       Delimiter
hi def link PAtom           Identifier
hi def link PComment        Comment
hi def link PForm           Statement
hi def link PFunc           Function
hi def link PMacro          Operator
hi def link PNumber         Number
hi def link PParenError     Error
hi def link PString         String
hi def link PTodo           Debug
hi def link PBool           Boolean
hi def link PParen          Delimiter

syn cluster PListCluster contains=PAtom,PAtomMark,PComment,PTodo,PFunc,PList,PNumber,PSymbol,PForm,PString,PMacro
syn region PList matchgroup=PParen start="(" matchgroup=PParen end=")" contains=@PListCluster

let b:current_syntax = "p"
