" vim syntax for p
" based on lisp.vim by Charles E Campbell <http://www.drchip.org/astronaut/vim/index.html#SYNTAX_LISP>
if exists("b:current_syntax")
  finish
endif
syn cluster PListCluster contains=PAtom,PAtomMark,PComment,PTodo,PFunc,PList,PNumber,PSymbol,PForm,PString,PMacro
syn match PSymbol contained ![^()"; \t]\+!
syn match PAtomMark "'"
syn match PAtom "'[^ \t()]\+" contains=PAtomMark
" FIXME highlight signs right
"syn match PNumber "\(+\|-\)*\(0b\|0B\)\(\.[01]\+\|[01]\+\(\.[01]*\)\=\)"
"syn match PNumber "\(+\|-\)*\(0o\|0O\)\(\.\o\+\|\o\+\(\.\o*\)\=\)"
"syn match PNumber "\(+\|-\)*\(0z\|0Z\)\(\.[0-9abAB]\+\|[0-9abAB]\+\(\.[0-9abAB]*\)\=\)"
"syn match PNumber "\(+\|-\)\?\(0x\|0X\)\(\.\x\+\|\x\+\(\.\x*\)\=\)"
syn region PAtom start=+'"+ skip=+\\"+ end=+"+
syn iskeyword @,!,37-38,42-47,:,60-63,\,`,|,~,^
syn keyword PForm ? : \\ , `
syn keyword PFunc < <= = >= >
syn keyword PFunc + - ~ ! * / % .
syn keyword PFunc X A B cons list car cdr caar cadr cdar cddr
syn keyword PFunc foldl foldr map filter id const append each all any init last reverse
syn keyword PFunc && \|\| \| & ^ << >>
syn keyword PFunc :: inc dec flip diag partition llen iota puts
syn keyword PFunc twop nump symp tblp strp nilp homp ev ap not
syn keyword PFunc str slen sget scat ssub ystr gensym putc
syn keyword PFunc tbl tget tset tlen thas tkeys tdel
syn keyword PMacro L vprintf

syn region PString start='"' skip='\\\\\|\\"' end='"'


syn match PParenError ")"

syn match PComment ";.*$" contains=PTodo
syn match PTodo "\(#.*$\|XXX\|TODO\|FIXME\)"
syn match PNumber "-\?\(\d\+\)"

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
hi def link PTodo           Todo

syn region PList matchgroup=PParen start="(" matchgroup=PParen end=")" contains=@PListCluster
hi def link PParen Delimiter

let b:current_syntax = "p"
