" vim syntax for gwen
" based on lisp.vim by Charles E Campbell <http://www.drchip.org/astronaut/vim/index.html#SYNTAX_LISP>
if exists("b:current_syntax")
  finish
endif
syn cluster GwenListCluster contains=GwenAtom,GwenAtomMark,GwenComment,GwenTodo,GwenFunc,GwenList,GwenNumber,GwenSymbol,GwenForm,GwenString,GwenMacro
syn match GwenSymbol contained ![^()"; \t]\+!
syn match GwenAtomMark "'"
syn match GwenAtom "'[^ \t()]\+" contains=GwenAtomMark
" FIXME highlight signs right
"syn match GwenNumber "\(+\|-\)*\(0b\|0B\)\(\.[01]\+\|[01]\+\(\.[01]*\)\=\)"
"syn match GwenNumber "\(+\|-\)*\(0o\|0O\)\(\.\o\+\|\o\+\(\.\o*\)\=\)"
"syn match GwenNumber "\(+\|-\)*\(0z\|0Z\)\(\.[0-9abAB]\+\|[0-9abAB]\+\(\.[0-9abAB]*\)\=\)"
"syn match GwenNumber "\(+\|-\)\?\(0x\|0X\)\(\.\x\+\|\x\+\(\.\x*\)\=\)"
syn region GwenAtom start=+'"+ skip=+\\"+ end=+"+
syn iskeyword @,!,37-38,42-47,:,60-63,\,`,|,~,^
syn keyword GwenForm ? : \\ , `
syn keyword GwenFunc < <= = >= >
syn keyword GwenFunc + - ~ ! * / % .
syn keyword GwenFunc X A B cons list car cdr caar cadr cdar cddr
syn keyword GwenFunc foldl foldr map filter id const append each all any init last reverse
syn keyword GwenFunc && \|\| \| & ^ << >>
syn keyword GwenFunc :: inc dec flip diag partition llen iota puts
syn keyword GwenFunc twop nump symp tblp strp nilp homp ev ap not
syn keyword GwenFunc str slen sget scat ssub ystr gensym putc
syn keyword GwenFunc tbl tget tset tlen thas tkeys tdel
syn keyword GwenMacro L vprintf

syn region GwenString start='"' skip='\\\\\|\\"' end='"'


syn match GwenParenError ")"

syn match GwenComment ";.*$" contains=GwenTodo
syn match GwenTodo "\(#.*$\|XXX\|TODO\|FIXME\)"
syn match GwenNumber "-\?\(\d\+\)"

syn sync lines=100

hi def link GwenAtomMark       Delimiter
hi def link GwenAtom           Identifier
hi def link GwenComment        Comment
hi def link GwenForm           Statement
hi def link GwenFunc           Function
hi def link GwenMacro          Operator
hi def link GwenNumber         Number
hi def link GwenParenError     Error
hi def link GwenString         String
hi def link GwenTodo           Todo

syn region GwenList matchgroup=GwenParen start="(" matchgroup=GwenParen end=")" contains=@GwenListCluster
hi def link GwenParen Delimiter

let b:current_syntax = "gwen"
