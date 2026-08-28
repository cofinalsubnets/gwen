; lush 🐚, assembled: the shell state + job control, the lexer, the grammar,
; the glob, the word expander, the engine, the repl loops, then the entry +
; seat -- each part a (module 'lush form, so the parts register the one module
; and the loader keeps the file-made registration: this file loads the lot.
(use "crew/lush/job.l")
(use "crew/lush/lex.l")
(use "crew/lush/gram.l")
(use "crew/lush/glob.l")
(use "crew/lush/word.l")
(use "crew/lush/eval.l")
(use "crew/lush/line.l")
(use "crew/lush/main.l")
