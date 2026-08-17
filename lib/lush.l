; lush 🐚, assembled: the shell state + job control, the lexer, the grammar,
; the glob, the word expander, the engine, the repl loops, then the entry +
; seat -- each part a (module 'lush form, so the parts register the one module
; and the loader keeps the file-made registration: (use 'lush) loads the lot.
(use 'lush/job)
(use 'lush/lex)
(use 'lush/gram)
(use 'lush/glob)
(use 'lush/word)
(use 'lush/eval)
(use 'lush/line)
(use 'lush/main)
