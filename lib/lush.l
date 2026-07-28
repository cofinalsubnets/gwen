; lush 🐚, assembled: the shell state + job control, the lexer, the grammar,
; the glob, the word expander, the engine, the repl loops, then the entry +
; seat -- all INCLUDED into this one layer (a slashed use evals into the
; current head), so (use 'lush) loads the lot and leave registers it whole.
(use 'lush/job)
(use 'lush/lex)
(use 'lush/gram)
(use 'lush/glob)
(use 'lush/word)
(use 'lush/eval)
(use 'lush/line)
(use 'lush/main)
