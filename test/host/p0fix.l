; test/host/p0fix.l -- p0's specimen. never evaluated (deliberately absent from
; hostnif_tests): it is TEXT, read by rdiff's A/B leg. p1.l and love/egg.l are the
; only .l files p0 reads for real, and they are two forms between them -- too thin
; to measure agreement on, so this spans the subset p0 owns on purpose.

; delimiters and nesting
(a) (a b) (a b c) (a (b) c) ((a) (b)) (((a))) (a (b (c (d (e)))))
(a b (c d) e (f (g h)) i) ((a b) (c d) (e f)) (a (b) (c) (d) (e))

; the zero point, and nothing in every position
() (()) (() ()) (a ()) (() a) (() () ())

; quote -- 'x is (\ x), and it nests
'a 'b '(a b) '(a (b c)) ''a '() '(())

; integers: decimal, and a digit after - is what starts a numeral
(0) (1) (-1) (42) (-42) (1234567890) (-1234567890)

; ... hexadecimal ...
(0x0) (0xff) (0xFF) (-0xff) (0xdeadbeef)

; ... and octal, iff [+-]?0[0-7]+
(00) (07) (0777) (-0777)

; floats, the lane a token takes when it is none of the three integer shapes
(0.0) (1.5) (-1.5) (3.141592653589793) (2.718281828459045) (100000.0)

; strings, and love's escapes -- n t r e 0 x and nothing else
("") ("a") ("a b c") ("(not a form)") (";not a comment")
("\n") ("\t") ("\r") ("\e") ("\0") ("\x41") ("\\") ("\"")

; a spaced operator is a plain symbol to both readers -- it is the glue that parts them
(= a b) (+ 1 2) (< a b) (- a b) (a = b)

; comments run to end of line, including after a form
(a) ; trailing
; a whole line
(b)
