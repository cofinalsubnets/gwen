; string-literal escape sequences. the C reader (g.c:g_read1) parses this
; source file at load time; the second block exercises the same reader via
; the charlist port (g.c:ci_getc, used by the REPL submit path) so the
; in-memory edit buffer agrees with file input.

(assert
  ; --- C reader path: this file is parsed by it ---
  ; \n / \t / \r / \0 become 1-byte strings with the corresponding byte.
  (= 1 (tally "\n"))           (= "\n" (string (link 10 0)))
  (= 1 (tally "\t"))           (= "\t" (string (link 9 0)))
  (= 1 (tally "\r"))           (= "\r" (string (link 13 0)))
  (= 1 (tally "\0"))           (= "\0" (string (link 0 0)))
  ; \xHH: two hex digits.
  (= 1 (tally "\x00"))         (= "\x00" (string (link 0 0)))
  (= 1 (tally "\x41"))         (= "\x41" (string (link 65 0)))
  (= 1 (tally "\xff"))         (= "\xff" (string (link 255 0)))
  (= "\x41" "A")             (= "\x0a" "\n")
  (= "\xAB" "\xab")          ; mixed case
  ; \\ and \" still pass through via the take-next-char-as-is fallback.
  (= 1 (tally "\\"))           (= "\\" (string (link 92 0)))
  (= 1 (tally "\""))           (= "\"" (string (link 34 0)))
  ; embedded mid-string
  (= 3 (tally "a\nb"))         (= "a\nb" (string (link 97 (link 10 (link 98 0)))))
  (= 3 (tally "a\tb"))         (= "a\tb" (string (link 97 (link 9  (link 98 0)))))
  (= 3 (tally "a\rb"))         (= "a\rb" (string (link 97 (link 13 (link 98 0)))))
  (= 3 (tally "a\0b"))         (= "a\0b" (string (link 97 (link 0  (link 98 0)))))
  (= 3 (tally "a\x42b"))       (= "a\x42b" (string (link 97 (link 66 (link 98 0))))))

; --- the reader's own path: feed it `"..."` SOURCE and read the datum back.
; the cases are written as byte codes so the escapes under test are not themselves
; escaped in this file; `string` makes the text the reader takes.
(:
 (parse cl) (cap (sound (string cl)))
 (assert
   (= (string (link 10 0))                   (parse '(34 92 110 34)))   ; "\n"
   (= (string (link 9 0))                    (parse '(34 92 116 34)))   ; "\t"
   (= (string (link 13 0))                   (parse '(34 92 114 34)))   ; "\r"
   (= (string (link 0 0))                    (parse '(34 92 48 34)))    ; "\0"
   (= (string (link 92 0))                   (parse '(34 92 92 34)))    ; "\\"
   (= (string (link 34 0))                   (parse '(34 92 34 34)))    ; "\""
   ; \xHH: 120='x', then 2 hex digits
   (= (string (link 0 0))                    (parse '(34 92 120 48 48 34)))    ; "\x00"
   (= (string (link 65 0))                   (parse '(34 92 120 52 49 34)))    ; "\x41" = 'A'
   (= (string (link 255 0))                  (parse '(34 92 120 102 102 34)))  ; "\xff"
   (= (string (link 171 0))                  (parse '(34 92 120 65 66 34)))    ; "\xAB"
   (= (string (link 97 (link 10 (link 98 0))))     (parse '(34 97 92 110 98 34)))   ; "a\nb"
   (= (string (link 97 (link  9 (link 98 0))))     (parse '(34 97 92 116 98 34)))   ; "a\tb"
   (= (string (link 97 (link 13 (link 98 0))))     (parse '(34 97 92 114 98 34)))   ; "a\rb"
   (= (string (link 97 (link  0 (link 98 0))))     (parse '(34 97 92 48 98 34)))    ; "a\0b"
   (= (string (link 97 (link 66 (link 98 0))))     (parse '(34 97 92 120 52 50 98 34))))) ; "a\x42b"
