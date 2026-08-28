; --- test/host/json.l: the json module (lib/json.l) -- the wire mapping, exact both ways ---
; `[]` is plain () and null is the named coin: the empty array is the common value (a
; mapped or filtered list writes `[]` with no renaming step), null the weird one. run
; standalone from the repo root:
;   cat test/00-init.l test/host/json.l | out/host/love

(use "lib/json.l")

(: (json-rt s) (: r (json-read s) (? (two? r) (json-show <>r) 'no-parse)))

; presence rides out of band: (1 value) or () -- null, 0, "", [] and {} are all
; legitimate documents and every one of them is nothing
(assert
 (two? (json-read "null")) (two? (json-read "0")) (two? (json-read "\"\""))
 (two? (json-read "[]")) (two? (json-read "{}"))
 !(json-read "") !(json-read "nul") !(json-read "[1,]") !(json-read "{\"a\":}")
 !(json-read "1 2"))                              ; trailing junk is a refusal

; null is the named coin, [] is plain (), and the two never collide
(assert
 (: r (json-read "null") (json-null? <>r))
 (: r (json-read "[]") (() = <>r))
 ("null" = (json-show json-null)) ("[]" = (json-show ()))
 !json-null !json-false (? json-true 1 0)         ; blue where json is false-ish
 !(json-null = json-false) !(json-null = ()) !(() = json-false)
 ("true" = (json-show (json-bool 1))) ("false" = (json-show (json-bool 0))))

; round trips, textual where key order cannot betray them
(assert
 ("[5]" = (json-rt "[5]"))                        ; the one-element array survives
 ("[[],{}]" = (json-rt "[[],{}]"))
 ("[1,\"x\",true,false,null,[]]" = (json-rt "[1,\"x\",true,false,null,[]]"))
 ("{}" = (json-show (tablet 0)))
 (: t (tablet 0) _ (pin t "diagnostics" ()) ("{\"diagnostics\":[]}" = (json-show t))))

; the celestial ladder stays exact; what json cannot say becomes null
(assert
 ("1.5" = (json-rt "1.5")) ("-3" = (json-rt "-3"))
 ("123456789012345678901234567890" = (json-rt "123456789012345678901234567890"))
 ("null" = (json-show (1.0e308 * 10.0))))         ; ieee-inf has no json spelling

; strings: the named escapes both ways, \uXXXX surrogates rejoined to utf-8
(assert
 ("\"a\\nb\"" = (json-show "a\nb"))
 (: r (json-read "\"a\\nb\"") ("a\nb" = <>r))
 ("\"𝄞\"" = (json-rt "\"\\uD834\\uDD1E\""))
 ("\"𝄞\"" = (json-rt "\"𝄞\"")))                    ; already-utf-8 rides untouched

; a real lsp message: publishDiagnostics with an EMPTY diagnostics array, the shape
; that forced the design -- [] must come back [], never null
(assert
 (: r (json-read (+ "{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/publishDiagnostics\","
                    "\"params\":{\"uri\":\"file:///x.l\",\"diagnostics\":[]}}"))
    v <>r
    p (v "params")
    (&& (two? r) (() = (p "diagnostics")) ("file:///x.l" = (p "uri"))
        (: r2 (json-read (json-show v)) (&& (two? r2) (() = ((<>r2 "params") "diagnostics")))))))

(test_fin ())
(puts "json: ok")
(putc 10)
