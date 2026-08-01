; patch.l -- the model-driven proposer: the WRITE half's brain. drive.l has the
; loop (propose -> grade -> keep/revert); this makes the proposal a real MODEL
; PATCH -- ask claude for the new contents of a file, hand it to drive as
; (path body). load after judge/serve/drive:
;   out/host/dock -l port/inle/judge.l -l port/inle/serve.l -l port/inle/drive.l -l port/inle/patch.l -e "(dock 7620)"
;   > (improve "add a `double` helper to the prelude" "love/prel.l")   ; one guarded iteration
;
; SAFE BY CONSTRUCTION: the model only ever proposes a file body; drive.l applies
; it with `lay`, the gate (rebuild + make test) judges it, and a RED gate reverts
; with git -- so a bad patch (a model's included) can never leave the tree broken.
; `adopt` (serve.l) is a SEPARATE, explicit step: the loop lands the change in the
; tree; you re-exec onto it when you choose. nothing here re-execs on its own.
;
; the curl/json machinery (find/unesc/xtext) is self-contained. no key -> the model
; call yields "" and the proposer goes idle; a MOCK
; model can be installed with `set-model` to exercise the whole loop offline.

(: ;; --- json codec ---------------------------------------------------------
   (find hay nee)
     (: n (tally nee)
        (hit i) (: (m j) (? (= j n) 1 (? (= (hay (+ i j)) (nee j)) (m (+ 1 j)) 0)) (m 0))
        (go i)  (? (> (+ i n) (tally hay)) -1 (? (hit i) i (go (+ 1 i))))
        (go 0))
   (esc1 c) (? (= c 34) "\\\"" (= c 92) "\\\\" (= c 10) "\\n" (= c 13) "\\r" (= c 9) "\\t" (+ "" c))
   (jesc s) (: (go i acc) (? (< i (tally s)) (go (+ 1 i) (+ acc (esc1 (s i)))) acc) (go 0 ""))
   (dec1 d) (? (= d 110) (+ "" 10) (= d 116) (+ "" 9) (= d 114) (+ "" 13) (+ "" d))  ; \n\t\r; else raw (so \" \\ pass through)
   (unesc hay start)
     (: (go i acc)
          (? (< i (tally hay))
             (: c (hay i)
                (? (= c 34) acc                                        ; closing quote
                   (= c 92) (go (+ 2 i) (+ acc (dec1 (hay (+ 1 i)))))   ; \x -> decode
                   (go (+ 1 i) (+ acc (+ "" c)))))
             acc)
        (go start ""))
   (xtext resp) (: nee "\"text\":\"" i (find resp nee)
                   (? (< i 0) "" (unesc resp (+ i (tally nee)))))       ; miss -> "" (proposer idles)

   ;; --- strip one leading+trailing ``` fence, if the model wrapped its answer.
   ;; only when the first non-space run is ``` AND a closing ``` exists; else the
   ;; body passes through untouched (never mangle real code). --
   (nlpast s i) (: (go j) (? (< j (tally s)) (? (= (s j) 10) (+ 1 j) (go (+ 1 j))) (tally s)) (go i))
   (strip-fence s)
     (: (? (&& (< 2 (tally s)) (&& (= (s 0) 96) (&& (= (s 1) 96) (= (s 2) 96))))  ; starts ```
           (: open (nlpast s 0)                              ; past the ```lang line
              close (find s "\n```")
              (? (< close 0) s (snip s open close)))         ; body between the fences
           s))

   ;; --- the model call. a MOCK (set-model) short-circuits curl for offline
   ;; testing; else real claude over curl (no key -> ""), one big completion. --
   patch-max 8192
   model-slot (tablet 0)
   (set-model f) (pin model-slot 0 f)                        ; f : (sys q) -> reply text
   sys-code (+ "You are editing the love programming language: a fully-curried "
            (+ "language with an infix, low-paren surface that factors to a lisp core over a tiny C core (see CLAUDE.md). I give you a FILE and a TASK. "
               "Return ONLY the COMPLETE new contents of that file -- no markdown fence, no commentary. The build + test gate must stay green."))
   (mkbody q)
     (+ "{\"model\":\"claude-opus-4-8\",\"max_tokens\":" (+ (show patch-max)
        (+ ",\"system\":\"" (+ (jesc sys-code)
           (+ "\",\"messages\":[{\"role\":\"user\",\"content\":\"" (+ (jesc q) "\"}]}"))))))
   (ask-code q)
     (: m (peep model-slot 0 0)
        (? m (m sys-code q)                                  ; a mock model is installed
           (: k (getenv "ANTHROPIC_API_KEY")
              (? (! k) ""
                 (: r (hark (L "curl" "-s" "https://api.anthropic.com/v1/messages"
                              "-H" (+ "x-api-key: " k)
                              "-H" "anthropic-version: 2023-06-01"
                              "-H" "content-type: application/json"
                              "-d" (mkbody q)))
                    (xtext (cup r)))))))

   ;; --- the proposer: read the file, ask the model, strip a stray fence, hand
   ;; drive.l a (path body). an empty reply (no key, a miss) -> () = idle/safe. --
   (mkprompt task path body)
     (+ "TASK: " (+ task (+ "\n\nHere is the current " (+ path (+ " -- return its COMPLETE new contents:\n\n" body)))))
   (propose-patch task path)
     (: body (: b (look path) (? (string? b) b ""))
        code (strip-fence (ask-code (mkprompt task path body)))
        (? (< (tally code) 1) () (L path code)))

   ;; --- wire it into drive.l: set the proposer, run ONE guarded iteration. --
   (improve task path)
     (: _ (set-proposer (\ _ (propose-patch task path)))
        (drive 1)))
