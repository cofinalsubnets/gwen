; tools/wm2uu.l -- stage 3 of the verified-lux ladder: the .l -> uu translator.
; Reads crew/lux/core.l's zipper ops as data and emits uu terms in the model's
; vocabulary, kind-directed by crew/lux/sigs.l (the overlay's boundary table is
; the type oracle): a cap on a zipper is zfoc, on a row hd0; cap-of-cup is zup,
; cap-of-cup-of-cup is zdn (the glued <>z / <>>z factor to those chains);
; ></link and +/cat/rev are ncons/lapp/lrev; a two? test cases on a
; row's length (nat_rect) or witnesses a zipper nonempty (the model's stk IS
; the nonempty zipper, so core.l's ()-lane erases -- zins's make-a-singleton
; else included); =/!= case through bool_rect on nateqb; local (: ..) chains
; become definitional lets; znub's filter shape is nfilt; zdel answers the
; model's Maybe (a zipper result wraps ii2, the bare () becomes ii1 tt).
; The output defn-certifies in the kernel (boot/overlay.l leg 6), answers the
; seeded fuzz exactly as core.l does, and lands convertible with the hand
; model -- stage 4's idpath bridges ride that. (wm2uu forms sigs) answers
; ((gen-name uu-type uu-term) ..) for the twelve ops in w2-want.
;
; a body-less `:` -- the w2-* vocabulary leaks; none shadows a nif.
(:
   (w2-nthl l n) (? (= n 0) (cap l) (w2-nthl (cup l) (- n 1)))
   (w2-sigof sigs nm) (? (! (two? sigs)) ()
                         (id? nm (cap (cap sigs))) (cap sigs)
                         (w2-sigof (cup sigs) nm))
   (w2-zip ps ks) (? (two? ps) (link [(cap ps) (cap ks)] (w2-zip (cup ps) (cup ks))) ())
   ; the list ctor: `list` is what [..] and the ` sigil read to.
   (w2-list? h) (id? h 'list)
   ; and so does negation: `!` is the sigil, `nil?` what opfix factors it to.
   ; the forms arrive factored, so match the core word and the surface one both.
   (w2-not? h) (|| (id? h '!) (id? h 'nil?))
   ; ..and the pair ops: >< is link, + is cat on a row. both spellings match.
   (w2-link? h) (|| (id? h '><) (|| (id? h 'link) (id? h '.)))
   (w2-cat? h) (|| (id? h '+) (id? h 'cat))

   ; --- kinds -> the model's types; zdel is the one Maybe op ---
   (w2-ty k) (? (id? k 'window) 'nat (id? k 'zipper) 'stk (id? k 'wrow) 'nlist 'nat)
   (w2-mty rk) (? (id? rk 'mstk) 'mstk (id? rk 'zipper) 'stk (id? rk 'wrow) 'nlist
                  (id? rk 'window) 'nat rk)
   (w2-pi ps ks res) (? (two? ps) ['pi (cap ps) (w2-ty (cap ks)) (w2-pi (cup ps) (cup ks) res)] res)
   (w2-lams ps b) (? (two? ps) ['lam (cap ps) (w2-lams (cup ps) b)] b)

   ; --- the wanted ops and their emitted names. zmk/zfoc are not wanted:
   ;     they are the representation vocabulary the rules target (a generated
   ;     zfoc would be (lam z (zfoc z)), a self-reference) -- they live in
   ;     test/uu.l's substrate. the name map is a slot so a driver can emit
   ;     under gen-* names (the gate's bridges) or the model's own names
   ;     (stage 5's test/uuwm.l) ---
   w2-want '(zrow zrev zfocup zfocdn zswapup zswapdn zmaster zins znub zdel)
   w2-gmap '((zrow gen-zrow) (zrev gen-zrev)
             (zfocup gen-zfocup) (zfocdn gen-zfocdn) (zswapup gen-zswapup)
             (zswapdn gen-zswapdn) (zmaster gen-zmaster) (zins gen-zins)
             (znub gen-znub) (zdel gen-zdel))
   w2-nm (: t (tablet 1) _ (pin t 0 w2-gmap) t)
   (w2-gen nm) (: p (assoc nm (peep w2-nm 0 ())) (? (two? p) (caup p) nm))
   (wm2uu-as forms sigs gmap) (: _ (pin w2-nm 0 gmap)
                                  r (wm2uu forms sigs)
                                  _ (pin w2-nm 0 w2-gmap)
                                  r)
   (w2-rk nm k) (? (id? nm 'zdel) 'mstk k)

   ; --- the kind of a source expression (enough to steer the rules) ---
   (w2-kof e env sigs)
    (? (nom? e) (: p (assoc e env) (? (two? p) (caup p) 'top))
       (charm? e) 'window
       (! (two? e)) 'top
       (: h (cap e)
          (? (id? h 'cap)
               (: a (w2-nthl e 1)
                  (? (&& (two? a) (id? (cap a) 'cup)) 'wrow          ; a cap-of-cup chain: the up/down row
                     (: k (w2-kof a env sigs)
                        (? (id? k 'zipper) 'window (id? k 'wrow) 'window 'top))))
             (id? h 'cup)  'wrow
             (id? h 'rev) 'wrow (w2-cat? h) 'wrow (w2-link? h) 'wrow
             (id? h 'filter) 'wrow (w2-list? h) 'zipper
             (: sg (w2-sigof sigs h) (? (two? sg) (w2-nthl sg 2) 'top)))))

   ; --- the translation. rk = the result kind at this position ---
   (w2-nil rk) (? (id? rk 'mstk) '(ii1 tt) 'nnil)
   (w2-tx e env sigs rk)
    (? (nom? e) e
       (charm? e) e
       (! (two? e)) (w2-nil rk)
       (: h (cap e)
          (? (id? h '?) (w2-cond (cup e) env sigs rk)
             (id? h ':) (w2-lets (cup e) env sigs rk)
             (id? rk 'mstk) ['ii2 (w2-app e env sigs 'zipper)]
             (w2-app e env sigs rk))))
   (w2-app e env sigs rk)
    (: h (cap e)
       (? (&& (id? h 'cap) (two? (w2-nthl e 1)) (id? (cap (w2-nthl e 1)) 'cup))
            ; the glued chains: <>z is (cap (cup z)) -> zup, <>>z one cup deeper -> zdn
            (: in (w2-nthl (w2-nthl e 1) 1)
               (? (&& (two? in) (id? (cap in) 'cup))
                  ['zdn (w2-tx (w2-nthl in 1) env sigs 'zipper)]
                  ['zup (w2-tx in env sigs 'zipper)]))
          (id? h 'cap)
            (: k (w2-kof (w2-nthl e 1) env sigs)
               [(? (id? k 'zipper) 'zfoc 'hd0) (w2-tx (w2-nthl e 1) env sigs k)])
          (id? h 'cup) ['tl0 (w2-tx (w2-nthl e 1) env sigs 'wrow)]
          (w2-link? h) ['ncons (w2-tx (w2-nthl e 1) env sigs 'window)
                               (w2-tx (w2-nthl e 2) env sigs 'wrow)]
          (w2-cat? h) ['lapp (w2-tx (w2-nthl e 1) env sigs 'wrow)
                             (w2-tx (w2-nthl e 2) env sigs 'wrow)]
          (id? h 'rev) ['lrev (w2-tx (w2-nthl e 1) env sigs 'wrow)]
          (w2-list? h) ['zmk (w2-tx (w2-nthl e 1) env sigs 'window)
                             (w2-tx (w2-nthl e 2) env sigs 'wrow)
                             (w2-tx (w2-nthl e 3) env sigs 'wrow)]
          (id? h 'filter)                              ; (filter (\ x (x != w)) l) -> nfilt
            (: fn (w2-nthl e 1) bd (w2-nthl fn 2)     ; the body wears != or (nil? (= ..))
               wex (? (id? (cap bd) '!=) (w2-nthl bd 2) (w2-nthl (w2-nthl bd 1) 2))
               ['nfilt (w2-tx wex env sigs 'window)
                       (w2-tx (w2-nthl e 2) env sigs 'wrow)])
          (: sg (w2-sigof sigs h)
             (? (two? sg) (link (w2-gen h) (w2-args (cup e) (cap (cup sg)) env sigs))
                (link h (map (\ a (w2-tx a env sigs 'top)) (cup e)))))))
   (w2-args as ks env sigs)
    (? (two? as) (link (w2-tx (cap as) env sigs (cap ks)) (w2-args (cup as) (cup ks) env sigs)) ())
   (w2-cond cs env sigs rk)
    (? (! (two? (cup cs))) (w2-tx (cap cs) env sigs rk)
       (: t (cap cs) r (cap (cup cs)) rest (cup (cup cs))
          els (? (two? (cup rest)) (link '? rest) (cap rest))
          (w2-if t r els env sigs rk)))
   (w2-if t then-x else-x env sigs rk)
    (? (&& (two? t) (w2-not? (cap t))) (w2-if (w2-nthl t 1) else-x then-x env sigs rk)
       (&& (two? t) (id? (cap t) 'two?))
         (: x (w2-nthl t 1)
            (? (id? (w2-kof x env sigs) 'zipper)
               (w2-tx then-x env sigs rk)                ; nonempty by kind: the ()-lane erases
               ['nat_rect ['lam 'gq (w2-mty rk)]
                  (w2-tx else-x env sigs rk)
                  ['lam 'gk ['lam 'gr (w2-tx then-x env sigs rk)]]
                  ['pr1 (w2-tx x env sigs 'wrow)]]))
       (&& (two? t) (id? (cap t) '=))
         ['bool_rect ['lam 'gx (w2-mty rk)]
            (w2-tx then-x env sigs rk)
            (w2-tx else-x env sigs rk)
            ['nateqb (w2-tx (w2-nthl t 1) env sigs 'window)
                     (w2-tx (w2-nthl t 2) env sigs 'window)]]
       (&& (two? t) (id? (cap t) '!=))                 ; != is = with the arms traded
         ['bool_rect ['lam 'gx (w2-mty rk)]
            (w2-tx else-x env sigs rk)
            (w2-tx then-x env sigs rk)
            ['nateqb (w2-tx (w2-nthl t 1) env sigs 'window)
                     (w2-tx (w2-nthl t 2) env sigs 'window)]]
       'w2-untranslatable)
   (w2-lets bs env sigs rk)
    (? (! (two? (cup bs))) (w2-tx (cap bs) env sigs rk)
       (: x (cap bs) v (cap (cup bs)) k (w2-kof v env sigs)
          ['let x (w2-tx v env sigs k)
             (w2-lets (cup (cup bs)) (link [x k] env) sigs rk)]))

   ; --- the driver: fish the wanted pairs out of core.l's forms ---
   (w2-pairs bs) (? (! (two? bs)) () (! (two? (cup bs))) ()
                    (link [(cap bs) (cap (cup bs))] (w2-pairs (cup (cup bs)))))
   (w2-find nm forms)
    (: prs (foldl (\ a f (? (&& (two? f) (id? (cap f) ':)) (cat a (w2-pairs (cup f))) a)) () forms)
       hit (filter (\ p (&& (two? (cap p)) (id? (cap (cap p)) nm))) prs)
       (? (two? hit) (cap hit) ()))
   (wm2uu forms sigs)
    (map (\ nm (: pr (w2-find nm forms)
                  sg (w2-sigof sigs nm)
                  ps (cup (cap pr))
                  ks (cap (cup sg))
                  [(w2-gen nm)
                   (w2-pi ps ks (w2-mty (w2-rk nm (w2-nthl sg 2))))
                   (w2-lams ps (w2-tx (caup pr) (w2-zip ps ks) sigs (w2-rk nm (w2-nthl sg 2))))]))
         w2-want))
