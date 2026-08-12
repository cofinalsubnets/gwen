; bind.l -- rung 2 of the splice JIT automation: the relocation step lift.l refuses,
; done IN-PROCESS. a JIT binds against ITSELF: `bindcode` reads a composed .o and
; answers a cask of runnable bytes bound to THIS process's live symbols -- so the
; caller nifs it in the same process, the only place the addresses are valid (a
; separate process has a different ASLR base; that is why this is a library, not a
; tool that writes a .l for later). load it: love -l bench/vmsplice/bind.l ...
;
; the mechanism: mooncc emits an external ref as 7-byte `lea r,[rip+d32]`
; (R_X86_64_PC32). flip it to a same-length `mov r,[rip+d32]` aimed at an 8-byte cell
; APPENDED to the blob holding the symbol's live address -- every ref is then
; blob-internal, so nif's mmap can land the bytes anywhere. we control both sides:
; symbols come from our own binary's symtab (/proc/self/exe, holo linked it) + the
; load bias from /proc/self/maps. a reloc not wearing the lea shape refuses by name --
; a CALL (ai_please, once allocating ops are covered) is a later rung, same appendix.
(: (bind-die m) (: _ (say err ("bind: " m "\n")) (welp '(bind)))
   (bind-rd p) (: q (open p "r") _ (? q 0 (bind-die ("cannot open " p))) s (slurp q) _ (close q) s)
   (bind-le s i n) (: (go k a m) (? (k = n) a (go (k + 1) (a + ((peep s (i + k) 0) * m)) (m * 256))) (go 0 0 1))

   ; --- the live symbol table: /proc/self/exe sections + the load bias from maps ----
   (bind-live-tab _) (: exe (bind-rd "/proc/self/exe")
      shoff (bind-le exe 40 8) shentsz (bind-le exe 58 2) shnum (bind-le exe 60 2)
      (sh i f w) (bind-le exe (shoff + ((i * shentsz) + f)) w)
      symi (: (go i) (? (i = shnum) -1 (? ((sh i 4 4) = 2) i (go (i + 1)))) (go 0))
      _ (? (0 <= symi) 0 (bind-die "no symtab in /proc/self/exe"))
      stroff (sh (sh symi 40 4) 24 8)
      symoff (sh symi 24 8) syment (sh symi 56 8) nsyms ((sh symi 32 8) // syment)
      (cstr i) (: (go j) (? (= 0 (peep exe j 0)) (snip exe i j) (go (j + 1))) (go i))
      (symva nm) (: (go i) (? (i = nsyms) -1
                     (: o (symoff + (i * syment))
                        (? (= nm (cstr (stroff + (bind-le exe o 4)))) (bind-le exe (o + 8) 8) (go (i + 1)))))
                   (go 0))
      exepath (? (lit? readlink) (readlink "/proc/self/exe") (bind-die "no readlink nif"))
      base (: mp (bind-rd "/proc/self/maps") n (tally mp)
              (sp? j) (= 32 (peep mp j 0))
              (eol? j) (|| (= j n) (= 10 (peep mp j 0)))
              (nlend j) (: (go e) (? (eol? e) e (go (e + 1))) (go j))
              (skipsp j) (: (go e) (? (&& (< e n) (sp? e)) (go (e + 1)) e) (go j))
              (tokend j) (: (go e) (? (|| (eol? e) (sp? e)) e (go (e + 1))) (go j))
              (fld i f) (: (go j k) (: t (tokend j) (? (= k f) (j >< t) (go (skipsp t) (k + 1)))) (go i 0))
              (hex i e) (: (go j a) (: c (peep mp j 0)
                           (? (>= j e) a
                              (&& (47 < c) (c < 58)) (go (j + 1) ((a * 16) + (c - 48)))
                              (&& (96 < c) (c < 103)) (go (j + 1) ((a * 16) + (c - 87)))
                              a)) (go i 0))
              (lstart i) (: se (fld i 0) (hex (cap se) (tokend (cap se))))
              (loff i) (: se (fld i 2) (hex (cap se) (cup se)))
              (lpath i) (: se (fld i 5) (snip mp (cap se) (nlend (cap se))))
              (go i) (? (>= i n) (bind-die "exe not found in maps")
                        (&& (= 0 (loff i)) (= exepath (lpath i))) (lstart i)
                        (go (+ (nlend i) 1)))
              (go 0))
      (\ nm (: v (symva nm) (? (v < 0) (bind-die ("no live symbol " nm)) (base + v)))))

   ; --- bindcode: a composed .o -> a cask of process-bound bytes | () ----------------
   (bindcode obj) (: s (bind-rd obj)
      ob ((from 'holo 'ld-read) 'bind s 62)
      secs (peep ob 'secs 0) syms (peep ob 'syms 0)
      (nth l i) (? (= i 0) (cap l) (nth (cup l) (i - 1)))
      tx (: (go ks) (? (atom? ks) () (: v (peep secs (cap ks) 0)
                      (? (id? (nth v 0) 'text) (link (cap ks) v) (go (cup ks))))) (go (keys secs)))
      _ (? tx 0 (bind-die "no .text"))
      tix (cap tx) toff (nth (cup tx) 1) tsz (nth (cup tx) 2)
      (symnm i) (: v (peep syms i 0) (? (tally v) (nth v 0) (bind-die ("no symbol " (show i)))))
      rels (: (go rs acc) (? (atom? rs) acc (: r (cap rs)
                (go (cup rs) (? ((cap r) = tix) ((cup r) + acc) acc)))) (go (peep ob 'relas ()) ()))
      pad ((8 - (tsz % 8)) % 8)
      (celloff i) ((tsz + pad) + (8 * i))
      (asn nm cells) (: (go l i) (? (atom? l) (i >< ((nm >< i) >< cells))
                                    (id? nm (caap l)) ((cuap l) >< cells)
                                    (go (cup l) (i + 1))) (go cells 0))
      ov (tablet 0)
      fin (: (go rs cells) (? (atom? rs) (rev (map cap cells))
               (: r (cap rs) off (nth r 0) ty (nth r 1) si (nth r 2) ad (nth r 3)
                  _ (? (= ty 2) 0 (bind-die ("reloc type " (show ty) " not bound (want PC32)")))
                  op (peep s ((toff + off) - 2) 0)             ; d32<-ModRM(off-1)<-opcode(off-2)<-REX(off-3)
                  _ (? (= op 141) 0 (bind-die ("reloc at " (show off) " is not a lea (op " (show op) ")")))
                  ac (asn (name (symnm si)) cells) slot (cap ac) cells2 (cup ac)
                  d ((celloff slot) - (off + 4)) du (? (d < 0) (d + 4294967296) d)
                  _ (pin ov ((toff + off) - 2) 139)            ; lea -> mov (8d->8b); ModRM keeps the dest reg
                  _ (pin ov (toff + off) (du % 256))
                  _ (pin ov ((toff + off) + 1) ((du // 256) % 256))
                  _ (pin ov ((toff + off) + 2) ((du // 65536) % 256))
                  _ (pin ov ((toff + off) + 3) ((du // 16777216) % 256))
                  (go (cup rs) cells2))) (go rels ()))
      roster fin
      lv (bind-live-tab 0)
      ; assemble the byte list: text (overridden) + pad + one live-address cell per sym
      txt (: (go i acc) (? (i < 0) acc (go (i - 1) ((peep ov (toff + i) (peep s (toff + i) 0)) >< acc))) (go (tsz - 1) ()))
      pd (: (go i acc) (? (i < 0) acc (go (i - 1) (144 >< acc))) (go (pad - 1) ()))
      cells (catmap (\ nm (: v (lv (string nm))
                             (go k w acc) (? (k = 8) (rev acc) (go (k + 1) (w // 256) ((w % 256) >< acc)))
                             (go 0 v ()))) roster)
      _ (say out ("bind: " (show tsz) " text + " (show (tally roster)) " cell(s): " (show roster) "\n"))
      (cask (txt + (pd + cells)))))
