; crew/reef/merge.l -- diff3: the three-way line merge reef falls to when two
; patches write ONE path from a shared base. the base is free: a hunk records the
; content hash it expected (its `old`), so the common ancestor is already in the
; store -- no ancestry search, just a blob read.
;
; the shape: myers each side against the base, then read each script as one PART
; PER SLOT, the slots ALTERNATING gap/line/gap/line../gap -- a line slot is what
; that side puts where base line i stood, a gap slot what it inserts between two
; base lines. the alternation is what keeps edits apart: a substitution moves its
; LINE slot only (the replacement rides with the del it answers), an insertion
; moves a GAP slot only, so two disjoint edits never share a slot and never meet.
; a slot where both sides produce the base's own content is STABLE; a maximal run
; where either side differs is UNSTABLE, and the run resolves by comparison alone:
;   ours = theirs   -> both made the same edit, take it (convergent)
;   ours = base     -> only theirs moved, take theirs
;   theirs = base   -> only ours moved, take ours
;   otherwise       -> a true overlap: markers, and the caller counts it
; so edits to different regions never meet, and only a real overlap conflicts.
; adjacent edits with no stable line between them share a run and conflict --
; textbook diff3, the same call git makes.
;
; reef is a REGISTERED MODULE and this file OPENS the layer -- (enter 'reef) here,
; (leave ()) at the foot of reef.l, holo's split-across-files shape. everything in
; both files binds in the `reef` layer, so the cat adds nothing to the book; a
; consumer reaches in with (from 'reef ..). loads after crew/kore/{text,diff}.l,
; which stay global (they are kore's, shared). it captures lines/unlines and myers
; at its define -- the capture law. the round trip through lines is
; exact on newline-terminated text; a final unterminated line gains its newline,
; text.l's one normalization, and it only ever touches a path already diverged.
(enter 'reef)

(:
   ; the 'adds sitting at the front of a script (a del's replacement)
   (m3grab m a) (? (&& (two? m) ((<<m) = 'add)) (m3grab >m (link <><m a)) `(m (rev a)))

   ; a myers script -> the 2n+1 slots, gap line gap line .. gap. an 'add after a
   ; KEEP is an insertion and lands in the pending gap; the 'adds after a DEL are
   ; that line's replacement and land in its line slot.
   (m3parts sc)
    (: (go m gp acc)
        (? (! (two? m)) (rev (link (rev gp) acc))
           (: o <m op <o x <>o acc2 (link (rev gp) acc)               ; the gap closes on a line op
              (? (op = 'keep) (go >m () (link `(x) acc2))
                 (op = 'del) (: r (m3grab >m ()) (go <r () (link <>r acc2)))
                 (go >m (link x gp) acc))))                           ; 'add: into the open gap
       (go sc () ()))

   ; the base's own slots: every gap empty, every line its one line
   (m3base b) (: (go m a) (? (two? m) (go >m (link `(<m) (link () a)))
                              (rev (link () a)))
                 (go b ()))

   ; the chunks (a list of line-lists) -> one flat line list
   (m3cat cs) (: (go m a) (? (two? m) (go >m (a + <m)) a) (go cs ()))

   ; the marker block for a true overlap
   (m3marks ob tb ol tl)
    ((link ("<<<<<<< " + ol) ob) + (link "=======" (tb + `((">>>>>>> " + tl)))))

   ; walk the three part-lists in step, chunking stable runs and resolving
   ; unstable ones. answers (chunks nconflicts), chunks newest-first.
   (m3go bs os ts ol tl acc nc)
    (? (! (two? bs)) `(acc nc)
       (&& ((<os) = <bs) ((<ts) = <bs)) (m3go >bs >os >ts ol tl (link <bs acc) nc)
       (: (run b o t bb ob tb)                                        ; extend while either side moved
           (? (&& (two? b) (|| (! ((<o) = <b)) (! ((<t) = <b))))
              (run >b >o >t (link <b bb) (link <o ob) (link <t tb))
              `(b o t (m3cat (rev bb)) (m3cat (rev ob)) (m3cat (rev tb))))
          r (run bs os ts () () ())
          bb <>>>r ob <>>>>r tb <>>>>>r
          (? (ob = tb) (m3go <r <>r <>>r ol tl (link ob acc) nc)      ; the same edit both sides
             (ob = bb) (m3go <r <>r <>>r ol tl (link tb acc) nc)      ; only theirs moved
             (tb = bb) (m3go <r <>r <>>r ol tl (link ob acc) nc)      ; only ours moved
             (m3go <r <>r <>>r ol tl (link (m3marks ob tb ol tl) acc) (nc + 1)))))

   ; THE ENTRY: three texts + the two side labels -> (mergedtext nconflicts).
   ; nconflicts is 0 when the merge came out clean.
   (diff3 base ours theirs ol tl)
    (: b (lines base)
       bp (m3base b)
       op (m3parts (myers b (lines ours)))
       tp (m3parts (myers b (lines theirs)))
       r (m3go bp op tp ol tl () 0)
       `((unlines (m3cat (rev <r))) (<>r))))
