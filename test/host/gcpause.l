; --- test/host/gcpause.l: the pause gauge ---
; what one collection costs, and that the cost keeps its shape. the collector
; is a copier, so a collection's pause is its copy volume: a minor copies the
; nursery's survivors (the churn's live edge) and owes nothing to the tenured
; set -- grow the live set 16x and the worst minor stays flat; a major copies
; the live set itself, the honest linear cost. words are the deterministic
; measure and carry the asserts; wall ns ride along in the printed table only
; (machines flake, words don't). both shapes are theorems in the model first --
; test/proof/rocq/gc.v's minor_work_bounded (the minor's work bounded by the nursery
; alone, old absent from the statement) and minor_flat (the survivor set
; identical under tenure-blind growth) -- so this gate is the instance-check,
; not free-standing evidence.
;
;   gauge[7]  n_evac    Σ words copied over all collections -- a delta around
;                       one forced collection is that collection's exact work
;   gauge[8]  old       words live in the major pool (the tenured set)
;   gauge[14] minor-hi  peak words a single minor copied (whole run)
;   gauge[15] major-hi  peak words a single major copied (its live set)
;   (please ()) asks for a minor, (please 1) a major, (nclock t) monotonic ns
;   (core nifs, every frontend; the gate stays host-side for its pool appetite
;   -- megabytes of tenured casks -- not for its names)
;
;   cat test/00-init.l test/host/gcpause.l | out/host/love

(: G_LEN 0 G_GC 3 G_EVAC 7 G_OLD 8 G_MINOR 11 G_MAJCAP 12 G_MINHI 14 G_MAJHI 15
   (gget i) (peep (gauge 0) i 0)
   (mx a b) (? (a < b) b a)

   ; one churn step: raise a fresh 4k list, force a minor under it (the list
   ; is the survivor set), answer `(minors words ns) for that one collection --
   ; counting minors specifically, so the asserts prove nothing escalated.
   (step _) (: e0 (gget G_EVAC) g0 (gget G_MINOR)
               keep (map (+ 1) (jot 4000))
               t0 (nclock 0)
               _  (please ())
               ns (nclock t0)
               dg ((gget G_MINOR) - g0) de ((gget G_EVAC) - e0)
               _  (tally keep)                       ; keep lives across the minor
             [dg de ns])

   ; a forced major fires on a near-empty nursery, so gen_major sizes its
   ; to-space just 25% over the live set -- less headroom than one full
   ; nursery, and gen_please's worst-case-promotion guard then (rightly)
   ; escalates every next collection to a major. production never sees this
   ; (a natural major fires with the nursery FULL, so the resized pool
   ; inherits nursery-sized headroom); restore it the same way -- churn dead
   ; casks so the next collections fire at a full nursery -- and hold on
   ; until the headroom clears the nursery by a whole window's promotions
   ; (the step-snap can land the first recovery tight; stage 2 proved it).
   (recover _) (: (room _) ((// (gget G_MAJCAP) 2) - gget G_OLD)
                  (go i) (? ((room ()) < (gget G_LEN) + 262144)
                          (go (i + tally (cask 262144))) i)
                (go 0))

   ; k steps -> `(worst-words worst-ns collections) over the window
   (churn k) (: (go i ww wn cs)
                (? (i < k) (: r (step ())
                          (go (i + 1) (mx ww <>r) (mx wn <>>r) (cs + <r)))
                         [ww wn cs])
              (go 0 0 0 0))

   ; tenure n 64KB casks: build them young, then the timed forced major
   ; promotes them -> `(major-words major-ns casks); the casks ride in the
   ; answer so a later read holds every level live to the end.
   (tenure n) (: l (map (\ _ (cask 65536)) (jot n))
                 e0 (gget G_EVAC)
                 t0 (nclock 0)
                 _  (please 1)
                 ns (nclock t0)
                 de ((gget G_EVAC) - e0)
               [de ns l])

   (row maj r) (: _ (puts "live-w ") _ (putx (gget G_OLD))
                  _ (puts "  major-w ") _ (putx <maj) _ (puts "  major-ns ") _ (putx <>maj)
                  _ (puts "  minor-worst-w ") _ (putx <r) _ (puts "  minor-worst-ns ") _ (putx <>r)
                  (putc 10))

   ; three levels, 12x tenured growth end to end, every level held live
   t1 (tenure 16)  _ (recover ()) r1 (churn 8) _ (row t1 r1)
   t2 (tenure 48)  _ (recover ()) r2 (churn 8) _ (row t2 r2)
   t3 (tenure 192) _ (recover ()) r3 (churn 8) _ (row t3 r3)
   _ (tally <>>t1 + tally <>>t2 + tally <>>t3)       ; every cask list live past the last minor

   _ (assert
      (8 = <>>r1) (8 = <>>r2) (8 = <>>r3)     ; every churn collection was a minor, one each
      (0 < <r1)                               ; the minors did real work (the claim isn't vacuous)
      (<r2 < <r1 + 1024)                      ; the flat line: 12x the tenured set moves the
      (<r3 < <r1 + 1024)                      ;   worst minor by less than 8KB of copy volume
      (<t1 < <t2) (<t2 < <t3)                 ; the major's work grows with the live set
      (<r1 <= gget G_MINHI)                   ; the whole-run peaks cover what we watched
      (<t3 <= gget G_MAJHI))

   _ (test_fin ())
   _ (puts "test/host/gcpause: ok")
 (putc 10))
