; crew/seed/up.l -- the up/down verbs + the verb table: the dist artifact's rail
; (doc/seed.md, the install answer: install = sync + cook build). this file rides
; the dist cat LAST (crew/build.mk distfiles), so it reads the crew bare -- seed's
; seed-sync/dirty, kore's dir?, cook/kiosko/moon mains -- and its tail pins
; `verbs`, the table love/cli.l's verb rail walks. `verbs` is unbound everywhere
; else, so the rail is a property of what is BAKED, not of the language: only an
; artifact whose bake evaluated this file answers `love up`.
;
; up [URL]: seed-sync ~/.love/src from URL (an http .seed/ tree or a directory),
; then `self cook install` INSIDE it -- cook parses the repo Makefile and lands
; the ~/.love nest (rung 1's cook-native install). re-running up IS the update:
; sync pulls only what is new, cook rebuilds only what is stale. the URL default
; is `origin`, pinned ahead of this file by the dist cat when the build set one
; (make dist DIST_ORIGIN=...); unbound it reads the zero point and up asks for
; an explicit URL.
;
; down [-f]: the uninstall twin -- the nest whole, plus only those ~/.local
; links that resolve into it. unrecorded work in src REFUSES without -f (down
; eats the tree; seed knows whether it holds anything unsaved). ~/.vim is the
; user's; down leaves it alone.
(: (oops m) (: _ (say err m) _ (put err 10) (quit 1))

   ; the nest paths, off HOME -- the seat walk's own convention, no env knobs
   (nestof _) (: h (getenv "HOME")
                 _ (? h 0 (oops "up: no HOME in the environment"))
                 (h + "/.love"))

   ; --- up ---------------------------------------------------------------
   (up-main as)
    (: url (? (two? as) <as origin)
       _ (? url 0 (: _ (say err "up: no origin baked in -- love up URL")
                     _ (put err 10) (quit 2)))
       nest (nestof ()) src (nest + "/src")
       _ (mkdir nest 493) _ (mkdir src 493)              ; EEXIST from a prior up is fine
       _ (? (charm? (chdir src)) (oops ("up: cannot enter " + src)) 0)
       _ (puts ("up: syncing " + src + " from " + url + "\n"))
       r (seed-sync url)
       _ (? r 0 (oops "up: the sync did not land"))
       exe (readlink "/proc/self/exe")
       _ (puts "up: cooking the nest (cook install)\n")
       s (runt (list exe "cook" "install"))
       st (? (two? s) <s s)
       _ (? (0 = st) 0 (oops ("up: cook install failed (exit " + (show st) + ")")))
       _ (puts ("up: the nest is " + nest + " -- love (and seed, cook, kore ..) on ~/.local/bin\n"))
       (quit 0))

   ; --- down -------------------------------------------------------------
   ; zap: rm -r that NEVER follows a link -- readlink answers the target string
   ; for a symlink and the REAL () otherwise, so the link test comes first and
   ; a linked directory loses its link, keeps its tree.
   (zap p)
    (? (! (id? (readlink p) ())) (! (unlink p))
       (dir? p) (: (go m ok) (? (! (two? m)) ok
                                (: k (zap (p + "/" + <m)) (go >m (? k ok 0))))
                   ok (go (readdir p) 1)
                   (&& ok (! (rmdir p))))
       (! (unlink p)))

   ; reap: unlink the links in d whose target leads with `nest` -- ours and only
   ; ours (a ~/.local/bin/cook pointing elsewhere is somebody else's cook)
   (reap d nest)
    (: (go m n) (? (! (two? m)) n
                   (: p (d + "/" + <m) t (readlink p)
                      hit (&& (! (id? t ()))
                              (&& (<= (tally nest) (tally t))
                                  ((snip t 0 (tally nest)) = nest)))
                      _ (? hit (unlink p) 0)
                      (go >m (? hit (n + 1) n))))
       (go (readdir d) 0))

   (down-main as)
    (: f? (&& (two? as) (<as = "-f"))
       nest (nestof ()) src (nest + "/src")
       here (cwd 0)
       d (? f? 0
            (? (two? (stat (src + "/.seed/snap")))     ; presence by stat, never by emptiness
               (: _ (chdir src) n (dirty ()) _ (chdir here) n)
               0))
       _ (? (0 < d)
            (oops ("down: " + (show d) + " unrecorded change(s) in " + src
                   + " -- seed record them, or love down -f"))
            0)
       _ (? (two? (stat nest)) 0 (: _ (puts ("down: no nest at " + nest + "\n")) (quit 0)))
       _ (chdir (getenv "HOME"))                       ; never rm the tree we stand in
       _ (? (zap nest) 0 (oops ("down: could not remove all of " + nest)))
       nb (reap ((getenv "HOME") + "/.local/bin") nest)
       nm (reap ((getenv "HOME") + "/.local/share/man/man1") nest)
       _ (puts ("down: removed " + nest
                + (? (0 < (nb + nm)) (" and " + (show (nb + nm)) + " ~/.local link(s)") "")
                + "\n"))
       (quit 0)))

; the table the rail walks: ((NAME FN) ..). FN takes the args AFTER the name and
; owns its exit; the rail quits 0 for any that return. kore keeps its own inner
; dispatch, so `love kore sed ..` nests -- the artifact is multi-call before any
; symlink is laid, and a symlink named for a verb (the argv[0] door) works too.
(: verbs (list
    (list "up" up-main)
    (list "down" down-main)
    (list "seed" seed-main)
    (list "cook" cook-main)
    (list "kore" (\ as (kore-main (link "kore" as))))
    (list "kiosko" kiosko-main)
    (list "mooncc" moon-main)))
