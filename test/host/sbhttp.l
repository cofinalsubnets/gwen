; --- test/host/sbhttp.l: svalbard pulling a store over a real socket ---
; NOT in the test/*.l corpus: the socket nifs live in host/sock.c, which love0 does
; not link, and this loads two crew files and splices their modules.
;
; the whole thing runs in ONE process, test/host/kiosko.l's shape: kiosko twirled
; onto a listener over the origin nest's .sb/, svalbard.s client in the main task,
; the two interleaving through the cooperative scheduler. so this is a real HTTP
; pull off a real socket, and the two crew members meet the way they would in
; anger -- love serving love.
;
; what it proves is the claim the design rests on: an sb remote needs NO server
; logic. the store is content-addressed and immutable, there is no listing
; endpoint, and the walk discovers everything by reading what it already fetched
; -- /tips names the heads, each patch names its `deps` and its `hunks`. an
; EMPTY blob rides through too, which is the case that found kiosko's 404.
(use 'sb)                                 ; the loader: lib/sb.l assembles the whole stack
(use 'kiosko)

(: SD (from 'sb)
   record (SD 'sb-record) sync (SD 'sb-sync) bank (SD 'sb-bank)
   look (SD 'look) lay (SD 'lay) mk (SD 'mk) dirty (SD 'dirty) refof (SD 'refof)
   urlsplit (SD 'url-split) urlp (SD 'url?) digits (SD 'digits) readform (SD 'readform)
   root "out/host/.sbhttp"
   _ (hark (list "rm" "-rf" root))
   _ (mkdir "out" 493) _ (mkdir "out/host" 493) _ (mkdir root 493)
   _ (mkdir (root + "/origin") 493) _ (mkdir (root + "/clone") 493)
   home (cwd 0)
   ; the server task shares this process's cwd, and the client chdirs into the
   ; clone before pulling -- so the root it serves must be ABSOLUTE
   oroot (home + "/" + root + "/origin/.sb")
   ; --- the origin nest: a nested path and an EMPTY file, then a release ---
   _ (chdir (root + "/origin"))
   _ (mkdir "sub" 493)
   _ (lay "a.txt" "v1\n") _ (lay "sub/b.txt" "nested\n") _ (lay "empty.txt" "")
   _ (mk ()) _ (record "first")
   _ (lay "a.txt" "v2\n") _ (record "second")
   _ (bank '("v1"))
   nblob (tally (readdir ".sb/blob"))
   _ (chdir home)
   ; --- kiosko over the origin's .sb/, in a task ---
   port 7433
   l (listen port)
   srv (twirl (\ _ ((: (g k) (: _ (await l)
                                c (accept l)
                                _ (? c (: _ ((from 'kiosko 'serve1) oroot () c)
                                          (close c)) ())
                                (g k)))
                    0)) 0)
   ; --- the pull, from a nest that has never seen the origin ---
   _ (chdir (root + "/clone"))
   got (sync ("http://127.0.0.1:" + (show port) + "/"))
   a (look "a.txt") b (look "sub/b.txt") e (look "empty.txt")
   clean (dirty ())
   ref (refof "v1")
   npat (tally (readdir ".sb/patch"))
   _ (chdir home))

(assert
 (urlp "http://h/") (! (urlp "https://h/")) (! (urlp "../peer"))     ; the url gate
 (5432 = (digits "5432")) (! (digits "8x"))
 ((show '("h" 8080 "/a")) = (show (urlsplit "http://h:8080/a")))
 ((show '("h" 80 "/")) = (show (urlsplit "http://h")))
 ((show '(1 2)) = (show (readform "(1 2)")))                         ; a form off a string
 (0 < got)                                                           ; the pull went through
 (2 = npat) (4 = nblob)                                              ; every patch, every blob
 (<>a = "v2\n")                                                    ; the head content
 (<>b = "nested\n")                                                ; ...a NESTED path materialized
 (two? e) (0 = (tally <>e))                                        ; ...and the EMPTY file, 0 bytes
 (two? ref)                                                          ; the ref rode along
 (0 = clean))                                                        ; the tree matches the snap

(: _ (test_fin ()) _ (puts "sbhttp: ok") _ (putc 10) 0)
