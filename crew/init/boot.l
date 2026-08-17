#!/bin/love
; --- boot.l: the initramfs /init -- love as pid 1 on the Linux kernel ---
; the REAL bringup companion to init.l's (pid1 0) tour. where (pid1 0) mounts into
; /tmp/love-* demo paths and reaps a planted orphan to PROVE the shape, this is what the
; kernel actually execs as process 1 out of the initramfs: mount the FHS filesystems,
; then hand the console to lush (crew/lush, catted to /lib/sh.l) and never exit (pid 1 leaving panics the kernel).
;
; packaged by mk/distro.mk as /init (this file, shebang -> /bin/love), beside /bin/love,
; /bin/kore (+ applet symlinks) and /lib/sh.l. boot line:
;   qemu ... -kernel <bzImage> -initrd initramfs.cpio.gz -append "console=ttyS0 rdinit=/init"
;
; SAFE TO DRY-RUN ON THE HOST: `out/host/love init/boot.l` sees getpid != 1, prints the
; banner and returns -- no mounts, no shell handoff (so it never blocks a dev machine).
(: (say1 s) (: _ (puts s) (puts "\n"))              ; a console line

   ; mount one early filesystem at its FHS path (mkdir first; mount is () success /
   ; +errno failure -- e.g. EBUSY when the kernel already auto-mounted devtmpfs).
   (mnt src tgt typ)
     (: _ (mkdir tgt 493)
        r (mount src tgt typ)
        (say1 (+ "  mount " (+ typ (+ " on " (+ tgt
                 (? r (+ " -> errno " (show r)) " -> ok")))))))

   ; the console job: a fresh `love` running the shell over the inherited console fds.
   ; the -l registers module 'lush (the parts' own wraps); the -e reads it through
   ; one use -- compile is per-binding, so (sh 0) compiles after the splice.
   (console _) (L "/bin/love" "-l" "/lib/sh.l" "-e" "(: _ (use 'lush) (sh 0))")

   ; a boot self-check: run a real userland command (kore, forked + captured) so the
   ; console SHOWS the base system works end to end before we hand off the tty. `hark`
   ; returns (exit . stdout); the shebang on /bin/kore points at /bin/love.
   (selfcheck _)
     (: r (hark (L "/bin/kore" "ls" "/proc"))
        (say1 (+ "-- self-check: kore ls /proc -> exit " (+ (show (cap r))
                 (+ ", " (+ (show (tally (cup r))) " bytes --"))))))

   ; pid 1's forever loop: harvest every child (waitpid(-1) via `glean`). the console dying
   ; RESPAWNS it (a getty); any other pid is a reparented orphan we just harvest. a
   ; non-pair glean (nothing pending / -ECHILD) paces the loop so we never busy-spin.
   (harvest shpid)
     (: ev (glean 0)
        (? (two? ev)
             (? (id? (cap ev) shpid)
                  (: _ (say1 "  [init] console exited -> respawn")
                     (harvest (spawn (console 0))))
                  (: _ (say1 "  [init] reaped a reparented orphan") (harvest shpid)))
           (: _ (rest 50) (harvest shpid))))

   (boot-init _)
     (: me (getpid 0)
        _ (say1 (+ "-- love as PID " (+ (show me) " -- the initramfs /init --")))
        (? (= me 1)
             (: _ (say1 "-- mount the early filesystems --")
                _ (mnt "proc"     "/proc" "proc")
                _ (mnt "sysfs"    "/sys"  "sysfs")
                _ (mnt "devtmpfs" "/dev"  "devtmpfs")
                _ (selfcheck 0)
                _ (say1 "-- start the console shell (init -> sh -> real programs) --")
                (harvest (spawn (console 0))))
           (: _ (say1 "  (NOT pid 1 -- host dry run: no mounts, no handoff)") 0))))
(boot-init 0)
