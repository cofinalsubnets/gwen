; --- test/kernel/svm.l: the AMD-V spike (doc/archive/svm.md) -- one guest, one exit.
; GUARDED TWICE, and both guards are the tree's own laws. The nom is in the book
; only on x86_64, so presence is asked of (names ()) and never by reading it (a
; read would be a missing condition, not an absence); and the CPU under that
; seat may still be an Intel one, or a qemu model without the feature, so
; (svm ()) answers for the silicon. Either way this file says so and stays
; green -- an absence is not a divergence.
;
; What the three laws below separate: that the ENTRY was accepted (a refused one
; answers -1, INVALID, and nothing else), that a guest instruction RETIRED (the
; sentinel is in the guest's own rax, and only the guest could have put it
; there), and that the intercept fired where it should (the saved rip is the
; cpuid's own address, one instruction in -- not 0, and not the hlt's 5).
(: had (member? 'svm (names ()))
   (? (! (? had (svm ()) ()))
      (: _ (puts "svm: no AMD-V here, laws skipped") _ (putc 10) ())
      (: r (svm-run ())
         _ (assert
            (3 = (tally r))          ; (exitcode rax rip) -- a refusal answers ()
            (114 = <r)               ; 0x72, the CPUID intercept
            (4660 = <>r)             ; 0x1234, the sentinel the guest loaded
            (3 = <>>r))              ; the cpuid's own address
         _ (puts "svm: guest ran, cpuid intercepted") _ (putc 10)
         ())))
