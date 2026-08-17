; crew/kore/asbook.l -- make sure the assembler module is REGISTERED for the cat:
; the wrapped crew/holo files (backends, elf/obj/link, copy) REOPEN module 'holo,
; and holo's readers say (use 'holo) scoped or probe (from 'holo ..) -- nothing
; reads it off an ambient splice anymore. the load rides an anonymous layer, so
; use's splice is popped and nothing lands in the cat's own walk. kore's cat
; order: .. asbook.l, crew/holo/elf.l, ..; mooncc's: this, then the cross
; backends join the module (defbackend mutates its table), then elf/obj/link.
(: _ (? (tablet? (from 'holo)) 0
        (: _ (enter ()) _ (use 'holo) (leave ())))
   0)
