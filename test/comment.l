; comment syntax: `;` runs to end of line; `#!` is a shebang line comment.
; there are no block comments -- every comment ends at the newline.
; a plain `;` line comment (this line) is skipped.
;; two semicolons: still just a line comment.
;;; three semicolons: also just a line comment now (the old block form is gone).
;;; (putx "if this printed, ;;; still started a block") (putc 10)
#! a `#!` shebang line is skipped too -- ) ) ( ( " weird tokens are fine here
(: cmt-after 7)  ;;; a trailing ;;; is an ordinary line comment
(assert
 (= 7 cmt-after)                                ; survived the ;;; / #! / ;; lines above
 (= 1 1))