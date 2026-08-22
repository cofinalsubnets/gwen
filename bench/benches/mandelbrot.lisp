;;; mandelbrot escape counts over a 128x128 grid of c in [-2,1]x[-1.5,1.5] (see
;;; bench/benches/mandelbrot.l). Same op order as the reference
;;; (zr*zr - zi*zi + cr ; 2.0*zr*zi + ci), the grid step 3.0/W computed first.
;;; the inner loop is declared double-float so it stays unboxed -- bit-exact
;;; IEEE-754, so the escape counts (checksum 424578) match every other port.
;;; note CL's `0.0` is SINGLE -- the kernel uses `d0` literals throughout.
(load "lib/bench.lisp")
(defconstant +maxit+ 128)
(defconstant +w+ 128)
(defconstant +h+ 128)
(declaim (ftype (function (double-float double-float) fixnum) pix))
(defun pix (cr ci)
  (declare (optimize (speed 3) (safety 0)) (type double-float cr ci))
  (let ((zr 0.0d0) (zi 0.0d0) (it 0))
    (declare (type double-float zr zi) (type fixnum it))
    (loop while (and (< it +maxit+) (<= (+ (* zr zr) (* zi zi)) 4.0d0)) do
      (let ((nzr (+ (- (* zr zr) (* zi zi)) cr)))
        (declare (type double-float nzr))
        (setf zi (+ (* (* 2.0d0 zr) zi) ci)
              zr nzr
              it (+ it 1))))
    it))
(defun work ()
  (declare (optimize (speed 3) (safety 0)))
  (let ((s 0))
    (declare (type fixnum s))
    (dotimes (py +h+)
      (let ((ci (+ -1.5d0 (* py (/ 3.0d0 +h+)))))
        (declare (type double-float ci))
        (dotimes (px +w+)
          (let ((cr (+ -2.0d0 (* px (/ 3.0d0 +w+)))))
            (declare (type double-float cr))
            (incf s (pix cr ci))))))
    s))
(bench "mandelbrot" (lambda () (work)))
