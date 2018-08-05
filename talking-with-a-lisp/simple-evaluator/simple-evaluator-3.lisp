;;;; simple-evaluator-3.lisp
;;;;
;;;; Copyright (c) 2018 Robert Smith

;;;; Another Common Lisp version of the Haskell code from the blog post:
;;;;
;;;; "Simple expression evaluator comparison between Haskell, Rust, and Common Lisp"
;;;;
;;;; August 4, 2018 ~ Talking with a Lisp Blog - by Timmy Jose
;;;;
;;;; https://z0ltan.wordpress.com/2018/08/04/simple-expression-evaluator-comparison-between-haskell-rust-and-common-lisp/

;; (ql:quickload :algebraic-data-library)

(adt:defdata op add sub mul div)

(adt:defdata expr
  (val integer)
  (app op expr expr))

(defun ev (e)
  (adt:match expr e
    ((val n) (adl:just n))
    ((app o e1 e2)
     (adl:>>= (ev e1)
              (lambda (v1)
                (adl:>>= (ev e2)
                         (lambda (v2)
                           (adt:match op o
                             (add (adl:just (+ v1 v2)))
                             (sub (adl:just (- v1 v2)))
                             (mul (adl:just (* v1 v2)))
                             (div (if (zerop v2)
                                      adl:nothing
                                      (adl:just (floor v1 v2))))))))))))

(defvar *e1* (app add (val 2) (app mul (val 3) (val 6))))
(defvar *e2* (app mul (app add (val 1) (val 3)) (app div (val 10) (val 0))))
(defvar *e3* (app mul (app add (val 1) (val 3)) (app div (val 10) (val 3))))
(defvar *e4* (app div (val 10) (val 3)))

(defun example-3 ()
  (print (ev *e1*))   ; => #.(JUST 20)
  (print (ev *e2*))   ; => #.NOTHING
  (print (ev *e3*))   ; => #.(JUST 12)
  (print (ev *e4*)))  ; => #.(JUST 3)

;;;; Compile, then run in Lisp image:
;;;; (example-3)
