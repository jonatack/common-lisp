;;;; simple-evaluator-4.lisp
;;;;
;;;; "Use MLET instead of chain of >>="
;;;;
;;;; Enabled by this new commit in tarbells-are-good/algebraic-data-library:
;;;; https://github.com/tarballs-are-good/algebraic-data-library/commit/4ec6a85
;;;;
;;;; Copyright (c) 2018 Robert Smith
;;;;
;;;; Another Common Lisp version of the Haskell code from the blog post:
;;;;
;;;; "Simple expression evaluator comparison between Haskell, Rust, and Common Lisp"
;;;; August 4, 2018 ~ Talking with a Lisp Blog - by Timmy Jose
;;;; https://z0ltan.wordpress.com/2018/08/04/simple-expression-evaluator-comparison-between-haskell-rust-and-common-lisp/

;;; You may need to clone algebraic-data-library master into your
;;; `~/quicklisp/local-projects/` directory until quicklisp is updated:
;;;
;;; git clone https://github.com/tarballs-are-good/algebraic-data-library.git

;; (ql:quickload :algebraic-data-library)

(adt:defdata op add sub mul div)

(adt:defdata expr
  (val integer)
  (app op expr expr))

(defun ev (e)
  "Use MLET instead of chain of >>="
  (adt:match expr e
    ((val n) (adl:just n))
    ((app o e1 e2)
     (adl:mlet ((v1 (ev e1))
                (v2 (ev e2)))
       (adt:match op o
         (add (adl:just (+ v1 v2)))
         (sub (adl:just (- v1 v2)))
         (mul (adl:just (* v1 v2)))
         (div (if (zerop v2)
                  adl:nothing
                  (adl:just (floor v1 v2)))))))))

(defvar *e1* (app add (val 2) (app mul (val 3) (val 6))))
(defvar *e2* (app mul (app add (val 1) (val 3)) (app div (val 10) (val 0))))
(defvar *e3* (app mul (app add (val 1) (val 3)) (app div (val 10) (val 3))))
(defvar *e4* (app div (val 10) (val 3)))

(defun example-4 ()
  (print (ev *e1*))   ; => #.(JUST 20)
  (print (ev *e2*))   ; => #.NOTHING
  (print (ev *e3*))   ; => #.(JUST 12)
  (print (ev *e4*)))  ; => #.(JUST 3)

;;;; Compile, then run in Lisp image:
;;;; (example-4)
