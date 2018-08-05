;;;; simple-evaluator-2.lisp;
;;;;
;;;; This version by Rainer Joswig, joswig@lisp.de, of the following blog post:
;;;
;;;; "Simple expression evaluator comparison between Haskell, Rust, and Common Lisp"
;;;;
;;;; August 4, 2018 ~ Talking with a Lisp Blog - by Timmy Jose
;;;;
;;;; https://z0ltan.wordpress.com/2018/08/04/simple-expression-evaluator-comparison-between-haskell-rust-and-common-lisp/

;; We create structures for the expression types.
;; Each structure defines a constructor of the same name.
;; Each expression knows the corresponding Lisp function.

(defstruct (val (:constructor val (e))) e)
(defstruct (bop :conc-name) e1 e2 op)

(defmacro defbinops (&rest names)
  `(progn ,@(loop for (name op) in names
                  collect `(defstruct (,name (:include bop (op ',op)) (:constructor ,name (e1 e2)))))))

(defbinops (mul *) (div /) (add +) (sub -))

;; CLOS methods for evaluation.
;; We use two functions - the first function catches the errors and returns NIL then

(defmethod evaluate-2 (e)
  (ignore-errors (evaluate-2b e)))

(defmethod evaluate-2b ((e val))
  (val-e e))

(defmethod evaluate-2b ((e bop))
  (funcall (op e) (evaluate-2 (e1 e)) (evaluate-2 (e2 e))))

(defun example-2 ()
  (format t "~s~%" (evaluate-2 (Add (Val 2) (Mul (Val 3) (Val 6)))))
  (format t "~s~%" (evaluate-2 (Div (Val 10) (Val 0))))
  (format t "~s~%" (evaluate-2 (Div (Val 20) (Val 3)))))

;;;; Compile, then run in Lisp image:
;;;; (example-2)
