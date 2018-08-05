;;;; simple-evaluator.lisp;
;;;;
;;;; "Simple expression evaluator comparison between Haskell, Rust, and Common Lisp"
;;;;
;;;; August 4, 2018 ~ Talking with a Lisp Blog - by Timmy Jose
;;;;
;;;; https://z0ltan.wordpress.com/2018/08/04/simple-expression-evaluator-comparison-between-haskell-rust-and-common-lisp/

(defun evaluate (e)
  (let ((op (car e)))
    (cond ((eql op 'Val) (cadr e))
          (t (let ((e1 (evaluate (cadr e)))
                   (e2 (evaluate (caddr e))))
               (cond ((eql op 'Add) (+ e1 e2))
                     ((eql op 'Sub) (- e1 e2))
                     ((eql op 'Mul) (* e1 e2))
                     ((eql op 'Div) (if (= e2 0)
                                        nil
                                        (/ e1 e2)))
                     (t (error "invalid operation"))))))))

(defun example ()
  (format t "~s~%" (evaluate '(Add (Val 2 nil) (Mul (Val 3 nil) (Val 6 nil)))))
  (format t "~s~%" (evaluate '(Div (Val 10 nil) (Val 0 nil))))
  (format t "~s~%" (evaluate '(Div (Val 10 nil) (Val 3 nil)))))

;;;; Compile, then run in Lisp image:
;;;; (example)
