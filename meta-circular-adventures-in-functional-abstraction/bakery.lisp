;;;; bakery.lisp
;;;;
;;;; Cleaned-up code from the semi-epic 2014 blog post by Chris Kohlhepp:
;;;; "Meta-Circular Adventures in Functional Abstraction"
;;;; "Challenging Clojure in Common Lisp"
;;;;
;;;; The original post is down but a mirror can still be found here:
;;;; https://hackerfall.com/story/challenging-clojure-in-common-lisp
;;;;
;;;; Instructions to run:
;;;;
;;;; (asdf:load-system :bakery)
;;;; T
;;;;
;;;; (use-package :bakery)
;;;; T
;;;;
;;;; (defvar mybaker (baker))
;;;; CAKE OBSERVER: Ingredients now NIL.
;;;; CAKE OBSERVER: Completed tasks now NIL.
;;;; MYBAKER
;;;;
;;;; (send mybaker '(:add :flour))
;;;; ; No value
;;;;
;;;; From here on, see *inferior-lisp* buffer for observer output:
;;;;
;;;; CAKE OBSERVER: Ingredients now (FLOUR).
;;;;
;;;; (send mybaker '(:act :knead))
;;;; ; No value
;;;; ERROR: Batter not ready. Can't knead dough.
;;;;
;;;; (send mybaker '(:add :milk))
;;;; ; No value
;;;; CAKE OBSERVER: Ingredients now (MILK FLOUR).
;;;;
;;;; (send mybaker '(:add :eggs))
;;;; ; No value
;;;; CAKE OBSERVER: Ingredients now (EGGS MILK FLOUR).
;;;; CAKE OBSERVER: Batter now complete!
;;;;
;;;; (send mybaker '(:add :pepper))
;;;; ; No value
;;;; ERROR: recipe error.
;;;;
;;;; (send mybaker '(:add :milk))
;;;; ; No value
;;;; ERROR: Batter already complete. Don't need MILK.
;;;;
;;;; (send mybaker '(:act :knead))
;;;; ; No value
;;;; CAKE OBSERVER: Completed tasks now (KNEAD).
;;;;
;;;; (send mybaker '(:act :bake))
;;;; ; No value
;;;; CAKE OBSERVER: Completed tasks now (BAKE KNEAD).
;;;;
;;;; (send mybaker '(:add :sugar))
;;;; ; No value
;;;; CAKE OBSERVER: Ingredients now (SUGAR EGGS MILK FLOUR).
;;;;
;;;; (send mybaker '(:add :candles))
;;;; ; No value
;;;; CAKE OBSERVER: Ingredients now (CANDLES SUGAR EGGS MILK FLOUR).
;;;; CAKE OBSERVER: Completed tasks now (DECORATE BAKE KNEAD).
;;;; CAKE OBSERVER: Cake is all done!

(defpackage :bakery
  ; import namespaces from the following packages
  (:use :common-lisp :cl-actors :optima :bordeaux-threads :cells)

  ; bakery package exported symbols
  (:export #:baker
           :cake))

(in-package :bakery)

(defun construct-accumulator ()
  "A closure constructor, lambda-over-let-over-lambda pattern"
  (let ((elements (list)))
    (lambda (element)
      (setf elements (remove nil (adjoin element elements)))
      elements)))

(defvar *all-ingredients-fu* nil)
(defvar *all-dones-fu* nil)

(declaim (sb-ext:muffle-conditions style-warning))
(declaim (sb-ext:muffle-conditions sb-kernel:redefinition-with-defmethod))

;;; The model defines nodes and how edges connect them, to build a computation
;;; graph. Incidentally the model derives from the Common Lisp Object System
;;; (CLOS). Hence the Common Lisp object-oriented semantics and syntax hold.

(defmodel cake ()
  (
   ;; Closures
   ;; ---------------------------------------------
   (allingredientsfu
    :cell nil
    :accessor allingredientsfu
    :initform *all-ingredients-fu*)

   (alldonesfu
    :cell nil
    :accessor alldonesfu
    :initform *all-dones-fu*)

   ;; Constraints
   ;; The reader attribute denotes a read-only
   ;; accessor or immutable value
   ;; ---------------------------------------------

   ;; To have batter we need milk, eggs & flour
   (batter :reader batter :initform '(:milk :eggs :flour))

   ;; To have an iced cake we must have sugar topping
   (icing :reader icing :initform '(:sugar))

   ;; To have a birthday cake we must have candles
   (decoration :reader decoration :initform '(:candles))

   ;; Basic actions that need to be performed
   (todos :reader todos :initform '(:knead :bake :decorate))

   ;; Events Nodes
   ;; ---------------------------------------------

   ;; A new ingredient is mixed in
   (mixin :initform (c-in nil))

   ;; A new action is performed
   (action :accessor action :initform (c-in nil))

   ;; Dependent Nodes --- initforms represent edges
   ;; ---------------------------------------------

   ;; At any time the total set of ingredients
   ;; is the set union of the last mixin and all previous ingredients
   (ingredients
    :accessor ingredients
    :initform (c? (funcall (allingredientsfu self) (mixin self))))

   ;; At any time the set of done actions or "dones"
   ;; is the set union of the last action and all previous actions
   (dones
    :accessor dones
    :initform (c? (funcall (alldonesfu self) (action self))))

   ;; Batter predicate "batter-p": At any time batterp is satisfied
   ;; if the batter constraint set is a subset of the ingredients.
   ;; This model permits adding other ingredients, such as spices
   ;; so the subset relationship is a good fit here.
   (batter-p
    :accessor batter-p
    :initform (c? (subsetp (batter self) (ingredients self))))

   ;; All done predicate "alldone-p": At any time we are "all done"
   ;; if the set difference of todos and the set of dones is an empty set.
   ;; This essentially says, follow the recipe. If you perform other tasks
   ;; we don't warrant the outcome. The cake may be destroyed.
   (alldone-p
    :accessor alldone-p
    :initform (c? (not (set-difference (todos self) (dones self)))))

   )
  )

(declaim (sb-ext:unmuffle-conditions style-warning))
(declaim (sb-ext:unmuffle-conditions sb-kernel:redefinition-with-defmethod))

(defobserver batter-p ((self cake))
  "An observer on cell batter-p on instances of cake models"
  (if new-value
      (format t "~%CAKE OBSERVER: Batter now complete!")))

(defobserver alldone-p ((self cake))
  "An observer on cell alldone-p on instances of cake models"
  (if new-value
      (format t "~%CAKE OBSERVER: Cake is all done!")))

(defobserver ingredients ((self cake))
  "An observer on cell ingredients on instances of cake models"
  (format t "~%CAKE OBSERVER: Ingredients now ~A." new-value))

(defobserver dones ((self cake))
  "An observer on cell dones on instances of cake models"
  (format t "~%CAKE OBSERVER: Completed tasks now ~A." new-value))


;;; An abstraction of RULES/INVARIANTS
;;; Only state is cake itself

;;; (defactor actor-class
;;;          (state)
;;;          (message-vars)
;;;          behavior)

(defactor baker
  ;; State Form - let bindings for actor local state
  ;; -----------------------------------------------
  ((*all-ingredients-fu* (construct-accumulator))
   (*all-dones-fu* (construct-accumulator))
   (mycake (make-instance 'cake)))

  ;; Message Form - We match on a single argument
  ;; --------------------------------------------
  (message)

  ;; Behavior Form
  ;; --------------------------------------------
  (match message
    ; match adding batter ingredients only
    ((list :add ingredient) when
     (member ingredient (batter mycake))

     (if (batter-p mycake) ; batter already done ?
         (format t "~%ERROR: Batter complete. Don't need ~A." ingredient)
         (if (member ingredient (ingredients mycake))
             (format t "~%Error: Already have ~A in batter." ingredient)
             (setf (mixin mycake) ingredient)))) ; update cake here

    ; match adding Icing ingredients but only after baking
    ((list :add ingredient) when
     (and (member ingredient (icing mycake))
          (member :bake (dones mycake)))

     (if (member ingredient (ingredients mycake))
         (format t "~%ERROR: Already have ~A on cake." ingredient)
         (setf (mixin mycake) ingredient))) ; update cake here

    ; match adding Decoration ingredients but only after baking
    ((list :add ingredient) when
     (and (member ingredient (decoration mycake))
          (member :bake (dones mycake)))

     (if (member ingredient (ingredients mycake))
         (format t "~%ERROR: Already have ~A on cake." ingredient)
         (progn
           (setf (mixin mycake) ingredient) ; update cake here
           (setf (action mycake) :decorate)))) ; update cake here

    ; match actions
    ((list :act todo) when
     (member todo (todos mycake))

     (if (alldone-p mycake) ; cake already finished?
         (format t "~%ERROR: Cake is finished. Decline to do ~A." todo)
         (if (member todo (dones mycake)) ; todo already done?
             (format nil "~%ERROR: Already did ~A." todo)

             (cond ((equal todo :bake) ; bake only after kneading dough
                    (if (not (member :knead (dones mycake)))
                        (format t "~%ERROR: Knead batter first. Can't do ~A."
                                todo)
                        (setf (action mycake) todo) ; update cake here
                        )
                    )
                   ((equal todo :knead) ; knead dough only after batter complete
                    (if (not (batter-p mycake))
                        (format t "~%ERROR: Batter not ready. Can't knead dough.")
                        (setf (action mycake) todo) ; update cake here
                        )
                    )
                   (t (format t "~%ERROR: Don't know ~A." todo ))))))

    ; fall-through
    (_
     (format t "~%ERROR: recipe error."))

    )

  ;; Match next message
  ;; ------------------
  next)
