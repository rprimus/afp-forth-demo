(defpackage afp-forth-demo
  (:use :cl)
  (:local-nicknames (:a :alexandria)))
(in-package :afp-forth-demo)

#|
registers on the forth machine
- pstack : parameter stack (where parameters will be stored until operation requires them)
- rstack : return stack (holds the instruction of the operation (i.e. forth word) to execute)
- dict : dictionary of words
- compiling : 
- dtable : 
|#
(defparameter forth-registers
  '(pstack rstack pc dict compiling dtable))

#|

defines the words in the dictionary
dictionary = linked list of words that we would use to see if a word is defined.
name - name of symbol
prev - previous pointer to previous word in dictionary
immediate - whether or not word is evaluated at compile-time or run-time like a lisp macro
thread - used in metaprogramming as well. This helps in determining how to structure code
specifically, on what level of indirection should a word be compiled to one of the following
strategies:
  - calling a subroutine (subroutine-threaded code)
  - calling an adjacent memory cell pointing to an instruction (so called indirect/direct threaded code)
  - accessing a fixnum that representing a pointer to the word in the same compilation thread (token threading)
  - Take advantage of Lisp's dynamic typing and cons cell list structure to access the next execution 
    form (called cons-threaded code in the book).
|#
(defstruct forth-word
  name
  prev
  immediate
  thread)

(defun forth-lookup (w top-word)
  "lookup word W in dictionary starting at TOP-WORD."
  (if top-word
      (if (eql (forth-word-name top-word) w)
	  top-word
	  (forth-lookup w (forth-word-prev top-word)))))

;; Common Lisp
;; (+ 1 2)
;; Forth
;; 1 2 + 
(defmacro forth-inner-interpreter ()
  "simple interpreter for forth"
  `(loop
     :do (cond
	   ((functionp (car pc)) (funcall (car pc)))
	   ((consp (car pc))
	    (push (cdr pc) rstack)
	    (setf pc (car pc)))
	   ((null pc)
	    (setf pc (pop rstack)))
	   (t
	    (push (car pc) pstack)
	    (setf pc (cdr pc))))
     :until (and (null pc) (null rstack))))

;; dictionary of primitives forms
;; prim-form: (name immediate . forms)
(defparameter *forth-primitive-forms* nil)

;; 
(defmacro def-forth-naked-prim (&body code)
  `(push ',code *forth-primitive-forms*))

(defmacro define-forth-primitive (&body code)
  `(def-forth-naked-prim
     ,@code
     (setf pc (cdr pc))))

;; NOP - no operation
(define-forth-primitive nop nil)

(define-forth-primitive * nil
  (push (* (pop pstack) (pop pstack)) pstack))

(define-forth-primitive + nil
  (push (+ (pop pstack) (pop pstack)) pstack))

;; a b - => b - a
(define-forth-primitive - nil
  (push (- (pop pstack) (pop pstack)) pstack))

(define-forth-primitive / nil
  (push (/ (pop pstack) (pop pstack)) pstack))

;; dup - duplicate argument on pstack
(define-forth-primitive dup nil
  (push (car pstack) pstack))

;; swap
(define-forth-primitive swap nil
  (rotatef (car pstack) (cadr pstack)))

(define-forth-primitive print nil
  (print (pop pstack)))

(define-forth-primitive >r nil
  (push (pop pstack) rstack))

(define-forth-primitive r> nil
  (push (pop rstack) pstack))

;; drop nil
(define-forth-primitive drop nil
  (pop pstack))

;; sketch of forth interpreter that will load up initial dictionary and start
;; looking through passed in
;; (defmacro new-forth-interpreter ()
;;   `(let ,forth-registers
;;      (forth-install-primitives)
;;      (lambda (v)
;;        (let ((word (forth-lookup v dict)))
;; 	 (if word
;; 	     (forth-handle-found)
;; 	     (forth-handle-not-found))))))


;; ;; clumsy way of using our forth interpreter (pre go-forth)
;; ;; forth code : 3 dup * print
;; (progn
;;   (funcall *new-forth* 3)
;;   (funcall *new-forth* 'dup)
;;   (funcall *new-forth* '*)
;;   (funcall *new-forth* 'print))

(defmacro square (x)
  (a:once-only (x)
    `(* ,x ,x)))

(square (+ 1 2))

(defmacro go-forth (forth &body words)
  (a:once-only (forth)
    `(dolist (w ',words)
       (funcall ,forth w))))

;; (go-forth *new-forth*
;;   3 dup * print)

;; variable representing our standard library
(load #P"~/.roswell/local-projects/afp-lol-workshop/src/pandoric-macros.lisp")

(defvar *forth-stdlib* '())

(defmacro forth-stdlib-add (&body all)
  `(setf *forth-stdlib*
	 (nconc *forth-stdlib*
		',all)))

(defmacro alet (letargs &body body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (lambda (&rest params)
       (apply this params))))

(defmacro alet% (letargs &body body)
  `(let ((this) ,@letargs)
     (setf this ,@(last body))
     ,@(butlast body)
     this))

(defmacro new-forth-interpreter ()
  `(alet% ,forth-registers
     (setf dtable (make-hash-table))
     (forth-install-primitives)
     (dolist (v *forth-stdlib*)
       (funcall this v))
     (lambda (v) ,forth-registers
       (let ((word (forth-lookup v dict)))
	 (if word
	     (forth-handle-found)
	     (forth-handle-not-found))))))

(defmacro forth-install-primitives ()
  `(progn
     ,@(mapcar #'(let ((thread (lambda () ,@(cddr a1))))
		   (setf dict (make-forth-word
			       :name ',(car a1)
			       :prev dict
			       :immediate ,(cadr a1)
			       :thread thread)
			 (gethash thread dtable) ',(cddr a1)))
	       *forth-primitive-forms*)))

(defvar *new-forth* (new-forth-interpreter))


