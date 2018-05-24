;;;; allegro.lisp

;;;; implementation-independent documentation can be found in doc.lisp
(in-package #:introspect-environment)

;;; implementations implementing the CLtL2 non-standard have this easy.

(defun specialp (name &optional env)
  (eq (variable-information name env t) :special))

(defun variable-type (name &optional env)
  (or (cadr (assoc 'type (nth-value 2 (variable-information name env t))))
      't))
(defun function-type (name &optional env)
  (or (cadr (assoc 'ftype (nth-value 2 (function-information name env t))))
      '(function * *)))

(defun policy-quality (quality &optional env)
  (or (second (assoc quality (declaration-information 'optimize env)))
      (error "Unknown policy quality ~s" quality)))
(defmacro policy (expr &optional env)
  ;; conveniently, declaration-information 'optimize is specified to
  ;; always return an alist with all optimization qualities.
  (let ((qualities (mapcar #'car (declaration-information 'optimize)))
	(optvar (gensym "POLICY")))
    `(let ((,optvar (declaration-information 'optimize ,env)))
       ;; cltl2 has an alist of lists instead of just conses.
       ;; dunno why. anyway it means we use second for cdr.
       (symbol-macrolet
	   ,(mapcar (lambda (quality)
		      `(,quality (second (assoc ',quality ,optvar))))
             qualities)
	 ;; CLHS 11.1.2.1.2.1 (ref because wow obscure) explicitly
	 ;; allows standard symbols that aren't variables
	 ;; to be symbol-macrolut.
	 ;; This may not be true of implementation-specific packages.
	 ,expr))))

(defun parse-macro (name lambda-list body &optional env)
  (excl::defmacro-expander `(,name ,lambda-list ,@body) env))

(defun parse-compiler-macro (name lambda-list body &optional env)
  (excl::defcmacro-expander `(,name ,lambda-list ,@body) env))

(defun typexpand-1 (type &optional env)
  "This implementation is not supported; no types are expanded."
  (declare (ignore env))
  (check-type type (or symbol cons class) "a type specifier")
  (values type nil))
(defun typexpand (type &optional env)
  (excl:normalize-type type
                       :environment env
                       :default type))

(defun constant-form-value (form &optional env)
  (sys:constant-value form env))
