
(in-package :plus.paren)

(defsection @class-manual (:title "Class definitions for Javascript")
  (defjsclass psmacro)
  (defmeta psmacro)
  (setf% psmacro)
  (*with-self* variable))

(defparameter *with-self* t
  "When set to t will expand all defun forms with \"self\" variable
  defined and bound to this, default is t")

(defvar defun-names '(defun defun/contract))
(defvar defun-name-transforms '((defun . lambda)
                                (defun/contract . lambda/contract)))

;; Avoid instantiating the base class just to setup inheritance See
;; https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/create
;; for a polyfill
;; Also, do a recursive merge of two prototypes, so we don't overwrite
;; the existing prototype, but still maintain the inheritance chain
;; Thanks to @ccnokes

(defparameter *jsclass-runtime*
  '(progn
    (defun build-id (&key prefix)
      (flet ((gen-id (&optional (length 8))
               (let* ((res (array))
                      (possible "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
                      (possible-length (@ possible length)))
                 (dotimes (i length)
                   (chain res (push (chain possible (char-at (floor (* (random) possible-length)))))))
                 (chain res (join "")))))
        (if prefix
            (+ prefix (gen-id))
            (gen-id))))
    
    (defun extend (base sub)
      (let ((orig-proto (@ sub prototype)))

        (setf (@ sub prototype) (chain *object (create (@ base prototype))))
        
        (dolist (key orig-proto)
          (setf (getprop sub 'prototype key) (getprop orig-proto key)))
        ;; Remember the constructor property was set wrong, let's fix it
        (setf (@ sub prototype constructor) sub)
        ;; In ECMAScript5+ (all modern browsers), you can make the constructor property
        ;; non-enumerable if you define it like this instead
        (chain *object (define-property (@ sub prototype) "constructor"
                         (create :enumerable false
                                 :value sub))))))
  "Runtime defines only one method - extend, which is used to setup
  inheritance for javascript objects")

(defun defun-form-p (form)
  "Check if form is a function definition form"
  (if (listp form)
      (member (symbol-name (car form))
              (mapcar #'symbol-name defun-names) :test #'equalp)
      nil))

(defun extension-p (form)
  "Check if form is an extension. Extension is a list that start with
a keyword."
  (and (listp form)
       (keywordp (car form))))

(defun transform-defun-body (body)
  "Transform defun body, possibly adding reference to self if
*WITH-SELF* is true."
  (if *with-self*
      (list 'progn
            '(setf self this)
            body)
      body))

(defun transform-defun (defun-form)
  "Transforms defun form to anonymous lambda form"
  (destructuring-bind (form name args body)
      defun-form
    (declare (ignore name))
    (list (cdr (find-if #'(lambda (pair)
                            (equalp (symbol-name form) (symbol-name (car pair))))
                        defun-name-transforms))
          args
          (transform-defun-body body))))
              
(defun parse-class-body (body)
  "Split class body into parts

  - **documentation** documentation string
  - **extensions** extensions used by class check TRANSFORM-EXTENSIONS
  - **forms** any other forms that are left
  - **defuns** functions definitions
"
  (let* ((defuns (remove-if-not #'defun-form-p body))
         (documentation (if (stringp (car body))
                            (car body)
                            nil))
         (remaining-forms (if documentation
                              (remove-if #'defun-form-p (cdr body))
                              (remove-if #'defun-form-p body)))
         (extensions (remove-if-not #'extension-p remaining-forms))
         (forms (remove-if #'extension-p remaining-forms)))
    (values documentation extensions forms defuns)))

(defun find-defun (name forms)
  (dolist (form forms)
    (when (find name form)
      (return-from find-defun form))))

;; (defun/contract hello (a b)
;;   (>> intp intp intp)
;;   (+ a b))

(defgeneric transform-extensions (ext defs forms defuns)
  (:documentation ""))

(defmethod transform-extensions ((ext (eql :contracts)) defs forms defuns)
  (dolist (def defs)
    (destructuring-bind (defun-name contract)
        def
      (let ((defun-form (find-defun defun-name defuns)))
        (when defun-form
          (setf defun-form (add-contract contract defun-form)))))))

(defpsmacro defjsclass (name super-classes &rest body)
  "Create parenscript classes.
   This form provides lighweight class definition.
   Syntax for class definition is the following:

```lisp
  (defjsclass $name ($superclass ...)
    \"Documentation string\"
    (:extensions)
    $initialization forms
    (defun $method-name ($method-param ...)
      $method-code))
```
"
  (multiple-value-bind (documentation extensions forms defuns)
      (parse-class-body body)
    (declare (ignore extensions))
    ;; (let ((ext-forms nil))
      ;; (dolist (ext extensions)
      ;;   (pushnew (transform-extensions (car ext) (cdr ext) forms defuns)
      ;;            ext-forms))
      
      `(progn (defun ,name (&rest args)
                ,documentation
                (setf (@ this meta) (@ this constructor meta)
                      (@ this id) (build-id))
                
                (when (in "initialize" this)
                  (apply (@ this initialize) args))
                
                ,@forms

                this)

              ,@(when super-classes
                      (mapcar #'(lambda (cls)
                                  `(extend ,cls ,name))
                              super-classes))
              
              ;; ,@ext-forms
              
              ,@(mapcar #'(lambda (def)
                            `(setf (ps:@ ,name prototype ,(cadr def))
                                   ,(transform-defun def)))
                        defuns))))

(defpsmacro defmeta (name &rest body)
  "Define meta for a class.

   Meta object can be used to store properties that are static for
   every class instance."
  (flet ((flatten-car (lst)
           (let ((flt '()))
             (dolist (obj lst)
               (setf flt (nconc flt obj)))
             flt)))
    `(setf (@ ,name meta) (create ,@(flatten-car body)))))

(defpsmacro setf% (&rest args)
  "Call setf with every key arg bound to this.

```lisp
  (setf% hello \"world\")
```

  Is equivalent to

```lisp
  (setf (@ this hello) \"world\")
```
"
  (assert (evenp (length args)) ()
          "~s does not have an even number of arguments." `(setf% ,args))
  `(progn ,@(loop for (place value) on args by #'cddr collect
                 (aif (and (listp place) (gethash (car place) ps::*setf-expanders*))
                      (funcall it (cdr place) value)
                      `(ps::ps-assign (@ this ,place) ,value)))))
