
(in-package :plus.paren)

(defparameter *with-self* t
  "When set to t will expand all defun forms with \"self\" variable
  defined and bound to this, default is t")

(defvar defun-names '(defun defun/contract))
(defvar defun-name-transforms '((defun . lambda)
                                (defun/contract . lambda/contract)))

;; Avoid instantiating the base class just to setup inheritance
;; See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/create
;; for a polyfill
;; Also, do a recursive merge of two prototypes, so we don't overwrite 
;; the existing prototype, but still maintain the inheritance chain
;; Thanks to @ccnokes

(defparameter *jsclass-runtime*
  '(progn
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
  "Runtime defines only one method - extend, which can be used to
  setup inheritance for javascript objects")

;(defpslib "jsclass" :runtime *jsclass-runtime*)

(defun defun-form-p (form)
  "Check if form is a function definition form"
  (member (symbol-name (car form)) (mapcar #'symbol-name defun-names) :test #'equalp))

(defun transform-defun (defun-form)
  "Transforms named defun form to anonymous lambda form"
  (let* ((name (car defun-form))
         (form (remove-if (constantly t) defun-form :start 1 :end 2))
         (name-replacement (find-if #'(lambda (pair)
                                         (equalp (symbol-name name) (symbol-name (car pair))))
                                    defun-name-transforms)))
    (rplaca form (cdr name-replacement))
    form))
            
(defun parse-class-body (body)
  "Splits class definition into class initialization code and methods"
  (let ((forms (remove-if #'defun-form-p body))
        (defuns (remove-if-not #'defun-form-p body)))
    (values forms defuns)))

(defpsmacro defjsclass (name super-classes &rest body)
  "Defines new javascript class with super classes. Syntax for class
  definition is the following:
  (defjsclass $name ($superclass ...)
    $initialization forms
    (defun $method-name ($method-param ...)
      $method-code))"
  (multiple-value-bind (forms defuns)
      (parse-class-body body)
    `(progn (defun ,name (&rest args)
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
                   ;`(extend ,(car super-class) ,name))
            
            ,@(mapcar #'(lambda (def)
                          `(setf (ps:@ ,name prototype ,(cadr def))
                                 ,(transform-defun def)))
                      defuns))))

(defpsmacro defmeta (name &rest body)
  "Define meta on a class"
  (flet ((flatten-car (lst)
           (let ((flt '()))
             (dolist (obj lst)
               (setf flt (nconc flt obj)))
             flt)))
    `(setf (@ ,name meta) (create ,@(flatten-car body)))))

(defpsmacro setf% (&rest args)
  (assert (evenp (length args)) ()
          "~s does not have an even number of arguments." `(this-setf ,args))
  `(progn ,@(loop for (place value) on args by #'cddr collect
                 (aif (and (listp place) (gethash (car place) ps::*setf-expanders*))
                      (funcall it (cdr place) value)
                      `(ps::ps-assign (@ this ,place) ,value)))))