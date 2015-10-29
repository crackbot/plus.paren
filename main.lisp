
(in-package :plus.paren)

(defpslib "plus"
    :package :plus.paren
    :runtime (list 'progn *standard-cl* *plus-library* *jsclass-runtime*))

(defsection @main-manual (:title "Plus manual")
  (plus.paren asdf:system)
  
  "plus.paren provides more common lisp functions ported to parenscript as well as some other non standard cl but useful in js functions and utilities"

  (@general-utilities section)
  (@class-manual section)
  (@anaphorics-manual section)
  (@runtime-manual section))

(defsection @general-utilities (:title "Random utilities")
  (create* psmacro)
  (-> psmacro)
  (defun/log psmacro)
  (defun/partial psmacro))

(defpsmacro create* (&rest body)
  "(create* some-variable value)
   ->
   (let ((obj (create)))
     obj[some-variable] = value;
     obj)"
  `(let ((obj (create)))
     (progn
       ,@(loop for key-value on body by #'cddr
              collect `(setf (getprop obj ,(car key-value)) ,(cadr key-value))))
     obj))

(defpsmacro -> (&rest body)
  "A shortcut for function call on an object, same as chain but
  shorter"
  `(chain ,@body))

(defpsmacro defun/partial (name lambda-list &body body)
  "Hello world"
  `(defun ,name ,lambda-list
     (let ((self this)
           (arity ,(length lambda-list))
           (args (chain *array prototype slice (call arguments))))
     (if (>= (@ args length) arity)
         (progn
           (chain (lambda ,lambda-list
                    ,@body) (apply this args)))
         (lambda ()
           (let ((next-args (chain *array prototype slice (call arguments))))
             (chain ,name (apply self (chain args (concat next-args))))))))))
