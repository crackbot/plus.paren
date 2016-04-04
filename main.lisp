
(in-package :plus.paren)

(defpslib "plus"
    :package :plus.paren
    :runtime (list 'progn *standard-cl* *plus-library* *jsclass-runtime*))

(defsection @main-manual (:title "Plus manual")
  (plus.paren asdf:system)
  
  "plus.paren provides more common lisp functions ported to parenscript as well as some other non standard cl but useful in js functions and utilities"

  (@general-utilities section)
  (@class-manual section)
  (@bindings-manual section)
  (@runtime-manual section))

(defsection @general-utilities (:title "Random utilities")
  (-> psmacro)
  (@% psmacro)
  (defun/partial psmacro)
  (create* psmacro)
  ;(error psmacro)
  (dolist-idx psmacro)
  (switch-with-break psmacro))

(defpsmacro create* (&rest body)
  "Different way of creating an object, where each key can be a variable.

```lisp
(create* some-variable value)
```

   expands into

```lisp
   (let ((obj (create)))
     obj[some-variable] = value;
     obj)
```"
  `(let ((obj (create)))
     (progn
       ,@(loop for key-value on body by #'cddr
              collect `(setf (getprop obj ,(car key-value)) ,(cadr key-value))))
     obj))

(defpsmacro error (msg)
  "Throw error with MSG"
  `(throw (new (*error ,msg))))

(defpsmacro -> (&rest body)
  "A shortcut for function call on an object, same as chain but
  shorter"
  `(chain ,@body))

(defpsmacro @% (&rest body)
  "Shortcut for this accessor"
  `(@ this ,@body))

(defpsmacro defun/partial (name lambda-list &body body)
  "Define a function with partial application support.

   For example

```lisp
(defun/partial sum (x y z)
  (+ x y z))
```

   Can be executed as

```lisp
(((sum 1) 2) 3)
((sum 1 2) 3)
(sum 1 2 3)
```

   With equivalent result."
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

(defpsmacro dolist-idx ((var list-idx array &optional (result nil result?)) &body body)
  "Dolist with one additional variable that is binded to array index."
  (let* ((idx (ps-gensym "_JS_IDX"))
         (introduce-array-var? (not (symbolp array)))
         (arrvar (if introduce-array-var?
                     (ps-gensym "_JS_ARRVAR")
                     array)))
    `(do* (,var
           ,@(when introduce-array-var?
                   (list (list arrvar array)))
           (,idx 0 (1+ ,idx)))
          ((>= ,idx (getprop ,arrvar 'length))
           ,@(when result? (list result)))
       (setq ,var (aref ,arrvar ,idx)
             ,list-idx ,idx)
       ,@body)))

(defpsmacro switch-with-break (val &body states)
  "Switch with explicit break, so you don't have to add (break) to
every form."
  (dolist (state states)
    (nconc (cdr state) '((break))))
  `(switch ,val
     ,@states))
