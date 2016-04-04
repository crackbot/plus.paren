
(in-package :plus.paren)

(defsection @bindings-manual (:title "Bindings macros for Parenscript")
  (when-let psmacro))

(defpsmacro when-let (bindings &body forms)
  "Create new variable bindings, and conditionally executes FORMS."
  (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,binding-list
       (when (and ,@variables)
         ,@forms))))
