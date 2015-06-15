
(in-package :plus.paren)

(defpsmacro when-let (bindings &body forms)
  "Creates new variable bindings, and conditionally executes FORMS."
  (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,binding-list
       (when (and ,@variables)
         ,@forms))))
