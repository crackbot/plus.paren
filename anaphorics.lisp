
(in-package :plus.paren)

(defsection @anaphorics-manual (:title "Anaphoric macros for Parenscript")
  (when-let psmacro)
  )

(defpsmacro when-let (bindings &body forms)
  "Creates new variable bindings, and conditionally executes FORMS."
  (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,binding-list
       (when (and ,@variables)
         ,@forms))))
