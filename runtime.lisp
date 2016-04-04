
(in-package :plus.paren)

(defsection @runtime-manual (:title "Runtime manual")
  "Different functions defined as a SERVE.PAREN runtime"
  (@standard-cl section)
  (@plus-library section))

(defsection @standard-cl (:title "Standard CL functions")
  (partial (static-ps-function *standard-cl*))
  (eql (static-ps-function *standard-cl*))
  (find (static-ps-function *standard-cl*))
  (find-if (static-ps-function *standard-cl*))
  (reverse (static-ps-function *standard-cl*))
  (adjoin (static-ps-function *standard-cl*))
  (append (static-ps-function *standard-cl*))
  (every (static-ps-function *standard-cl*))
  (copy-list (static-ps-function *standard-cl*))
  (last (static-ps-function *standard-cl*))
  (remove-if (static-ps-function *standard-cl*))
  (count (static-ps-function *standard-cl*))
  (rest (static-ps-function *standard-cl*))
  (position (static-ps-function *standard-cl*))
  (assoc (static-ps-function *standard-cl*))
  (subseq (static-ps-function *standard-cl*))
  (memeber (static-ps-function *standard-cl*))
  (mapcan (static-ps-function *standard-cl*))
  (mapcar (static-ps-function *standard-cl*))
  (map-into (static-ps-function *standard-cl*))
  (map (static-ps-function *standard-cl*))
  (set-difference (static-ps-function *standard-cl*))
  (reduce (static-ps-function *standard-cl*))
  (nconc (static-ps-function *standard-cl*))
  (maplist (static-ps-function *standard-cl*)))

(defsection @plus-library (:title "plus.paren additional functions")
  (filter (static-ps-function *plus-library*))
  (nreplace (static-ps-function *plus-library*))
  (find-idx (static-ps-function *plus-library*))
  (vals (static-ps-function *plus-library*))
  (keys (static-ps-function *plus-library*))
  (len (static-ps-function *plus-library*))
  (onep (static-ps-function *plus-library*))
  (onem (static-ps-function *plus-library*))
  (defaults (static-ps-function *plus-library*)))

(defparameter *standard-cl*
  '(progn

    ;; boundp 138
    ;; fboundp 149
    ;; reverse 164
    ;; get 203
    ;; third 258
    ;; nconc 297
    ;; nreverse 362
    ;; make-hash-table 416
    ;; typep 655
    ;; consp 660
    ;; make-array 714
    ;; getf 894
    ;; gethash 1126

    ;; (defun get (lookup obj)
    
    (defun partial (fn &rest args)
      (lambda (&rest more-args)
        (-> fn (apply this (-> (array) (concat args more-args))))))
    
  ;;   (defun partial (fn &rest args)
  ;;     (lambda ()
  ;;       (let ((arg 0))
  ;;         (do ((i 0 (1+ i)))
  ;;             ((and (< i (length args)) (< arg arguments.length)))
  ;;           (when (eql (aref args i) undefined)
  ;;             (setf (aref args i)
  ;;                   (aref arguments (1+ arg))
  ;;                   arg (1+ arg))))
  ;;         (-> fn (apply this args)))))
      
  ;;       var fn = this, args = Array.prototype.slice.call(arguments);
  ;;   return function(){
  ;;     var arg = 0;
  ;;     for ( var i = 0; i < args.length && arg < arguments.length; i++ )
  ;;       if ( args[i] === undefined )
  ;;         args[i] = arguments[arg++];
  ;;     return fn.apply(this, args);
  ;;   };
  ;; };
    
    (defun eql (a b)
      "Check if a and b EQL"
      (eql a b))
    
    (defun find (item lst &key (test #'eql))
      "Find item in list using TEST"
      (find-if (partial test item) lst))
    
    (defun find-if (pred lst)
      "Return first value from list that pass PRED test"
      (dolist (item lst)
        (when (pred item)
          (return-from find-if item))))

    (defun reverse (lst)
      "Return reversed list"
      (-> lst (reverse)))
    
    (defun adjoin (item lst &key (test #'eql))
      "Add item to lst unless it's already present"
      (when (not (find item lst))
        (chain lst (push item))))

    (defun append (&rest arrays)
      "Concatenate arrays"
      (let ((res (array)))
        (dolist (arr arrays)
          (setf res (-> res (concat arr))))
        res))
    
    (defmacro string-downcase (str)
      `(chain ,str (to-lower-case))) 

    (defmacro string-upcase (str)
      `(chain ,str (to-upper-case)))

    (defmacro nth (idx array)
      `(aref ,array ,idx))

    (defun every (pred seq)
      (eq (length seq)
          (length (find-if pred seq))))

    (defun copy-list (lst)
      (chain lst (slice 0)))

    (defun last (lst)
      (aref lst (- (length lst) 1)))

    (defun remove-if (pred lst)
      (let ((res (array)))
        (dolist (item lst)
          (when (not (pred item))
            (chain res (push item))))
        res))

    (defun count (item seq)
      (length (find item seq)))

    (defun rest (lst)
      (subseq lst 1))

    (defun position (item seq)
      (cond ((arrayp seq)
             (find item seq))
            ((stringp seq)
             (chain seq (index-of item)))
            (t (error ""))))

    ;; for some reason parenscript complains about "test" key
    (defun assoc (item alist &key (tst #'eql))
      (dolist (alist-item alist)
        (when (tst item (aref alist-item 0))
          (return alist-item))))

    ;; assert 820
    (defmacro assert (pred &optional (msg "Assertion failed"))
      `(when (not ,pred)
         (throw (new (*error ,msg)))))

        (defun subseq (seq start &optional (end (length seq)))
      (cond ((arrayp seq)
             (chain seq (slice start end)))
            ((stringp seq)
             (chain seq (substring start end)))
            (t (error ""))))
    
    (defmacro first (lst)
      `(aref ,lst 0))
    
    (defmacro second (lst)
      `(aref ,lst 1))

    (defun memeber (item lst &key test)
      "Check if ITEM is a member of ARR."
      (if (find item lst :test test) t f))

    (defun mapcan (fun arr)
      (when (and (length arr)
                 (arrayp arr))
        (let ((res (make-array)))
          (dolist (el arr)
            (let ((val (fun el)))
              (if (arrayp val)
                  (setq res (-> res (concat val)))
                  (-> res (push val)))))
          res)))
    
    ;; this are taken from parenscript runtime
    (defun mapcar (fun &rest arrs)
      (when (and (length arrs)
                 (arrayp (aref arrs 0)))
        (let ((result-array (make-array)))
          (if (eq 1 (length arrs))
              (dolist (element (aref arrs 0))
                ((@ result-array push) (fun element)))
              (dotimes (i (length (aref arrs 0)))
                (let ((args-array (mapcar (lambda (a) (aref a i)) arrs)))
                  ((@ result-array push) ((@ fun apply) fun args-array)))))
          result-array)))

    (defun map-into (fn arr)
      "Call FN on each element in ARR, replace element with the return value."
      (let ((idx 0))
        (dolist (el arr)
          (setf (aref arr idx) (fn el))
          (setf idx (1+ idx))))
      arr)

    (defun map (fn arr)
      "Call FN on each element in ARR and return the returned values in a new array."
      ;; In newer versions of ECMAScript, this may call Array.map, too
      (when (or (not arr) (not (length arr)))
        (return))
      
      (let ((idx 0)
            (result (array)))
        (dolist (el arr)
          (setf (aref result idx) (fn el))
          (setf idx (1+ idx)))
        result))
    
    (defun set-difference (arr arr-to-sub)
      "Return a new array with only those elements in ARR that are not in ARR-TO-SUB."
      (let ((idx 0)
            (result (array)))
        (dolist (el arr)
          (unless (member el arr-to-sub)
            (setf (aref result idx) el)
            (setf idx (1+ idx))))
        result))

    (defun reduce (func list &optional init) ;; the use of init here is actually a bit broken wrt null
      (let* ((acc))
        (do* ((i (if init -1 0) (1+ i))
              (acc (if init init (elt list 0)) (func acc (elt list i))))
             ((>= i (1- (length list)))))
        acc))

    (defun nconc (arr &rest arrs)
      (when (and arr (> (length arr) 0))
        (loop :for other :in arrs :when (and other (> (length other) 0)) :do
           ((@ arr :splice :apply) arr
            (append (list (length arr) (length other)) other))))
      arr)

    (defun maplist (fn arr)
      (when (or (not arr) (not (length arr)))
        (return))

      (let ((res (array)))
        (dotimes (i (length arr))
          (push (fn (slice arr 0 (1+ i))) res))
        res)))
  "Standard common lisp functions ported to parenscript")

(defparameter *plus-library*
  '(progn
    
    (defmacro =|| (var1 def1)
      `(when (not ,var1)
         (setf ,var1 ,def1)))
    
    (defun filter (pred lst)
      "Same as remove-if-not"
      (let ((res (array)))
        (dolist (item lst)
          (when (pred item)
            (push item res)))
        res))
    
    (defun/partial pick1 (name obj)
     (getprop obj name))
    
    ;; (defmacro push (item lst)
    ;;   `(chain ,lst (push ,item)))
    
    ;; non CL
    (defmacro push (sym arr)
      `(chain ,arr (push ,sym)))

    ;; (defmacro pop (arr)
    ;;   `(chain ,arr (pop)))

    (defmacro join (sym arr)
      "Create a string by joining two array with sym"
      `(chain ,arr (join ,sym)))

    (defmacro except-last (arr)
      "Return every array elements except last one"
      `(chain ,arr (slice 0 -1)))

    (defmacro last (arr)
      "Return last array element"
      (let ((var (ps::ps-gensym)))
        `(let ((,var ,arr))
           (getprop ,var (- (length ,var) 1)))))

    (defmacro split (sym arr)
      "Split array using sym"
      `(chain ,arr (split ,sym)))
    
    (defmacro slice (arr start &optional end)
      "Splice"
      (if end
          `(chain ,arr (slice ,start ,end))
          `(chain ,arr (slice ,start))))

    (defmacro car (arr)
      `(elt ,arr 0))

    (defmacro cdr (arr)
      `(chain ,arr (slice 1)))
    
    ;; (defmacro slice (arr num)
    ;;   `(chain ,arr (slice ,num)))
    
    (defmacro concat (&rest body)
      `(+ ,@body))
    
    (defun nreplace (lst obj1 obj2 &key test)
      "replace obj1 with obj2 inside lst, eq-fun is used to find obj1
   inside lst"
      (let ((idx (find-idx lst obj1 test)))
        (when (numberp idx)
          (setf (aref lst idx) obj2)))
      lst)
    
    (defun find-idx (lst obj &optional (eq-fun (lambda (x y)
                                             (eq x y))))
      "Same as find but returns index instead of element"
      (let ((res nil))
        (do ((idx 0 (1+ idx)))
            ((>= idx (length lst)))
          (when (eq-fun (aref lst idx) obj)
            (setf res idx)
            (return)))
        res))
    
    (defun vals (obj)
      "Object values"
      (let ((res (array)))
        (for-in (key obj)
          (push (getprop obj key) res))
        res))

    (defun keys (obj)
      "Object keys"
      (let ((res (array)))
        (for-in (key obj)
          (push key res))
        res))

    ;; first order functions
    (defun len (val)
      (length val))
    
    (defun onep (val)
      (1+ val))
    
    (defun onem (val)
      (1- val))

    (defun defaults (obj def)
      "Iterate through DEF keys and values setting any key found in
DEF but not found in OBJ to corresponding value"
      (dolist (key (keys def))
        (when (not (getprop obj key))
          (setf (getprop obj key)
                (getprop def key))))
      obj)))
