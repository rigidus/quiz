(defparameter *the-ring* '('a 'b 'c 'd 'e 'f 'g))

(defparameter *bench* '(a a a a a a a))

(defmacro ring-inc (var)
  (let* ((clauses
          (loop for idx from 0 to (- (length *the-ring*) 1)
             collect `(,(nth idx *the-ring*)  ,(nth (+ 1 idx) *the-ring*)))))
    `(cond ,@(loop for mcase in clauses
                collect `((equal ,var ,(car mcase))  ,(let ((it (cadr mcase)))
                                                        (if it it `(values ,(car *the-ring*) t))))))))

;; (format nil "~A" (macroexpand-1 '(ring-inc elt)))
;; => (cond ((equal elt 'a) 'b)
;;          ((equal elt 'b) 'c)
;;          ((equal elt 'c) 'd)
;;          ((equal elt 'd) 'e)
;;          ((equal elt 'e) 'f)
;;          ((equal elt 'f) 'g)
;;          ((equal elt 'g) (values 'a t)))

(defun test-seq (param)
  (let ((known))
    (flet ((test-local (param)
             (if (find param known)
                 nil
                 (prog1 t (push param known)))))
      (loop for elt in param always (test-local elt)))))

(defun test-seq2 (param)
  (loop
     for idx from 0 to (- (length param) 1)
     for elt in param
     :thereis
       (let ((prev (unless (= idx 0)
                     (nth (- idx 1) param)))
             (next (unless (= idx (- (length param) 1))
                     (nth (+ idx 1) param))))
         (or (unless (null prev)
               (or (and (equal 'b elt)
                        (equal 'a prev))
                   (and (equal 'a elt)
                        (equal 'b prev))))
             (unless (null next)
               (or (and (equal 'a elt)
                        (equal 'b next))
                   (and (equal 'b elt)
                        (equal 'a next))))))))



(define-condition overflow-error (error)
  ((text :initarg :text :reader text)))

(defun process (pos)
  (let ((current *bench*)
        (acc))
      (labels ((incrementor (pos)
                 (when (<= (length current) pos)
                   (error 'overflow-error :text "OVERFLOW-ERROR in INCREMENTOR"))
                 (multiple-value-bind (result carry)
                     (ring-inc (nth pos current))
                   (setf (nth pos current) result)
                   (when carry
                     (incrementor (+ 1 pos))))
                 current))
        (handler-case (loop for var = current then (incrementor pos)
                         do (when (and (test-seq  current)
                                       (test-seq2 current))
                              (push (copy-list current) acc)))
          (overflow-error () 'fin))
        acc)))

;; test
(print (process 0))

;; time
(time (length (process 0)))

;; answer
(/ 1440 5040)
