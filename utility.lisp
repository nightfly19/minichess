(in-package :elo100)

(defmacro memoize (fn-name hash-fn)
  (let ((old-function (gensym "old-function"))
        (new-function (gensym "new-function"))
        (state-arg (gensym "state-arg"))
        (value-cache (gensym "value-cache"))
        (cache-size (gensym "cache-size"))
        (not-found (gensym "not-found"))
        (hash (gensym "hash"))
        (cached-value (gensym "cached-value"))
        (new-value (gensym "new-value")))
    `(let* ((,old-function (fdefinition ',fn-name))
            (,cache-size *default-cache-size*)
            (,value-cache (make-array ,cache-size :initial-element ',not-found))
            (,new-function (lambda (,state-arg)
                             (let* ((,hash (mod (funcall ,hash-fn ,state-arg) ,cache-size))
                                    (,cached-value (aref ,value-cache ,hash)))
                               (if (eql ',not-found ,cached-value)
                                   (let ((,new-value (funcall ,old-function ,state-arg)))
                                     (setf (aref ,value-cache ,hash) ,new-value)
                                     ,new-value)
                                   ,cached-value)))))
       (setf (fdefinition ',fn-name) ,new-function))))

(defmacro lazy-memoization (fn-name)
  (let ((old-function (gensym "old-function"))
        (new-function (gensym "new-function"))
        (state-arg (gensym "state-arg"))
        (value-cache (gensym "value-cache"))
        (new-value (gensym "new-value")))
    `(let* ((,old-function (fdefinition ',fn-name))
            (,value-cache (make-hash-table :test 'equal))
            (,new-function (lambda (&rest ,state-arg)
                             (if (not (nth-value 1 (gethash (sxhash ,state-arg) ,value-cache)))
                                 (let ((,new-value (apply ,old-function ,state-arg)))
                                   (setf (gethash (sxhash ,state-arg) ,value-cache) ,new-value)
                                   ,new-value)
                                 (gethash (sxhash ,state-arg) ,value-cache)))))
       (setf (fdefinition ',fn-name) ,new-function))))
