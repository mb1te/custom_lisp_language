(defmacro generate-list (size)
    `(cond
        ((= ,size 0) nil)
        (t (cons 0 (generate-list (- ,size 1))))
    )
)

(defmacro generate-matrix (rows cols)
    `(cond
        ((= ,rows 0) nil)
        (t (cons (generate-list ,cols) (generate-matrix (- ,rows 1) ,cols)))
    )
)

(defmacro revert-cell-array (pos arr)
    `(cond 
        ((= ,pos 0) (cons (mod (+ (car ,arr) 1) 2) (cdr ,arr)))
        (t (cons (car ,arr) (revert-cell-array (- ,pos 1) (cdr ,arr))))
    )
)

(defmacro revert-cell-aux (cur-map x y)
    `(cond
        ((= ,x 0) (cons (revert-cell-array ,y (car ,cur-map)) (cdr ,cur-map)))
        (t (cons (car ,cur-map) (revert-cell-aux (cdr ,cur-map) (- ,x 1) ,y)))
    )
)

(defmacro revert ()
    `(setq matrix (revert-cell-aux matrix posx posy))
)

(defmacro left ()
    `(setq posy (cond 
        ((= posy 0) (- cols 1))
        (t (- posy 1))
    ))
)

(defmacro right ()
    `(setq posy (cond
        ((= posy (- cols 1)) 0)
        (t (+ posy 1))
    ))
)

(defmacro up ()
    `(setq posx (cond
        ((= posx 0) (- rows 1))
        (t (- posx 1))
    ))
)

(defmacro down ()
    `(setq posx (cond
        ((= posx (- rows 1)) 0)
        (t (+ posx 1))
    ))
)

(defmacro print-matrix ()
    `(progn (mapcar (lambda (row) (print row)) matrix) (fresh-line))
)

(setq
    rows 2
    cols 12 
    posx 0 
    posy 0 
    matrix nil
    matrix (generate-matrix rows cols)
)

(defun process (stream)
    (let ((command (read-line stream nil)))
        (cond 
            ((null command) (exit))
            (t (progn
                (eval (read-from-string (format nil "(~A)" command)))
                (process stream)
            )))
    )
)

(with-open-file (stream (car *args*))
    (process stream)
)