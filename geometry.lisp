(defun ignore-warnings (condition)
  (declare (ignore condition))
  (muffle-warning))

(gl:define-gl-array-format position-3-color
  (gl:vertex :type :float :components (x y z))
  (gl:color :type :unsigned-char :components (r g b))
  (gl:normal :type :float :components (nx ny nz)))

(defun set-format-element (vertex-array index position color normal)
  (setf (gl:glaref vertex-array index 'x) (car position))
  (setf (gl:glaref vertex-array index 'y) (cadr position))
  (setf (gl:glaref vertex-array index 'z) (caddr position))
  (setf (gl:glaref vertex-array index 'r) (car color))
  (setf (gl:glaref vertex-array index 'g) (cadr color))
  (setf (gl:glaref vertex-array index 'b) (caddr color))
  (setf (gl:glaref vertex-array index 'nx) (car normal))
  (setf (gl:glaref vertex-array index 'ny) (cadr normal))
  (setf (gl:glaref vertex-array index 'nz) (caddr normal)))

(defstruct mesh vertex-array index-array)

(defun prep-mesh (vertexes normals indexes color)
  (declare (optimize speed))
  (handler-bind ( (cl:warning #'ignore-warnings) )
  (let ((ret (make-mesh)))
    (setf (mesh-vertex-array ret) (gl:alloc-gl-array 'position-3-color (length vertexes)))
    (setf (mesh-index-array ret) (gl:alloc-gl-array :unsigned-short (length indexes)))
    (loop
        for idx from 0
        for pos in vertexes
        for nor in normals
        do (progn
          (set-format-element (mesh-vertex-array ret) idx pos color nor)
      ))
    (loop
      for idx from 0
      for value in indexes
      do (progn
        (setf (gl:glaref (mesh-index-array ret) idx) value)
    ))
    ret)))

(defun draw-mesh (mesh)
  (handler-bind ( (cl:warning #'ignore-warnings) )
  (gl:bind-gl-vertex-array (mesh-vertex-array mesh))
  (gl:draw-elements :triangles (mesh-index-array mesh))
  ))

(defun resize-vertexes (vertex-list scale)
  (loop
    for triplet in vertex-list
    collect
      (loop for elem in triplet
            collect (* scale elem))))

(defun prep-cube (color radius)
  (let (
    (corners '(
      (1.0 1.0 1.0)       (1.0 1.0 -1.0)
            (1.0 -1.0 1.0)     (1.0 -1.0 -1.0)

        (-1.0 1.0 1.0)    (-1.0 1.0 -1.0)
            (-1.0 -1.0 1.0)    (-1.0 -1.0 -1.0)
    ))
    (indices '( 0 1 2 1 2 3 ; up
                   4 5 6 5 6 7 ; down
                   0 2 4 2 4 6 ; front
                   1 3 5 3 5 7 ; back
                   0 1 4 1 4 5 ; left
                   2 3 6 3 6 7 ; right
                 )))
  (prep-mesh (resize-vertexes corners radius) corners indices color)))

(defun prep-ico-sphere (color radius)
  (let* ((X 0.525731112119133606)
         (Z 0.850650808352039932)
         (N 0.0)
        (vertexes `(
            (,(- X) ,N ,Z)  (,X ,N ,Z)  (,(- X) ,N ,(- Z))  (,X ,N ,(- Z))
            (,N ,Z ,X)  (,N ,Z ,(- X))  (,N ,(- Z) ,X)  (,N ,(- Z) ,(- X))
            (,Z ,X ,N)  (,(- Z) ,X  ,N)  (,Z ,(- X) ,N)  (,(- Z) ,(- X)  ,N)
                   ))
        (indices '(
           0 4 1 0 9 4 9 5 4 4 5 8 4 8 1
            8 10 1 8 3 10 5 3 8 5 2 3 2 7 3
            7 10 3 7 6 10 7 11 6 11 0 6 0 1 6
            6 1 10 9 0 11 9 11 2 9 2 5 7 2 11
                  ))
        )
    (prep-mesh (resize-vertexes vertexes radius) vertexes indices color)))

(defun prep-floor (color size-x size-y)
  (declare (optimize (debug 3)))
  (let* ((vertices (loop
                     for y from 0 to size-y
                     append (loop for x from 0 to size-x
                                  collect `(,(float x) ,(float y) 0.0))))
         (indices (loop
                    for y from 0 below size-y
                    append (loop for x from 0 below size-x
                                 append (let ((cur-idx (+ x (* y (+ 1 size-x))))
                                              (row_size (+ 1 size-x)))
                                          `(,cur-idx ,(+ 1 cur-idx) ,(+ cur-idx row_size)
                                                     ,(+ 1 cur-idx) ,(+ 1 row_size cur-idx) ,(+ cur-idx row_size))))))
         (normals (loop
                    for i in vertices
                    collect '(0.0 0.0 1.0))))
    ;`(,vertices ,indices)))
    (prep-mesh vertices normals indices color)))

