(defvar *speed* 0.11)
(defstruct object mesh pos)

(defun move-object (object dir)
  (setf
    (object-pos object)
    (loop
      for pos in (object-pos object)
      for delta in dir
      collect (+ pos (* *speed* delta)))))

(defun draw-object (obj)
  (gl:matrix-mode :modelview)
  (gl:push-matrix)
  (gl:translate (car (object-pos obj)) (cadr (object-pos obj)) (caddr (object-pos obj)))
  (draw-mesh (object-mesh obj))
  (gl:pop-matrix))
