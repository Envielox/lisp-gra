(defvar *camera-speed* 0.11)
(defvar *camera-rot-speed* 0.08)
(defstruct camera pos rot)

(defun move-camera (cam dir)
  (setf
    (camera-pos cam)
    (loop
      for pos in (camera-pos cam)
      for rot in (camera-rot cam)
      for delta in dir
      collect (+ pos (* *camera-speed* delta rot)))))

(defun rotate-camera (cam dir)
  (setf
    (camera-rot cam)
    (loop
      for rot in (camera-rot cam)
      for delta in dir
      collect (+ rot (* *camera-rot-speed* delta)))))

; current camera
(defun update-camera-pos (cam)
  (gl:load-identity)
  (let ((pitch (car (camera-rot cam)))
        (yaw (cadr (camera-rot cam)))
        (roll (caddr (camera-rot cam))))
    (gl:rotate yaw   1.0 0.0 0.0)
    (gl:rotate pitch 0.0 1.0 0.0)
    (gl:rotate roll  0.0 0.0 1.0))
  (let ((x (car (camera-pos cam)))
        (y (cadr (camera-pos cam)))
        (z (caddr (camera-pos cam))))
    (gl:translate x y z)))

