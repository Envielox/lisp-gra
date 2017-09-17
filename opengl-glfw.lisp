(require :cl-opengl)
(require :cl-glfw3)

(ql:quickload :trivial-main-thread)

(use-package :trivial-main-thread)
(use-package :alexandria)
(use-package :cl-glfw3)


(defparameter *keys-pressed* nil)
(defparameter *buttons-pressed* nil)
(defparameter *window-size* nil)

(defun update-window-title (window)
  (set-window-title (format nil "size ~A | keys ~A | buttons ~A"
                            *window-size*
                            *keys-pressed*
                            *buttons-pressed*)
                    window))

(def-key-callback key-callback (window key scancode action mod-keys)
  (declare (ignore scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (set-window-should-close))
  (if (eq action :press)
    (pushnew key *keys-pressed*)
    (deletef *keys-pressed* key))
  (update-window-title window))

(def-mouse-button-callback mouse-callback (window button action mod-keys)
  (declare (ignore mod-keys))
  (if (eq action :press)
    (pushnew button *buttons-pressed*)
    (deletef *buttons-pressed* button))
  (update-window-title window))

(def-window-size-callback window-size-callback (window w h)
  (setf *window-size* (list w h))
  (update-window-title window))

;; star drawing stuff
(defun point-angle (x y angle)
  (list
    (+ (* y (sin angle)) (* x (sin (+ (/ PI 2) angle))))
    (+ (* y (cos angle)) (* x (cos (+ (/ PI 2) angle))))
    ))

(defvar *star-a* 0.5)
(defvar *star-b* 1.0)
(defvar *star-c* 0.4)


(setf *star-a* (* *star-c* (/ (sqrt (+ 25 (* 10 (sqrt 5) ) ) ) 10) ))


(defun tri (angle)
  (list 
    (point-angle 0 *star-b* angle)
    (point-angle (/ *star-c* -2) *star-a* angle)
    (point-angle (/ *star-c*  2) *star-a* angle)
    ))

(defun render ()
  (gl:clear :color-buffer)
  ;; Draw a demo triangle
  (gl:begin :triangles)
  (gl:color 1.0 0.0 0.0)
  (gl:vertex 0.0 1.0)
  (gl:vertex -1.0 -1.0)
  (gl:vertex 1.0 -1.0)
  (gl:color 0.0 1.0 0.0)
  (gl:vertex 0.0 0.5)
  (gl:vertex -0.5 -0.5)
  (gl:vertex 0.5 -0.5)
  (gl:color 0.0 0.0 1.0)
  (dotimes (n 5)
    (dolist (item (tri (* n (/ (* PI 2) 5))))
      (apply #'gl:vertex item)))
  (gl:end)
  (gl:flush))
;; end of drawing stuff

(defun setup-gl ()
  (gl:viewport 0 0 400 400)
  (gl:matrix-mode :projection)
  (gl:ortho -2 2 -2 2 -2 2)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  ;; Clear to black
  (gl:clear-color 0.0 0.0 0.0 1.0))

(defun main ()
  ;; Graphics calls on OS X must occur in the main thread
  (with-body-in-main-thread ()
    (with-init-window (:title "" :width 400 :height 400)
      (set-key-callback 'key-callback)
      (set-mouse-button-callback 'mouse-callback)
      (set-window-size-callback 'window-size-callback)
      (setf *window-size* (get-window-size))
      (setup-gl)
      (update-window-title *window*)
      (loop until (window-should-close-p) do (progn (render) (swap-buffers) (poll-events) )))))
