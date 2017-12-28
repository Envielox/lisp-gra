(require :cl-opengl)
(require :cl-glfw3)

(ql:quickload :trivial-main-thread)

(ql:quickload :bordeaux-threads)

(use-package :trivial-main-thread)
(use-package :alexandria)
(use-package :cl-glfw3)

(defparameter *keys-pressed* nil)
(defparameter *buttons-pressed* nil)
(defparameter *window-size* nil)
(defparameter *mouse-pos* nil)

;; camera controls

(load "camera.lisp")
(defparameter *camera* (make-camera :pos '(0 0 -5) :rot '(0 0 0)))

(defparameter *middle-mouse-button-pressed* nil)

(defmacro mapcro (macro &rest args)
  `(progn
     ,@(apply #'mapcar
     (lambda (&rest args2)
       `(,macro ,@args2))
    args)
   ))


(defun update-window-title (window)
  (set-window-title (format nil "size ~A | keys ~A | buttons ~A | pos ~A | MMB ~A |	camera rot: ~A"
                            *window-size*
                            *keys-pressed*
                            *buttons-pressed*
                            *mouse-pos*
                            *middle-mouse-button-pressed*
                            (camera-rot *camera*))
                    window))

;(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))

;(declaim (ftype function move-object))
;(declaim (ftype function move-sun))
;(declaim (ftype function move-camera))
;(declaim (ftype function rotate-camera))

(defparameter *actions* (make-hash-table))

(def-key-callback key-callback (window key scancode action mod-keys)
  (declare (ignore scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (set-window-should-close))
  (if (eq action :press)
    (pushnew key *keys-pressed*)
    (deletef *keys-pressed* key))
  (when (or (eq action :press) (eq action :repeat))
    (if (gethash key *actions*)
      (eval (gethash key *actions*))
      nil))
  (update-window-title window))

;; Other files

(load "geometry.lisp")

(load "objects.lisp")

(defparameter *light-pos* '(0.0 0.0 0.1 1.0))
(defparameter *box* (prep-cube '(255 0 0) 1))
(defparameter *sphere* (prep-ico-sphere '(0 0 255) 1.2))
(defparameter *floor* (prep-floor '(0 255 0) 5 5))

(defparameter *obj-1* (make-object :mesh *sphere* :pos '(0 0 -2)))
(defparameter *sun* (make-object :mesh (prep-cube '(255 255 0) 0.1) :pos (butlast *light-pos*)))
(defparameter *ground* (make-object :mesh *floor* :pos '(0 0 -1)))

(defun move-sun (dir)
  (move-object *sun* dir)
  (setf *light-pos* (append (object-pos *sun*) (last *light-pos*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "render.lisp")
(setf *objects-to-draw* `(,*obj-1* ,*sun* ,*ground*))

; #################################################################

(setf (gethash :W *actions*) '(move-object *obj-1* '(0  1 0) ))
(setf (gethash :S *actions*) '(move-object *obj-1* '(0 -1 0) ))
(setf (gethash :A *actions*) '(move-object *obj-1* '(-1 0 0) ))
(setf (gethash :D *actions*) '(move-object *obj-1* '( 1 0 0) ))
(setf (gethash :E *actions*) '(move-object *obj-1* '(0 0 -1) ))
(setf (gethash :Q *actions*) '(move-object *obj-1* '( 0 0 1) ))

(setf (gethash :I *actions*) '(move-sun '(0  1 0) ))
(setf (gethash :K *actions*) '(move-sun '(0 -1 0) ))
(setf (gethash :J *actions*) '(move-sun '(-1 0 0) ))
(setf (gethash :L *actions*) '(move-sun '( 1 0 0) ))
(setf (gethash :O *actions*) '(move-sun '(0 0 -1) ))
(setf (gethash :U *actions*) '(move-sun '( 0 0 1) ))

(setf (gethash :UP *actions*) '(move-camera *camera* '(0  1 0) ))
(setf (gethash :DOWN *actions*) '(move-camera *camera* '(0 -1 0) ))
(setf (gethash :LEFT *actions*) '(move-camera *camera* '(-1 0 0) ))
(setf (gethash :RIGHT *actions*) '(move-camera *camera* '( 1 0 0) ))
(setf (gethash :DEL *actions*) '(move-camera *camera* '(0 0 -1) ))
(setf (gethash :END *actions*) '(move-camera *camera* '( 0 0 1) ))

(setf (gethash :KP-8 *actions*) '(rotate-camera *camera* '(0 -45 0) ))
(setf (gethash :KP-2 *actions*) '(rotate-camera *camera* '(0  45 0) ))
(setf (gethash :KP-4 *actions*) '(rotate-camera *camera* '( 45 0 0) ))
(setf (gethash :KP-6 *actions*) '(rotate-camera *camera* '(-45 0 0) ))

(def-mouse-button-callback mouse-callback (window button action mod-keys)
  (declare (ignore mod-keys))
  (if (eq action :press)
    (pushnew button *buttons-pressed*)
    (deletef *buttons-pressed* button))
  (when (eq button :3)
    (if (eq action :press)
      (progn
        (setf *middle-mouse-button-pressed* t)
        (set-input-mode :cursor :disabled)
        (setf *mouse-pos* (get-cursor-position)))
      (progn
        (setf *middle-mouse-button-pressed* nil)
        (set-input-mode :cursor :normal))))
  (update-window-title window))

(def-cursor-pos-callback cursor-callback (window xpos ypos)
  (when *middle-mouse-button-pressed*
    (let ((xdiff (- xpos (car *mouse-pos*)))
          (ydiff (- ypos (cadr *mouse-pos*))))
      (rotate-camera *camera* `(,xdiff ,ydiff 0))
      )
    )
  (setf *mouse-pos* `(,xpos ,ypos))
  (update-window-title window))

(def-window-size-callback window-size-callback (window w h)
  (setf *window-size* (list w h))
  (gl:viewport 0 0 w h)
  (update-window-title window))

; ##################################################################

(defun main ()
  ;; Graphics calls on OS X must occur in the main thread
  (with-body-in-main-thread ()
    (with-init-window (:title "" :width 800 :height 800)
      (setf *keys-pressed* nil)
      (set-key-callback 'key-callback)
      (set-mouse-button-callback 'mouse-callback)
      (set-window-size-callback 'window-size-callback)
      (set-cursor-position-callback 'cursor-callback)
      (setf *window-size* (get-window-size))
      (setup-gl)
      (update-window-title *window*)
      (loop until (window-should-close-p)
            do (progn
                 ;(setup-view)
                 (render)
                 (swap-buffers)
                 (poll-events)
                )))))

;; Game control stuff

(defvar *game-thread* nil)

(defun start ()
  (setf *game-thread* (bt:make-thread #'main)))

(defun stop ()
  (set-window-should-close))

