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

(defvar *sel* 3)

(defmacro mapcro (macro &rest args)
  `(progn
     ,@(apply #'mapcar
     (lambda (&rest args2)
       `(,macro ,@args2))
    args)
   ))

(defun update-window-title (window)
  (set-window-title (format nil "size ~A | keys ~A | buttons ~A | pos ~A"
                            *window-size*
                            *keys-pressed*
                            *buttons-pressed*
                            *mouse-pos*)
                    window))

(defun ignore-warnings (condition)
  (declare (ignore condition))
  (muffle-warning))

;(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))

(declaim (ftype function move-object))


(def-key-callback key-callback (window key scancode action mod-keys)
  (declare (ignore scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (set-window-should-close))
  (if (eq action :press)
    (pushnew key *keys-pressed*)
    (deletef *keys-pressed* key))
  (when (eq action :press)
    (when (eq key :EQUAL) (incf *sel*))
    (when (eq key :MINUS) (decf *sel*))
    (when (eq key :W) (move-object '(0  1) ))
    (when (eq key :S) (move-object '(0 -1) ))
    (when (eq key :A) (move-object '(-1 0) ))
    (when (eq key :D) (move-object '(1  0) )))
  (update-window-title window))

(def-mouse-button-callback mouse-callback (window button action mod-keys)
  (declare (ignore mod-keys))
  (if (eq action :press)
    (pushnew button *buttons-pressed*)
    (deletef *buttons-pressed* button))
  (update-window-title window))

(def-cursor-pos-callback cursor-callback (window xpos ypos )
  (setf *mouse-pos* `(,xpos ,ypos))
  (update-window-title window))

(def-window-size-callback window-size-callback (window w h)
  (setf *window-size* (list w h))
  (gl:viewport 0 0 w h)
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

(gl:define-gl-array-format position-color
  (gl:vertex :type :float :components (x y))
  (gl:color :type :unsigned-char :components (r g b)))

(defparameter *vertex-array* (gl:alloc-gl-array 'position-color 5))
(defparameter *indices-array* (gl:alloc-gl-array :unsigned-short 10))
(defun prep-figure ()
  (declare (optimize speed))
  (handler-bind ( (cl:warning #'ignore-warnings) )
  ;(gl:gen-vertex-array)
  (dotimes (i 5)
    (let ((phi (float (+ (/ pi 2) (* (/ i 5) (* 2 pi))) 0.0)))
      ;; vertices
      (setf (gl:glaref *vertex-array* i 'x) (sin phi))
      (setf (gl:glaref *vertex-array* i 'y) (cos phi))
      ;; indices
      (setf (gl:glaref *indices-array* (* 2 i)) i)
      (setf (gl:glaref *indices-array* (1+ (* 2 i))) (mod (+ i 2) 5))
      ;; colors
      (setf (gl:glaref *vertex-array* i 'r) 255)
      (setf (gl:glaref *vertex-array* i 'g) 0)
      (setf (gl:glaref *vertex-array* i 'b) 0)))
    )
  (gl:clear-color 0 0 0 0))

(defstruct figure vertex-array index-array)

(defun prep-shape (sides color radius)
  (declare (optimize speed))
  (handler-bind ( (cl:warning #'ignore-warnings) )
  (let ((ret (make-figure)))
  (setf (figure-vertex-array ret) (gl:alloc-gl-array 'position-color (1+ sides)))
  (setf (figure-index-array ret) (gl:alloc-gl-array :unsigned-short (* 3 sides)))

  (setf (gl:glaref (figure-vertex-array ret) 0 'x) 0.0)
  (setf (gl:glaref (figure-vertex-array ret) 0 'y) 0.0)
  (setf (gl:glaref (figure-vertex-array ret) 0 'r) (car color))
  (setf (gl:glaref (figure-vertex-array ret) 0 'g) (cadr color))
  (setf (gl:glaref (figure-vertex-array ret) 0 'b) (caddr color))
  (dotimes (i sides)
    (let ((phi (float (* 2 pi (/ i sides) ) 0.0 ) )
          (idx (1+ i)))

      (setf (gl:glaref (figure-vertex-array ret) idx 'x) (* radius (sin phi)))
      (setf (gl:glaref (figure-vertex-array ret) idx 'y) (* radius (cos phi)))
      ;;; colors
      (setf (gl:glaref (figure-vertex-array ret) idx 'r) (car color))
      (setf (gl:glaref (figure-vertex-array ret) idx 'g) (cadr color))
      (setf (gl:glaref (figure-vertex-array ret) idx 'b) (caddr color))
      ;; indices
      (setf (gl:glaref (figure-index-array ret) (* 3 i)) idx)
      (setf (gl:glaref (figure-index-array ret) (1+ (* 3 i))) (1+ (mod idx sides)))
      (setf (gl:glaref (figure-index-array ret) (+ 2 (* 3 i))) 0)
    ))
  ret)))

(defun prep-square (color)
  (declare (optimize speed))
  (handler-bind ( (cl:warning #'ignore-warnings) )
  (let ((ret (make-figure)))
  (setf (figure-vertex-array ret) (gl:alloc-gl-array 'position-color 4))
  (setf (figure-index-array ret) (gl:alloc-gl-array :unsigned-short 6))

  (loop for i in '(0 1 2 2 1 3)
        for idx from 0
    do (setf (gl:glaref (figure-index-array ret) idx) i))

  (loop for i in '((0 0) (0 1) (1 0) (1 1))
        for idx from 0
    do (progn
      (setf (gl:glaref (figure-vertex-array ret) idx 'x) (float (car i) 0.0))
      (setf (gl:glaref (figure-vertex-array ret) idx 'y) (float (cadr i) 0.0))
      (setf (gl:glaref (figure-vertex-array ret) idx 'r) (car color))
      (setf (gl:glaref (figure-vertex-array ret) idx 'g) (cadr color))
      (setf (gl:glaref (figure-vertex-array ret) idx 'b) (caddr color))
        ))
  ret)))


(defparameter r-triangle (prep-shape 3 '(255 0 0) 1))
(defparameter b-square (prep-shape 4 '(0 0 255) 1))
(defparameter y-hexagon (prep-shape 6 '(255 255 0) 1))


(defun draw-shape (shape)
  (handler-bind ( (cl:warning #'ignore-warnings) )
  (gl:bind-gl-vertex-array (figure-vertex-array shape))
  (gl:draw-elements :triangles (figure-index-array shape))
  ))

(defstruct object figure pos)

(defparameter *obj-1* (make-object :figure r-triangle :pos '(0 0)))
(defvar *speed* 0.11)

(defun move-object (dir)
  (incf (car  (object-pos *obj-1*)) (* *speed* (car dir)))
  (incf (cadr (object-pos *obj-1*)) (* *speed* (cadr dir)))
  )

(defun draw-object (obj)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:translate (car (object-pos obj)) (cadr (object-pos obj)) -2)
  (draw-shape (object-figure obj))
  (gl:load-identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gl:define-gl-array-format position-3-color
  (gl:vertex :type :float :components (x y z))
  (gl:color :type :unsigned-char :components (r g b))
  (gl:normal :type :float :components (nx ny nz)))

(defparameter *box-vertex-array* (gl:alloc-gl-array 'position-3-color 8))
(defparameter *box-indices-array* (gl:alloc-gl-array :unsigned-short 36))

(defun prep-box (color radius)
  (declare (optimize speed))
  (handler-bind ( (cl:warning #'ignore-warnings) )
  (setf *box-vertex-array* (gl:alloc-gl-array 'position-3-color 8))
  (setf *box-indices-array* (gl:alloc-gl-array :unsigned-short 36))
  (let ((corners '(
                    (1.0 1.0 1.0)       (1.0 1.0 -1.0)
                          (1.0 -1.0 1.0)     (1.0 -1.0 -1.0)

                      (-1.0 1.0 1.0)    (-1.0 1.0 -1.0)
                          (-1.0 -1.0 1.0)    (-1.0 -1.0 -1.0)
                  )))
    (loop
      for idx from 0
      for pos in corners
      do (progn
        (setf (gl:glaref *box-vertex-array* idx 'x) (* radius (car pos)))
        (setf (gl:glaref *box-vertex-array* idx 'y) (* radius (cadr pos)))
        (setf (gl:glaref *box-vertex-array* idx 'z) (* radius (caddr pos)))
        (setf (gl:glaref *box-vertex-array* idx 'r) (car color))
        (setf (gl:glaref *box-vertex-array* idx 'g) (cadr color))
        (setf (gl:glaref *box-vertex-array* idx 'b) (caddr color))
        (setf (gl:glaref *box-vertex-array* idx 'nx) (* radius (car pos)))
        (setf (gl:glaref *box-vertex-array* idx 'ny) (* radius (cadr pos)))
        (setf (gl:glaref *box-vertex-array* idx 'nz) (* radius (caddr pos)))
    )))

  (let ((indexes '( 0 1 2 1 2 3 ; up
                    4 5 6 5 6 7 ; down
                    0 2 4 2 4 6 ; front
                    1 3 5 3 5 7 ; back
                    0 1 4 1 4 5 ; left
                    2 3 6 3 6 7 ; right
                   )))
    (loop
      for idx from 0
      for value in indexes
      do (progn
        (setf (gl:glaref *box-indices-array* idx) value)
    )))))

(defun draw-box ()
  (handler-bind ( (cl:warning #'ignore-warnings) )
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:translate 2 2 -2)
  (gl:bind-gl-vertex-array *box-vertex-array*)
  (gl:draw-elements :triangles *box-indices-array*)
  (gl:load-identity)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter w-square (prep-square '(255 255 255)))
(defparameter bl-square (prep-square '(12 12 12)))
(defparameter w-piece (prep-shape 17 '(172 172 172) 0.45))
(defparameter bl-piece (prep-shape 17 '(56 56 56) 0.45))

(defparameter *white-pieces* ())
(defparameter *black-pieces* ())

(defun draw-board ()
  (loop for i from 0 to 7
    do (loop for j from 0 to 7
        do (let ((cur-square (make-object
                           :figure (if (evenp (+ i j)) bl-square w-square)
                           :pos `(,(+ -4 j) ,(+ -4 i) ) )))
          (draw-object cur-square))
        ))
  (loop for pos in *white-pieces*
    do (let ((cur-piece (make-object
                       :figure w-piece
                       :pos `(,(+ -4.5 (car pos)) ,(+ -4.5  (cadr pos)) ) )))
      (draw-object cur-piece))
    )
  (loop for pos in *black-pieces*
    do (let ((cur-piece (make-object
                       :figure bl-piece
                       :pos `(,(+ -4.5 (car pos)) ,(+ -4.5  (cadr pos)) ) )))
      (draw-object cur-piece))
    )
  )

(defun game-init ()
  (setf *white-pieces*
        (loop for y from 1 to 3 append
              (loop for x from 1 to 8 when (evenp (+ x y)) collect `(,x ,y))))
  (setf *black-pieces*
        (loop for y from 6 to 8 append
              (loop for x from 1 to 8 when (evenp (+ x y)) collect `(,x ,y))))
  )

(defun make-move (old-pos new-pos)
  (cond
    ((member old-pos *white-pieces* :test #'equal)
     (deletef *white-pieces* old-pos :test #'equal)
     (pushnew new-pos *white-pieces*)
     )
    ((member old-pos *black-pieces* :test #'equal)
     (deletef *black-pieces* old-pos :test #'equal)
     (pushnew new-pos *black-pieces*)
     )
    (T (format t "Illegal move" )))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *light-pos* '(0.0 0.0 0.1 0.0))

(defun render ()
  (declare (optimize speed))

    (gl:light :light0 :position *light-pos*)
    (gl:clear :color-buffer :depth-buffer)
    (gl:enable-client-state :vertex-array)
    (gl:enable-client-state :color-array)
    (gl:enable-client-state :normal-array)
    (gl:matrix-mode :modelview)
    (gl:load-identity)
    (gl:translate 0 0 -2)
    ;(draw-board)
    ;(draw-object *obj-1*)
    (draw-box)
    ;(gl:matrix-mode :modelview)
    ;(gl:load-identity)
    ;(gl:translate 0 0 -2)
    ;(case *sel*
          ;(0
           ;(draw-shape r-triangle);
          ;)
          ;(1
           ;(draw-shape b-square);
          ;)
          ;(2
           ;(draw-shape y-hexagon);
          ;)
          ;(otherwise
            ;(handler-bind ( (cl:warning #'ignore-warnings) )
              ;(gl:bind-gl-vertex-array *vertex-array*)
              ;(gl:draw-elements :lines *indices-array*)
              ;)
           ;)
          ;)
    (gl:flush))

  ;; Draw a demo triangle
  ;(gl:begin :triangles)
  ;(gl:color 1.0 0.0 0.0)
  ;(gl:vertex 0.0 1.0)
  ;(gl:vertex -1.0 -1.0)
  ;(gl:vertex 1.0 -1.0)
  ;(gl:color 0.0 1.0 0.0)
  ;(gl:vertex 0.0 0.5)
  ;(gl:vertex -0.5 -0.5)
  ;(gl:vertex 0.5 -0.5)
  ;(gl:color 0.0 0.0 1.0)
  ;(dotimes (n 5)
    ;(dolist (item (tri (* n (/ (* PI 2) 5))))
      ;(apply #'gl:vertex item)))
  ;(gl:end)
  ;(gl:flush))
;; end of drawing stuff

(defun setup-gl ()
  (gl:viewport 0 0 400 400)
  (gl:matrix-mode :projection)
  ;(gl:ortho -5 5 -5 5 -2 2)
  (gl:frustum -5 5 -5 5 1 20)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  ;; Lightning stuff
  (gl:shade-model :smooth)  
  (gl:material :front-and-back :specular '(1.0 1.0 1.0 1.0))
  (gl:material :front-and-back :shininess 50.0)
  (gl:light :light0 :position *light-pos*)
  (gl:light :light0 :linear-attenuation 1.0)
  (gl:light :light0 :quadratic-attenuation 1.0)
  ;(gl:light :light0 :diffuse '(1.0 0.5 0.0 0.0))
  (gl:enable :lighting)
  (gl:enable :light0)
  (gl:enable :depth-test)
  (gl:enable :color-material)
  (gl:color-material :front-and-back :diffuse)
  ;; Clear to black
  (gl:clear-color 0.0 0.0 0.0 1.0))


(defun main ()
  ;; Graphics calls on OS X must occur in the main thread
  (with-body-in-main-thread ()
    (with-init-window (:title "" :width 400 :height 400)
      (set-key-callback 'key-callback)
      (set-mouse-button-callback 'mouse-callback)
      (set-window-size-callback 'window-size-callback)
      (set-cursor-position-callback 'cursor-callback)
      (setf *window-size* (get-window-size))
      (setup-gl)
      (update-window-title *window*)
      (prep-figure)
      (prep-box '(128 128 92) 1.0)
      (game-init)
      (loop until (window-should-close-p) do (progn (render) (swap-buffers) (poll-events) )))))

;; Game control stuff

(defvar *game-thread* nil)

(defun start ()
  (setf *game-thread* (bt:make-thread #'main)))

(defun stop ()
  (set-window-should-close))



