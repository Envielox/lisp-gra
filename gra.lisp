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
(declaim (ftype function move-sun))


(def-key-callback key-callback (window key scancode action mod-keys)
  (declare (ignore scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (set-window-should-close))
  (if (eq action :press)
    (pushnew key *keys-pressed*)
    (deletef *keys-pressed* key))
  (when (or (eq action :press) (eq action :repeat))
    (when (eq key :EQUAL) (incf *sel*))
    (when (eq key :MINUS) (decf *sel*))
    (when (eq key :W) (move-object *obj-1* '(0  1 0) ))
    (when (eq key :S) (move-object *obj-1* '(0 -1 0) ))
    (when (eq key :A) (move-object *obj-1* '(-1 0 0) ))
    (when (eq key :D) (move-object *obj-1* '(1  0 0) ))
    (when (eq key :E) (move-object *obj-1* '(0 0 -1) ))
    (when (eq key :Q) (move-object *obj-1* '(0 0  1) ))
    (when (eq key :I) (move-sun '(0  1 0) ))
    (when (eq key :K) (move-sun '(0 -1 0) ))
    (when (eq key :J) (move-sun '(-1 0 0) ))
    (when (eq key :L) (move-sun '(1  0 0) ))
    (when (eq key :O) (move-sun '(0 0 -1) ))
    (when (eq key :U) (move-sun '(0 0  1) )))
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

;; #########################################

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

;##################################################

(defstruct object mesh pos)

(defvar *speed* 0.11)
(defparameter *light-pos* '(0.0 0.0 0.1 1.0))

(defparameter *box* (prep-cube '(255 0 0) 1))
(defparameter *sphere* (prep-ico-sphere '(0 0 255) 1.2))
(defparameter *floor* (prep-floor '(0 255 0) 5 5))

(defparameter *obj-1* (make-object :mesh *sphere* :pos '(0 0 -2)))
(defparameter *sun* (make-object :mesh (prep-cube '(255 255 0) 0.1) :pos (butlast *light-pos*)))
(defparameter *ground* (make-object :mesh *floor* :pos '(0 0 -1)))

(defun move-object (object dir)
  (setf
    (object-pos object)
    (loop
      for pos in (object-pos object)
      for delta in dir
      collect (+ pos (* *speed* delta)))))

(defun move-sun (dir)
  (move-object *sun* dir)
  (setf *light-pos* (append (object-pos *sun*) (last *light-pos*))))

(defun draw-object (obj)
  (gl:matrix-mode :modelview)
  (gl:push-matrix)
  (gl:translate (car (object-pos obj)) (cadr (object-pos obj)) (caddr (object-pos obj)))
  (draw-mesh (object-mesh obj))
  (gl:pop-matrix))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun render ()
  (declare (optimize speed))

    (gl:light :light0 :position *light-pos*)
    (gl:clear :color-buffer :depth-buffer)
    (gl:enable-client-state :vertex-array)
    (gl:enable-client-state :color-array)
    (gl:enable-client-state :normal-array)
    (gl:matrix-mode :modelview)
    (gl:load-identity)
    (gl:translate 0 0 -5)
    (draw-object *obj-1*)
    (draw-object *sun*)
    (draw-object *ground*)
    (gl:flush))

(defun setup-view ()
  (gl:viewport 0 0 400 400)
  (gl:matrix-mode :projection)
  ;(gl:ortho -5 5 -5 5 -2 2)
  (gl:frustum -5 5 -5 5 5 20)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defun setup-lightning () 
  ;; Lightning stuff
  (gl:light :light0 :position *light-pos*)
  (gl:light :light0 :linear-attenuation 1.0)
  ;(gl:light :light0 :quadratic-attenuation 1.0)
  ;(gl:light :light0 :diffuse '(1.0 0.5 0.0 0.0))
  (gl:enable :lighting)
  (gl:enable :light0)
  )

(defun setup-gl ()
  (setup-view)
  (setup-lightning)
  (gl:shade-model :smooth)
  (gl:material :front-and-back :specular '(1.0 1.0 1.0 1.0))
  (gl:material :front-and-back :shininess 50.0)
  (gl:enable :depth-test)
  ;(gl:enable :color-material)
  (gl:color-material :front-and-back :diffuse)
  ;(gl:enable :cull-face)
  ;(gl:cull-face :back)
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
      (loop until (window-should-close-p)
            do (progn
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



