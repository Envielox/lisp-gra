(ql:quickload "cffi")

;(setf cffi:*foreign-library-directories* '( "~/dlls" ) )

(cffi:load-foreign-library "SDL2.dll" :search-path "D:/lisp/dlls/" )
(cffi:load-foreign-library "libffi-6.dll" :search-path "D:/lisp/dlls/" )
(cffi:load-foreign-library "libglfw3.dll" :search-path "D:/lisp/dlls/" )

(ql:quickload :cl-glfw3)
