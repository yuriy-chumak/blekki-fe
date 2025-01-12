#!/usr/bin/env ol

(import (lib gl-2)
   (scheme inexact))
(import (lib soil))

(import (olite math))
(import (scheme dynamic-bindings))

; config
(define RL_CULL_DISTANCE_NEAR 0.1) ; как в VR
(define RL_CULL_DISTANCE_FAR 10000000000)


; init
(define sun_dir [#i0 #i0.176 #i1])
(define sun_distance #i905520)
(define sun_radius #i84449.095459)

(define sunPos (vec3scale sun_dir sun_distance))

(import (prefix (sun) sun:))

(define time (make-parameter (time-ms))) ; время последнего рендеринга

; --------------------------------------------------------------
; pre-renderer
(gl:set-prerenderer (lambda ()
   ; билборд солнечной короны
   (sun:prerender)
   #true
))

; --------------------------------------------------------------
(import (lib GLU))
(gl:set-renderer (lambda (mouse)
   (glViewport 0 0 (gl:get-window-width) (gl:get-window-height))
   (glClearColor 0 0 0 1)
   (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   ; матрицы вида/проецирования
   (define FOVY 45.0) ; TODO: взять из настроек текущего корабля
   (define ASPECT (/ (gl:get-window-width) (gl:get-window-height)))

   ; перспективная матрица (в VR режиме игнорируется)
   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (gluPerspective FOVY ASPECT RL_CULL_DISTANCE_NEAR RL_CULL_DISTANCE_FAR)

   ; матрица вида
   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)

   ; добавим повороты головы мышкой (в очках мышки нет, если что)
   (when mouse
      (define dx (- (/ (inexact (car mouse)) (gl:get-window-width)) 0.5))
      (define dy (- (/ (inexact (cdr mouse)) (gl:get-window-height)) 0.5))
      (glRotated (* dx 30) 0 1 0)
      (glRotated (* dy 30) 1 0 0))

   ; от чьего имени смотрим
   (define matrix [
      #i1 #i0 #i0 #i0
      #i0 #i1 #i0 #i0
      #i0 #i0 #i1 #i0
      #i0 #i0 #i0 #i1
   ])
   (define position (vector-copy matrix 12 15))

   (apply gluLookAt (append
      (vector->list [0 0 0])
      (vector->list (* matrix [0 0 1]))
      (vector->list (* matrix [0 1 0]))
   ))

   ; все операции с миром мы проводим в модельном пространстве,
   ;  в качестве которого мы используем текстурную матрицу "0".
   (glMatrixMode GL_TEXTURE) ; модельная матрица (матрицу вида больше не трогаем!)
   (glActiveTexture GL_TEXTURE0) ; active texture держим по дефолту: 0

   (glEnable GL_BLEND) ; полупрозрачность
   (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
   (glEnable GL_DEPTH_TEST)

   ; ------ рендерим обычный мир и его объекты ---------------------
   ; Cолнце
   ; оно всегда "где-то там даааааалеко"
   (define degree (/ (mod (time-ms) (* 2 314280)) 1000)) ; 0..2PI degrees (для вращения планет и солнца)

   (sun:draw sun_radius sunPos degree)
   (sun:draw-corona sun_radius sunPos)

))
