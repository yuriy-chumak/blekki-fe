#!/usr/bin/env ol

(import (lib gl-2))
(import (lib gl-2 vr))
(gl:enable-vr 'Oculus)

(import (lib soil))

(import (math infix-notation))
,load "model.lisp"
,load "skybox.lisp"
(skybox:generate! 8887 129)

(define Carrier-T (read-model-file "Carrier-T"))
(print Carrier-T)

(define program (gl:create-program
"#version 120 // OpenGL 2.1

varying vec2 st;
void main()
{
   gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
   gl_FrontColor = gl_Color;
   st = gl_MultiTexCoord0.st;
}"

"#version 120 // OpenGL 2.1
uniform sampler2D tex0;

varying vec2 st;
void main()
{
   gl_FragColor = texture2D(tex0, st);
}"))

; init
(glEnable GL_BLEND)
(glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
(glEnable GL_DEPTH_TEST)

; draw
(import (lib GLU))
(define old (time-ms))
(gl:set-renderer (lambda ()
   (glViewport 0 0 (gl:get-window-width) (gl:get-window-height))
   (glClearColor 0 0 0 1)
   (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   (define FOVY 45.0)
   (define ASPECT (/ (gl:get-window-width) (gl:get-window-height)))

   ; --------------------------
   ; projection matrix
   (glMatrixMode GL_PROJECTION)
   ;non-vr projection matrix (todo: use own math)
   (define projection-matrix [
      #i1 #i0 #i0 #i0
      #i0 #i1 #i0 #i0
      #i0 #i0 #i1 #i0
      #i0 #i0 #i0 #i1
   ])

   (glLoadIdentity)
   (gluPerspective FOVY ASPECT 0.1 1000)
   (glGetFloatv GL_PROJECTION_MATRIX projection-matrix)
   ; try to get vr matrix if we use vr mode
   (glGetFloatv VR_PROJECTION_MATRIX projection-matrix)
   ; and set the final matrix
   (glLoadMatrixf projection-matrix)

   ; --------------------------
   ; projection matrix
   (glMatrixMode GL_MODELVIEW)
   ;non-vr modelview matrix
   (define modelview-matrix [
      #i1 #i0 #i0 #i0
      #i0 #i1 #i0 #i0
      #i0 #i0 #i1 #i0
      #i0 #i0 #i0 #i1
   ])
   (glLoadIdentity)
   (gluLookAt 0 0 5  ; eye
              0 0 0  ; center
              0 1 0) ; up
   (glGetFloatv GL_MODELVIEW_MATRIX modelview-matrix)
   ; try to get vr matrix if we use vr mode
   (glGetFloatv VR_MODELVIEW_MATRIX modelview-matrix)
   ; and set the final matrix
   (glLoadMatrixf modelview-matrix)

   ; skybox
   (when #true
      ; так как мы "всегда в нуле", то и скайбокс не нужно никуда двигать
      ; и даже вращать не надо
      (glDisable GL_DEPTH_TEST)
      (skybox:draw)
      (glEnable GL_DEPTH_TEST))


   ;; draw the spaceship:
   (define t (/ (mod (- (time-ms) old) 30000) #i100))
   (glTranslatef 0 -60 (- (negate t) -100))
   (glScalef 1/3 1/3 1/3)
   
   ;(glRotatef (* t 360/3.14) 0 1 0)

   (glEnable GL_TEXTURE_2D)

   (glUseProgram program)
   (glUniform1i (glGetUniformLocation program "tex0") 0)
   (glActiveTexture GL_TEXTURE0)
   ;(draw-geometry Carrier-T)
   (draw-textured Carrier-T)
))
