#!/usr/bin/env ol
(syscall 1014 (c-string "__NV_PRIME_RENDER_OFFLOAD") (c-string "1") #true)
(syscall 1014 (c-string "__GLX_VENDOR_LIBRARY_NAME") (c-string "nvidia") #true)

; используем зашитые в модель оси координат:
;  ось Ox - налево, Oy - вверх, Oz - вперед

(import (lib gl-2))

(import (lib soil))
(import (math infix-notation))
(import (olite math))
,load "model.lisp"

(define Carrier-T (read-model-file "Raiders Ion Array Frigate"))

; object math:
; матрица поворота кораблика в пространстве
; column-major! потому что для OpenGL
(define rotation (identity-matrix))

; предвычисленные вектора поворота
(define Lh [1 0 0])   ; вектор "левая рука" (Ox)
(define Up [0 1 0])   ; вектор "вверх" (Oz)
(define Fw [0 0 1])   ; вектор "вперед" (Oz)
; - yaw (рыскание)-------------------------------
; найдем 1/20 вектора от "левая" до "вперед" (и нормализуем его)
(define ds (\\ normalize(
   Lh + (Fw - Lh) / 20
)))

; найдем кватернион поворота от Lh к Lh
(define q (rotation-between Lh ds))
(define yaw (rotation-matrix q))

(define -q (conjugate q))
(define -yaw (rotation-matrix -q))
; - roll (крен) ---------------------------------
(define ds (\\ normalize(
   Lh + (Up - Lh) / 20
)))
(define q (rotation-between Lh ds))
(define roll (rotation-matrix q))
(define -q (conjugate q))
(define -roll (rotation-matrix -q))
; - pitch (тангаж) ------------------------------
(define ds (\\ normalize(
   Up + (Fw - Up) / 20
)))
(define q (rotation-between Up ds))
(define pitch (rotation-matrix q))
(define -q (conjugate q))
(define -pitch (rotation-matrix -q))
; -

; повороты келавиатурой
(import (lib keyboard))
(define axisq (box #true))
(gl:set-keyboard-handler (lambda (key)
   (case key
      (KEY_ESC
         (set-car! axisq (not (car axisq))))
      (KEY_RIGHT
         (mat4-set! rotation (mat4*mat4 rotation yaw)))
      (KEY_LEFT
         (mat4-set! rotation (mat4*mat4 rotation -yaw)))
      (KEY_UP
         (mat4-set! rotation (mat4*mat4 rotation pitch)))
      (KEY_DOWN
         (mat4-set! rotation (mat4*mat4 rotation -pitch)))
      (KEY_E
         (mat4-set! rotation (mat4*mat4 rotation roll)))
      (KEY_Q
         (mat4-set! rotation (mat4*mat4 rotation -roll)))
   )))

; opengl init
(glEnable GL_DEPTH_TEST)

; sun
(define sun [10 10 -10 0])
(define shader-program (gl:create-program
"  #version 120 // OpenGL 2.1

varying vec4 vertexPosition;
varying vec3 vertexNormal;

void main()
{
   vec4 vertex = gl_Vertex;
   vec3 normal = gl_Normal;

   vertexPosition = gl_ModelViewMatrix * vertex;
   vertexNormal = mat3(gl_ModelViewMatrix) * normal;

   gl_Position = gl_ProjectionMatrix * vertexPosition;
   gl_TexCoord[0] = gl_MultiTexCoord0;
}"

"  #version 120 // OpenGL 2.1
#define PI 3.1415927410125732421875 // IEEE754 Pi Approximation

uniform sampler2D tex0;

varying vec4 vertexPosition;
varying vec3 vertexNormal;

void main()
{
   vec3 vertex = vertexPosition.xyz;
   vec3 normal = normalize(vertexNormal);

	vec3 eyeDirection = normalize(-vertex); // in the modelview space eye direction is just inverse of position

   vec4 diffuseTex = texture2D(tex0, gl_TexCoord[0].st);

   vec4 lightPosition = gl_LightSource[0].position;
   vec3 lightDirection = lightPosition.xyz - vertex * lightPosition.w;

   vec3 unitLightDirection = normalize(lightDirection);

   float diffuseIntensity = dot(normal, unitLightDirection);

   if (diffuseIntensity < 0.2)
      diffuseIntensity = 0.2;

   gl_FragColor = vec4(diffuseTex.rgb * diffuseIntensity, 1.0);
}"))
(setq tex0 (glGetUniformLocation shader-program "tex0"))


; opengl draw
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
   (glLoadIdentity)
   (gluPerspective FOVY ASPECT 0.1 1000)

   ; --------------------------
   ; modelview matrix
   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (gluLookAt 1 2 1.5  ; eye
              0 0 0  ; center
              0 1 0) ; up

   (glUseProgram 0)

   ; глобальная система координат
   (when (unbox axisq)
      (glDisable GL_TEXTURE_2D)
      (glLineWidth 1)
      (glBegin GL_LINES)
         (glColor3f 1 0 0) ; Ox
         (glVertex3fv [-0.5 0 0])
         (glVertex3fv [1000 0 0])
         (glColor3f 0 1 0) ; Oy
         (glVertex3fv [0 -0.5 0])
         (glVertex3fv [0 1000 0])
         (glColor3f 0 0 1) ; Oz
         (glVertex3fv [0 0 -0.5])
         (glVertex3fv [0 0 1000])
      (glEnd))
   
   ; draw spaceship
   (glUseProgram shader-program)
   (glColor3f 1 1 1)
   (glEnable GL_TEXTURE_2D)
   (glEnable GL_LIGHT0)
   (glLightfv GL_LIGHT0 GL_POSITION sun)

   (glActiveTexture GL_TEXTURE0)
   (glUniform1i tex0 0)

   ; no more glRotate, just matrix multiplication
   (glMultMatrixf rotation)

   (glPushMatrix)
   (glScalef 1/500 1/500 1/500) ; уменьшим оригинальную модельку
   (glColor3f 1 1 1)
   (render-model Carrier-T)
   ;(draw-textured Carrier-T)
   (glPopMatrix)

   ; draw model axis for debug
   (glUseProgram 0)
   (when (unbox axisq)
      (draw-model-axis))
))

(print "Ok.")
