#!/usr/bin/env ol

,include "config.scm"

(import (lib glfw))
(glfwSetErrorCallback
   (GLFWerrorfun (error_code description)
      (print error_code ": " description)))

; init
(unless (glfwInit)
   (runtime-error "glfw error"))

(define width 640)
(define height 480)

; we need the opengl 2.0
(glfwWindowHint GLFW_CONTEXT_VERSION_MAJOR 2)
(glfwWindowHint GLFW_CONTEXT_VERSION_MINOR 1)
(define window (glfwCreateWindow width height "White Sphere" NULL NULL))

; some math
(import (scheme inexact)
   (math infix-notation)
   (only (srfi 27) random-real))

(define (normalize a)
   (define lv (/ #i1 (sqrt (vector-fold + 0 (vector-map * a a)))))
   (vector-map (lambda (x) (* x lv)) a))

(define (vec3 . xyz)
   (vm:make type-vector xyz))
(define (mat3 . mat)
   (vm:make type-vector mat))

(define (vec3-equal? a b) ; fast compare
   (vector-apply a (lambda (x0 y0 z0)
   (vector-apply b (lambda (x1 y1 z1)
      (and (= x0 x1) (= y0 y1) (= z0 z1))
   )))))

(define (rotation-between v0 v1) ; todo: speedup and cleanup
   (vector-apply v0 (lambda (x0 y0 z0)
   (vector-apply v1 (lambda (x1 y1 z1)
      (define s (sqrt (* #i2 (+ #i1.0 (* x0 x1) (* y0 y1) (* z0 z1)))))
      (if (> s #i0)
      then
         (define is (/ #i1 s))
         [
            (* is (- (* y0 z1) (* z0 y1)))
            (* is (- (* z0 x1) (* x0 z1)))
            (* is (- (* x0 y1) (* y0 x1)))
            (/ s 2)
         ]
      else (if (or
                  (vec3-equal? v1 [0 0 1]) ; Oz
                  (vec3-equal? v0 [0 0 1]))
         [1 0 0 0]
         [0 0 0 1])))))))

(define (normalize-quat quat)
   (vector-apply quat (lambda (x y z w)
      (define lv (/ #i1 (sqrt (+ (* x x) (* y y) (* z z) (* w w)))))
      [(* lv x) (* lv y) (* lv z) (* lv w)])))

(define (rotation-matrix3 quat)
   ; без нормализации кватерниона матрица будет не ортогональна
   (vector-apply (normalize-quat quat) (lambda (x y z w)
      (define dx (* x 2.))  (define dy (* y 2.))  (define dz (* z 2.))
      (define wx (* w dx))  (define wy (* w dy))  (define wz (* w dz))
      (define xx (* x dx))  (define xy (* x dy))  (define xz (* x dz))
      (define yy (* y dy))  (define yz (* y dz))
      (define zz (* z dz))
      (mat3
         (- #i1.0 yy zz) (- xy wz)       (+ xz wy)
         (+ xy wz)       (- #i1.0 xx zz) (- yz wx)
         (- xz wy)       (+ yz wx)       (- #i1.0 xx yy)
      ))))

(define (mat3*vec3 mat vec)
   (vector-apply vec (lambda (x y z)
      (vec3
         (+ (* (ref mat 1) x) (* (ref mat 4) y) (* (ref mat 7) z))
         (+ (* (ref mat 2) x) (* (ref mat 5) y) (* (ref mat 8) z))
         (+ (* (ref mat 3) x) (* (ref mat 6) y) (* (ref mat 9) z))
      ))))

; the OpenGL
(glfwMakeContextCurrent window)
(import (OpenGL 2.1)
   (lib GLU))

(define shader-program (gl:create-program
"  #version 120 // OpenGL 2.1
void main()
{
   gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
   gl_FrontColor = gl_Color;
   gl_TexCoord[0] = gl_MultiTexCoord0;
}"
"  #version 120 // OpenGL 2.1
uniform sampler2D tex0;
void main()
{
   vec3 color = texture2D(tex0, gl_TexCoord[0].st).rgb;
   float alpha = (color.r + color.g + color.b) / 3.0;
   gl_FragColor = vec4(gl_Color.rgb, gl_Color.a * alpha);
}"))
(define tex0 (glGetUniformLocation shader-program "tex0"))

; skybox preparation
(import (lib soil))
(define sky-atlas (let ((buffer (file->bytevector "media/sky-atlas.png")))
   (SOIL_load_OGL_texture_from_memory buffer (size buffer) SOIL_LOAD_RGB SOIL_CREATE_NEW_ID SOIL_FLAG_MIPMAPS)))

(define skybox (glGenLists 1))

(define R SKYBOX-RADIUS)
(define -R (negate R))
(define N (/ #i1 SKYBOX-ATLASSIZE))

; todo: move to external json config
(define stars-probabilities '(1 0.00002 0.0005 0.00005 0.05 0.0005 0.00005 0.00005 0.9 0.9 0.0005 1))
(define stars-probabilities-sum (fold + 0 stars-probabilities))
(define nebula-probabilities '(1 0.5 0.1 1))
(define nebula-probabilities-sum (fold + 0 nebula-probabilities))

(define (random-star-texture)
   (let cycle ((n 0)
               (id (* (random-real) stars-probabilities-sum))
               (stars stars-probabilities))
      (if (< id (car stars))
         (* n N)
      else
         (cycle (++ n) (- id (car stars)) (cdr stars)))))
(define (random-nebula-texture)
   (let cycle ((n (length stars-probabilities))
               (id (* (random-real) nebula-probabilities-sum))
               (nebulas nebula-probabilities))
      (if (< id (car nebulas))
         (* n N)
      else
         (cycle (++ n) (- id (car nebulas)) (cdr nebulas)))))

(define (random-xyz) (values
   (\\ random-real() * #i2 - 1)
   (\\ random-real() * #i2 - 1)
   (\\ random-real() * #i2 - 1)))

(define (random-rgb) (values
   (+ (* (random-real) 0.6) 0.4)
   (+ (* (random-real) 0.6) 0.4)
   (+ (* (random-real) 0.6) 0.4)))

(define PI #i3.1415927410125732421875) ; IEEE754 Pi Approximation

(define (generate! stars nebulas)
   (define up (vec3 0 1 0))

   (define (spawn count SCALE ALPHA texture)
      (for-each (lambda (i)
         (define-values (x y z) (random-xyz))
         (define-values (r g b) (random-rgb))
         (define a (* 2 PI (random-real))) ; rotation

         (define xyz (normalize (vec3 x y z)))
         (define rotation (rotation-between xyz [0 0 -1]))
         (define M (rotation-matrix3 rotation))

         (define scale (* SCALE (random-real)))
         (define texa (texture))
         (define texb (+ texa N))

         (define +sa (* scale (sin a))) (define -sa (negate +sa))
         (define +ca (* scale (cos a))) (define -ca (negate +ca))

         (glColor4f r g b ALPHA)

         (glTexCoord2d texa 0)
         (glVertex3dv (mat3*vec3 M [+ca +sa -R]))
         (glTexCoord2d texa 1)
         (glVertex3dv (mat3*vec3 M [-sa +ca -R]))
         (glTexCoord2d texb 1)
         (glVertex3dv (mat3*vec3 M [-ca -sa -R]))
         (glTexCoord2d texb 0)
         (glVertex3dv (mat3*vec3 M [+sa -ca -R])))
      (iota count)))

   (glNewList skybox GL_COMPILE)
   (glBegin GL_QUADS)
      ; nebulas (они размытые фоновые, поэтому рисуем первыми)
      (spawn nebulas (* NEBULAS-SCALE SKYBOX-RADIUS) NEBULAS-ALPHA random-nebula-texture)
      ; stars
      (spawn stars (* STARS-SCALE SKYBOX-RADIUS) STARS-ALPHA random-star-texture)
   (glEnd)
   (glEndList))

(generate! 1000 30)

; draw
(let loop ()
   (unless (glfwWindowShouldClose window)
      (define width (box 0))
      (define height (box 0))
      (glfwGetWindowSize window width height)
      (define aspect (/ (unbox width) (unbox height)))

      (glViewport 0 0 (unbox width) (unbox height))

      ; clear the color and depth buffers
      (glClearColor 0 0 0 1)
      (glClear (bor GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

      (glEnable GL_DEPTH_TEST)

      (glEnable GL_BLEND)
      (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)

      ; classical projection matrix
      (glMatrixMode GL_PROJECTION)
      (glLoadIdentity)
      (gluPerspective 45 aspect 0.1 1000)

      (glMatrixMode GL_MODELVIEW)
      (glLoadIdentity)
      (define r (\\ mod(time-ms(), 62830) / #i10000))
      (gluLookAt (* 3 (sin r)) 2 (* 3 (cos r))
         0 0 0
         0 1 0)


      (glUniform1i tex0 0)
      (glEnable GL_TEXTURE_2D)
      (glBindTexture GL_TEXTURE_2D sky-atlas)

      (glCallList skybox)

      (glfwSwapBuffers window)
      (glfwPollEvents) (sleep)
      (loop)))

; done
(glfwDestroyWindow window)
(glfwTerminate)
