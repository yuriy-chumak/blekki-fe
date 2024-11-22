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
(glfwWindowHint GLFW_CONTEXT_VERSION_MINOR 0)
(define window (glfwCreateWindow width height "White Sphere" NULL NULL))

; some math
(import (scheme inexact)
   (math infix-notation))

(define (normalize a)
   (define lv (/ #i1 (sqrt (vector-fold + 0 (vector-map * a a)))))
   (vector-map (lambda (x) (* x lv)) a))

; the OpenGL
(glfwMakeContextCurrent window)
(import (OpenGL 1.1)
   (lib GLU))

; draw
(let loop ()
   (unless (glfwWindowShouldClose window)
      (define width (box 0))
      (define height (box 0))
      (glfwGetWindowSize window width height)
      (define aspect (/ (unbox width) (unbox height)))

      (glViewport 0 0 (unbox width) (unbox height))

      ; clear the color and depth buffers
      (glClearColor 0.2 0.2 0.2 1)
      (glClear (bor GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

      (glEnable GL_DEPTH_TEST)

      ; classical projection matrix
      (glMatrixMode GL_PROJECTION)
      (glLoadIdentity)
      (gluPerspective 45 aspect 0.1 100)

      (glMatrixMode GL_MODELVIEW)
      (glLoadIdentity)
      (define r (\\ mod(time-ms(), 62830) / #i10000))
      (gluLookAt (* 3 (sin r)) 2 (* 3 (cos r))
         0 0 0
         0 1 0)

      ; draw the geoid
      (define (side a b c n)
         (vector-apply a (lambda (ax ay az)
         (vector-apply b (lambda (bx by bz)
         (vector-apply c (lambda (cx cy cz)
            (if (> n 0)
            then
               (define d (normalize [(/ (+ ax bx) 2) (/ (+ ay by) 2) (/ (+ az bz) 2)]))
               (define e (normalize [(/ (+ bx cx) 2) (/ (+ by cy) 2) (/ (+ bz cz) 2)]))
               (define f (normalize [(/ (+ cx ax) 2) (/ (+ cy ay) 2) (/ (+ cz az) 2)]))

               (side a d f (-- n))
               (side d b e (-- n))
               (side e c f (-- n))
               (side d e f (-- n))
            else
               (glVertex3dv a)
               (glVertex3dv b)
               (glVertex3dv c) ))))))))

      ; "нулевой" тетраэдр
      (define r (/ (sqrt 6) 4))
      (define m (negate r))
      (define vertices [
         [m m m] [r m r] [m r r] [r r m] ])

      ; сфера
      (glBegin GL_TRIANGLES)
      (for-each (lambda (a b c)
            (side (normalize (ref vertices a))
                  (normalize (ref vertices b))
                  (normalize (ref vertices c)) DIVISIONS))
         '(1 2 3 4)
         '(2 3 4 1)
         '(3 4 1 2))
      (glEnd)

      (glfwSwapBuffers window)
      (glfwPollEvents) (sleep)
      (loop)))

; done
(glfwDestroyWindow window)
(glfwTerminate)
