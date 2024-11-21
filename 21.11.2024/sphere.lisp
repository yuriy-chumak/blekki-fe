#!/usr/bin/env ol

(import (EGL 1.4))
(import (OpenGL 1.0)
   (lib GLU))
(import (scheme dynamic-bindings))
(import (math infix-notation))

(define N (make-parameter #i0))
(define show-stages (make-parameter #f))
(define show-triangles (make-parameter #f))
(define show-lines (make-parameter #f))

; some math
(import (scheme inexact))
(define (normalize a)
   (define lv (/ #i1 (sqrt (vector-fold + 0 (vector-map * a a)))))
   (vector-map (lambda (x) (* x lv)) a))

(define colors (vector-map (lambda (color) (map (lambda (v) (inexact (/ v 255))) color))
   '[(255 0 0) (255 90 0) (255 154 0) (255 206 0) (255 232 8)
   (255 232  18) (255 232  38) (255 232  58) (255 232  78) (255 232  98) (255 232 118) (255 232 138) (255 232 158)
   (255 232 178) (255 232 198) (255 232 218) (255 232 238) (255 232 258)
   ]))


(define (draw aspect)
   (glClearColor 0.2 0.2 0.2 1)
   (glClear (bor GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   (glEnable GL_DEPTH_TEST)
   (glDisable GL_CULL_FACE)

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
   (define DIVISIONS (exact (N)))

   (define (loop a b c n triangles)
      (vector-apply a (lambda (ax ay az)
      (vector-apply b (lambda (bx by bz)
      (vector-apply c (lambda (cx cy cz)
         (if (> n 0)
         then
            (define d (normalize [(/ (+ ax bx) 2) (/ (+ ay by) 2) (/ (+ az bz) 2)]))
            (define e (normalize [(/ (+ bx cx) 2) (/ (+ by cy) 2) (/ (+ bz cz) 2)]))
            (define f (normalize [(/ (+ cx ax) 2) (/ (+ cy ay) 2) (/ (+ cz az) 2)]))

            (loop a d f (-- n) triangles)
            (loop d b e (-- n) triangles)
            (loop e c f (-- n) triangles)
            (loop d e f (-- n) triangles)
         else
            (unless triangles (glBegin GL_LINE_LOOP))
            (glVertex3dv a)
            (glVertex3dv b)
            (glVertex3dv c)
            (unless triangles (glEnd))) )))))))

   ; "нулевой" тетраэдр
   (define r (/ (sqrt 6) 4))
   (define m (negate r))
   (define vertices [
      [m m m] [r m r] [m r r] [r r m] ])

   ; генератор
   (when (show-triangles) (glBegin GL_TRIANGLES))

   (if (show-stages)
      (for-each (lambda (n)
            (glColor3fv (ref colors (+ n 1)))
            (for-each (lambda (a b c)
                  (loop (normalize (ref vertices a))
                        (normalize (ref vertices b))
                        (normalize (ref vertices c)) n (show-triangles)))
               '(1 2 3 4)
               '(2 3 4 1)
               '(3 4 1 2)) )
         (iota (+ DIVISIONS 1) 0))
   else
      (glColor3fv (ref colors (+ DIVISIONS 1)))
      (for-each (lambda (a b c)
            (loop (normalize (ref vertices a))
                  (normalize (ref vertices b))
                  (normalize (ref vertices c)) DIVISIONS (show-triangles)))
         '(1 2 3 4)
         '(2 3 4 1)
         '(3 4 1 2)) )

   (when (show-lines)
      (when (show-triangles) (glEnd))
      (glColor3f 0 0 0) 
      (for-each (lambda (a b c)
            (loop (normalize (ref vertices a))
                  (normalize (ref vertices b))
                  (normalize (ref vertices c)) DIVISIONS #f))
         '(1 2 3 4)
         '(2 3 4 1)
         '(3 4 1 2))
      (when (show-triangles) (glBegin GL_TRIANGLES)) )
   
   (when (show-triangles) (glEnd))
)

;; --------------------------------------------
;; -=( UI )=-----------------------------------
;; --
(import
   (lib glib-2)
   (lib gdk-3)
   (lib gtk-3)
   (lib gtk-3 toggle-button)
   (lib gtk-3 glarea))

;; explicitly init
(gtk_init)

(define context (make-parameter [#f #f #f #f]))

;; load ui from the file
(define builder
   (GtkBuilder "ui.glade"))

(define glarea ; OpenGL draw area
   ((builder 'get-object) "glarea"))
(define stages ; "show stages" checkbox
   ((builder 'get-object) "stages"))

((builder 'add-callback-symbol) "Changed"
   (GTK_CALLBACK (me userdata)
      (N (gtk_adjustment_get_value me))
      (gtk_gl_area_queue_render glarea) ; immediately rerender
      TRUE))
(N (gtk_adjustment_get_value ((builder 'get-object) "N")))

((builder 'add-callback-symbol) "Setup EGL"
   (GTK_CALLBACK (widget)
      (define egl_config (vm:cast 0 EGLConfig))
      (define n_config '(0))
      (define attributes [EGL_RENDERABLE_TYPE EGL_OPENGL_BIT EGL_NONE])

      (define egl_display (eglGetDisplay (gdk_x11_display_get_xdisplay (gtk_widget_get_display widget))))
      (eglInitialize egl_display #f #f)
      (eglChooseConfig egl_display attributes egl_config 1 n_config)
      (eglBindAPI EGL_OPENGL_API)

      (define egl_surface (eglCreateWindowSurface egl_display egl_config (gdk_x11_window_get_xid (gtk_widget_get_window widget)) #f))
      (define egl_context (eglCreateContext egl_display egl_config EGL_NO_CONTEXT #f))

      (context [egl_display egl_surface egl_context])
   ))

((builder 'add-callback-symbol) "Draw Me"
   (GTK_CALLBACK (widget)
      (vector-apply (context) (lambda (egl_display egl_surface egl_context)
         (eglMakeCurrent egl_display egl_surface egl_surface egl_context)
         (glViewport 0 0 (gtk_widget_get_allocated_width widget) (gtk_widget_get_allocated_height widget))

         (draw (/ (gtk_widget_get_allocated_width widget) (gtk_widget_get_allocated_height widget)))

         (eglSwapBuffers egl_display egl_surface)
         (gtk_gl_area_queue_render glarea) ; rerender again
         TRUE))
   ))

((builder 'add-callback-symbol) "Stages Toggled"
   (GTK_CALLBACK (self userdata)
      (show-stages (gtk_toggle_button_get_active self))
      TRUE))

((builder 'add-callback-symbol) "Triangles Toggled"
   (GTK_CALLBACK (self userdata)
      (show-triangles (gtk_toggle_button_get_active self))
      TRUE))

((builder 'add-callback-symbol) "Lines Toggled"
   (GTK_CALLBACK (self userdata)
      (show-lines (gtk_toggle_button_get_active self))
      TRUE))

((builder 'connect-signals))

;; setup main window
(define window (GtkWindow
   ((builder 'get-object) "window") {
      'title "GLArea with OpenGL 2.1"
      'on-destroy (lambda (this)
         (print "Close pressed. Bye-bye.")
         ; when we do a (gtk_main) we should use (gtk_main_quit)
         ;   instead of (g_application_quit)
         (gtk_main_quit))
   }))

;; show it
((window 'show-all))

;; run
(gtk_main)