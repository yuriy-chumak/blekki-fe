#!/usr/bin/env ol
(import (lib gl-2)
   (lib soil))

(glOrtho 0 1 1 0 0 1)
(glEnable GL_TEXTURE_2D)

(define id
   (let ((file (file->bytevector "splash.jpeg")))
      (SOIL_load_OGL_texture_from_memory file (size file) SOIL_LOAD_RGBA SOIL_CREATE_NEW_ID 0)))
(glBindTexture GL_TEXTURE_2D id)
(glBegin GL_QUADS)
   ; рисуем на весь экран квадратик с текстурой
   (for-each (lambda (xy)
         (glTexCoord2f (car xy) (cdr xy))
         (glVertex2f (car xy) (cdr xy)))
      '((0 . 0) (1 . 0) (1 . 1) (0 . 1)))
(glEnd)
(glDisable GL_TEXTURE_2D)
(gl:redisplay)
(glDeleteTextures 1 (list id)) ; и спокойно удалим сплеш текстуру

; simulate loading for 5 seconds
(for-each (lambda (x)
      (display ".")
      (wait 1000))
   (iota 5))

; render pass
(glLoadIdentity)
(glOrtho -1 1 1 -1 -1 1)

(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT)

   (glBegin GL_TRIANGLES)
      (glColor3f 1 0 0)
      (glVertex2f -0.6 -0.6)

      (glColor3f 0 1 0)
      (glVertex2f +0.6 -0.6)

      (glColor3f 0 0 1)
      (glVertex2f -0.0 +0.7)
   (glEnd) ))
