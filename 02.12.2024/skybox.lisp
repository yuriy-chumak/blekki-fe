(import (srfi 27))
(import (olite math))

(define SKYBOX-RADIUS 900)

; number of images in atlas
(define SKYBOX-ATLASSIZE 16)

(define NEBULAS-SCALE #i1.4)
(define STARS-SCALE #i0.021)

(define NEBULAS-ALPHA #i0.1)
(define STARS-ALPHA #i0.812)

; textures
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

(define PI #i3.14159265)

; todo: добавить зерно генератора
(define (skybox:generate! stars nebulas)
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

(define skybox-program (gl:create-program
"  #version 120 // OpenGL 2.1

varying vec2 st;
void main()
{
   gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
   gl_FrontColor = gl_Color;
   //gl_TexCoord[0] = gl_MultiTexCoord0;
   st = gl_MultiTexCoord0.st;
}"
"  #version 120 // OpenGL 2.1
uniform sampler2D tex0;

varying vec2 st;
void main()
{
   //vec3 color = texture2D(tex0, gl_TexCoord[0].st).rgb;
   vec3 color = texture2D(tex0, st).rgb;
   float alpha = (color.r + color.g + color.b) / 3.0;
   gl_FragColor = vec4(gl_Color.rgb, gl_Color.a * alpha);
}"))
(define tex0 (glGetUniformLocation skybox-program "tex0"))

(define (skybox:draw)
   (glUseProgram skybox-program)

   (glUniform1i tex0 0)
   (glBindTexture GL_TEXTURE_2D sky-atlas)

   (glCallList skybox))
