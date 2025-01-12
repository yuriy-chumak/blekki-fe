(define-library (olite planet)
   (version 1.0)
   (license MIT/LGPL3)
   (keywords (otus ol))

   (export
      geoid)

   (import
      (otus lisp)
      (OpenGL 1.4)
      (olite math))

(begin
   ; процедурный геоид (медленнее, но гибче) / todo: перенести в отдельную библиотеку?
   (define geoid (glGenLists 1))
   (glNewList geoid GL_COMPILE)
   (begin
      (glBegin GL_TRIANGLES)

      ; todo: проверить:
      ; если не нормализовать точки - страдает разбиение
      (define (loop a b c n)
         (vector-apply a (lambda (ax ay az)
         (vector-apply b (lambda (bx by bz)
         (vector-apply c (lambda (cx cy cz)
            (if (> n 0)
            then
               (define d (normalize [(/ (+ ax bx) 2) (/ (+ ay by) 2) (/ (+ az bz) 2)]))
               (define e (normalize [(/ (+ bx cx) 2) (/ (+ by cy) 2) (/ (+ bz cz) 2)]))
               (define f (normalize [(/ (+ cx ax) 2) (/ (+ cy ay) 2) (/ (+ cz az) 2)]))

               (loop a d f (-- n))
               (loop d b e (-- n))
               (loop e c f (-- n))
               (loop d e f (-- n))
            else
               (glVertex3dv a)
               (glVertex3dv b)
               (glVertex3dv c)) )))))))

      ; сгенерируем нужной точности сферу
      (define DIVISIONS 5) ; TODO: move to json config
      ; "нулевой" тетраэдр
      (define r (/ (sqrt 6) 4))
      (define m (negate r))
      (define vertices [
         [m m m] [r m r] [m r r] [r r m] ])
      ; генератор
      (for-each (lambda (a b c)
            (loop (normalize (ref vertices a))
                  (normalize (ref vertices b))
                  (normalize (ref vertices c)) DIVISIONS))
         '(1 2 3 4)
         '(2 3 4 1)
         '(3 4 1 2))
   (glEnd))
   (glEndList)

))