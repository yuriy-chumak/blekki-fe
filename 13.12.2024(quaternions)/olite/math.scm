(define-library (olite math)
(import
   (otus lisp)
   (scheme inexact)
   (srfi 27))

(export
   vec3 quat ; fast vector and quaternion creation
   mat3 mat4 ; mat3 mat4 are column-major matrices (!)

   mat4*vec3 ; matrix * vector
   mat4*vert ; matrix * vertex
   mat4*mat4 ; matrix * matrix

   ; todo: rename to quat->rotation-matrix
   ;                 quat->rotation-matrix3
   rotation-matrix  ; матрица вращения для кватерниона
   rotation-between ; кватернион вращения между векторами

   normalize ; normalize vector
   normalize-quat ; normalize quaternion
   magnitude
   magnitude²
   conjugate ; conjugate quaternion

   ; расширяем обычную математику векторной
   + - * / ; • ⨯ / ;∇ ² π π² ⨉ ∇ ⊹ ✕
   zero-vector
   zero-quaternion
   identity-matrix
   deep-vector-copy

   ; дополнительные функции
   minmax clamp

   mat4-set!

   (exports (scheme inexact))
   (exports (srfi 27))
)

; векторно-матрично-кватернионнаяая арифметика
(begin
   (import (math infix-notation))

   ; быстрые аллокаторы
   (define (vec3 . xyz)
      (vm:make type-vector xyz))
   (define (quat . xyzw)
      (vm:make type-vector xyzw))
   (define (mat3 . mat)
      (vm:make type-vector mat))
   (define (mat4 . mat)
      (vm:make type-vector mat))

   (define (vec3? q)
      (and (vector? q) (eq? (size q) 3)))
   (define (vec4? q)
      (and (vector? q) (eq? (size q) 4) (eq? (ref q 4) 0)))

   (define (vec3-equal? a b) ; fast compare
      (vector-apply a (lambda (x0 y0 z0)
      (vector-apply b (lambda (x1 y1 z1)
         (and (= x0 x1) (= y0 y1) (= z0 z1))
      )))))

   (define (vec3+vec3 a b)
      (vector-apply a (lambda (ax ay az)
      (vector-apply b (lambda (bx by bz)
         (vec3 (+ ax bx) (+ ay by) (+ az bz))
      )))))
   (define (vec3-vec3 a b)
      (vector-apply a (lambda (ax ay az)
      (vector-apply b (lambda (bx by bz)
         (vec3 (- ax bx) (- ay by) (- az bz))
      )))))
   (define (vec3scale v a)
      (vector-apply v (lambda (x y z)
         (vec3 (* x a) (* y a) (* z a))
      )))


   (define (normalize a)
      (define lv (/ #i1 (sqrt (vector-fold + 0 (vector-map * a a)))))
      (vector-map (lambda (x) (* x lv)) a))

   (define (normalize-quat quat)
      (vector-apply quat (lambda (x y z w)
         (define lv (/ #i1 (sqrt (+ (* x x) (* y y) (* z z) (* w w)))))
         [(* lv x) (* lv y) (* lv z) (* lv w)])))

   ; мне нравятся шестнадцатеричные цифры
   (define-values (A B C D E F) (values 10 11 12 13 14 15))

   ; Column-major
   (define (mat4*vec3 mat vec)
      (vector-apply vec (lambda (x y z) (\\
         vec3( ; column-major 
            ref(mat, 1) * x + ref(mat, 5) * y + ref(mat, 9) * z,
            ref(mat, 2) * x + ref(mat, 6) * y + ref(mat, A) * z,
            ref(mat, 3) * x + ref(mat, 7) * y + ref(mat, B) * z
         )))))

   ; Column-major
   (define (mat4*vert mat vec)
      (vector-apply vec (lambda (x y z)
         (vec3
            (+ (* (ref mat 1) x) (* (ref mat 5) y) (* (ref mat 9) z)    (ref mat 13))
            (+ (* (ref mat 2) x) (* (ref mat 6) y) (* (ref mat A) z)    (ref mat 14))
            (+ (* (ref mat 3) x) (* (ref mat 7) y) (* (ref mat B) z)    (ref mat 15))
         ))))


   ; Column-major
   (define (rotation-matrix rotation)
      ; без нормализации кватерниона матрица будет не ортогональна
      (vector-apply (normalize-quat rotation) (lambda (x y z w)
         (define dx (* x 2.))  (define dy (* y 2.))  (define dz (* z 2.))
         (define wx (* w dx))  (define wy (* w dy))  (define wz (* w dz))
         (define xx (* x dx))  (define xy (* x dy))  (define xz (* x dz))
         (define yy (* y dy))  (define yz (* y dz))
         (define zz (* z dz))
         (mat4
            (- #i1.0 yy zz) (+ xy wz) (- xz wy) #i0.0
            (- xy wz) (- #i1.0 xx zz) (+ yz wx) #i0.0
            (+ xz wy) (- yz wx) (- #i1.0 xx yy) #i0.0
            #i0.0       #i0.0       #i0.0       #i1.0
         ))))

   ; rotation quaternion between two vectors (from v0 to v1)
   (define (rotation-between v0 v1)
      (vector-apply v0 (lambda (x0 y0 z0)
      (vector-apply v1 (lambda (x1 y1 z1)
         (define s (sqrt (* #i2 (+ #i1.0 (* x0 x1) (* y0 y1) (* z0 z1)))))
         (cond
            ((> s #i0) [
               (/ (- (* y0 z1) (* z0 y1)) s)
               (/ (- (* z0 x1) (* x0 z1)) s)
               (/ (- (* x0 y1) (* y0 x1)) s)
               (/ s 2) ])
	         ((or (vec3-equal? v1 [0 0 1]) ; Oz
                 (vec3-equal? v0 [0 0 1]))
               [1 0 0 0])
            (else
               [0 0 0 1])))))))

   (define (zero-vector) [
      (inexact 0) (inexact 0) (inexact 0)
   ])
   (define (zero-quaternion) [
      (inexact 0) (inexact 0) (inexact 0) (inexact 1)
   ])
   (define (identity-matrix) [
      (inexact 1) (inexact 0) (inexact 0) (inexact 0)
      (inexact 0) (inexact 1) (inexact 0) (inexact 0)
      (inexact 0) (inexact 0) (inexact 1) (inexact 0)
      (inexact 0) (inexact 0) (inexact 0) (inexact 1)
   ])

   (define (deep-vector-copy v)
      (vector-map (lambda (x) (vm:cast x (type x))) v))

   ;; (define (distance-between2 a b)
   ;;    (define a-b (vector-map - a b))
   ;;    (vector-fold + 0 (vector-map * a-b a-b)))

   (define (magnitude a)
      (vector-apply a (lambda (x y z)
         (sqrt (+ (* x x) (* y y) (* z z)))
      )))

   (define (magnitude² a)
      (vector-apply a (lambda (x y z)
         (+ (* x x) (* y y) (* z z))
      )))

   (define (dot-product a b)
      (vector-apply a (lambda (ax ay az)
      (vector-apply b (lambda (bx by bz)
         (+ (* ax bx) (* ay by) (* az bz)))))))

   ; cross-product
   (define (cross-product a b)
      (vector-apply a (lambda (ax ay az)
      (vector-apply b (lambda (bx by bz)
         (vec3 (- (* ay bz) (* az by))
               (- (* az bx) (* ax bz))
               (- (* ax by) (* ay bx)))
      )))))

   ; -=( smart math )=-------------------
   ; smart plus
   (define +
      (define plus (lambda (a b)
         (if (eq? (type a) type-vector)
            (if (eq? (type b) type-vector)
               (vec3+vec3 a b)
               (runtime-error "invalid vector math: " a " + " b))
            (+ a b))))
      (case-lambda
         ((a b) (plus a b))
         ((a . bc) (fold plus a bc))
         (() 0)))

   ; smart minus
   (define -
      (define minus (lambda (a b)
         (if (eq? (type a) type-vector)
            (if (eq? (type b) type-vector)
               (vec3-vec3 a b)
               (runtime-error "invalid vector math: " a " - " b))
            (- a b))))
      (define neg (lambda (a)
         (case (type a)
            (type-vector
               (vector-map negate a))
            (else
               (negate a)))))
      (case-lambda
         ((a) (neg a))
         ((a b) (minus a b))
         ((a . bc) (fold minus a bc))
         (() 0)))

   ; smart mult
   (define *
      (define multiply (lambda (a b)
         (if (eq? (type a) type-vector)
            (if (eq? (type b) type-vector)
               (mat4*vec3 a b)
               (vec3scale a b))
            (* a b))))
      (case-lambda
         ((a b) (multiply a b))
         ((a . bc)
               ; перемножение матриц идет справа-налево!
               (define abc (cons a bc))
               (define cba (reverse abc))
               (define c (car cba))
               (define ba (cdr cba))
               (define ab (reverse ba))
               (foldr multiply c ab))
         (() 1)))

   ;(define • dot-product)
   ;(define ⨯ cross-product)

   (define (minmax x a b)
      (min b (max x a)))
   (define clamp minmax)

   (define (mat4-set! to from)
      (for-each (lambda (i)
            (vm:set! (ref to i) (ref from i)))
         (iota (size from))))

   ; both row-major and column-major
   (define (mat4*mat4 matrix1 matrix2)
      (define (at m x y) (ref m (+ x (* y 4) -4)))
      (define out (identity-matrix))
      (for-each (lambda (i)
            (for-each (lambda (j)
                  (vm:set! (at out i j) (fold (lambda (s k)
                        (+ s (* (at matrix1 i k) (at matrix2 k j))))
                        0 (iota 4 1))))
               (iota 4 1)))
         (iota 4 1))
      out)

   (define (conjugate q)
      (quat
         (negate (ref q 1))
         (negate (ref q 2))
         (negate (ref q 3))
         (ref q 4)))

   ; smart divide
   (define /
      (define divide (lambda (a b)
         (if (vector? a)
            (if (vector? b)
               (runtime-error "'-' error: " "can't divide vector by vector")
               (vector-map (lambda (a) (/ a b)) a))
            (/ a b))))
      (case-lambda
         ((a b)(divide a b))
         ((a . bc)
               (fold divide a bc))))

))
