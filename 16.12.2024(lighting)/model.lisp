;; (import
;;    (file wavefront obj)
;;    (file wavefront mtl))
;; (import (owl parse))
(import (file glTF))
(import (only (OpenGL 3.0)
   glBindVertexArray
)) ; todo: use extension

(setq vref vector-ref)

(define (read-model-file name)
   (read-glTF-file (string-append "models/" name ".glb")))


(define (draw-model-axis)
   ; получим матрицу трансформации модели
   (define modelview [
      #i0 #i0 #i0 #i0
      #i0 #i0 #i0 #i0
      #i0 #i0 #i0 #i0
      #i0 #i0 #i0 #i0])
   (glGetFloatv GL_MODELVIEW_MATRIX modelview)
   ;; (print modelview)

   ;; (define x0 [0 0 0 1])
   ;; (define x1 [1 0 0 1])
   ; повернем оси с помощью матрицы вращения (с демо целью)
   (define x0 (mat4*vert modelview [0 0 0 1]))
   (define x1 (mat4*vert modelview [1 0 0 1]))
   (define y0 (mat4*vert modelview [0 0 0 1]))
   (define y1 (mat4*vert modelview [0 1 0 1]))
   (define z0 (mat4*vert modelview [0 0 0 1]))
   (define z1 (mat4*vert modelview [0 0 1 1]))

   (glPushMatrix)
   (glLoadIdentity) ; reset matrix to default one
   (glDisable GL_TEXTURE_2D)
   (glLineWidth 3)
   (glBegin GL_LINES)
      (glColor3f 1 0 0) ; Ox
      (glVertex3fv x0)
      (glVertex3fv x1)
      (glColor3f 0 1 0) ; Oy
      (glVertex3fv y0)
      (glVertex3fv y1)
      (glColor3f 0 0 1) ; Oz
      (glVertex3fv z0)
      (glVertex3fv z1)
   (glEnd)

   ; restore
   (glPopMatrix)
   (glColor3f 1 1 1))

(define (render-model model)
   ; model shortcuts
   (define nodes (model 'nodes))
   (define meshes (model 'meshes))
   (define bufferViews (model 'bufferViews))
   (define accessors (model 'accessors))
   (define materials (model 'materials))
   (define images (model 'images))

   ; current shader program
   (define program (let ((id (box 0)))
      (glGetIntegerv GL_CURRENT_PROGRAM id)
      (unbox id)))

   ; render scene tree
   (let walk ((i 0))
      (define node (vref nodes i))
      (glPushMatrix)
      (when (node 'matrix #f)
         (glMultMatrixf (node 'matrix)))

      (when (node 'mesh #f)
         (define mesh (vref meshes (node 'mesh)))
         (vector-for-each (lambda (primitive)
               (glBindVertexArray (primitive 'vao))

               (define indices (primitive 'indices #f))
               (when indices
                  (define accessor (vref accessors indices))
                  (define bufferView (vref bufferViews (accessor 'bufferView)))

                  (define materialId (primitive 'material #f))
                  (when materialId
                     (define material (vref materials materialId))

                     ; let's pass material filter
                     (when #true
                        (define pbr (material 'pbrMetallicRoughness #f))
                        (when pbr
                           ; ok, we will draw this primitive
                           (define color (pbr 'baseColorFactor #f))
                           (when color
                              (glColor4fv color))

                           (define colorTexture (pbr 'baseColorTexture #f))
                           (when colorTexture
                              (define index (colorTexture 'index))
                              (define image (vref images index))
                              (when image
                                 (glBindTexture GL_TEXTURE_2D (image 'texture)))) )

                        (define alphaCutoff (material 'alphaCutoff #f))
                        (when alphaCutoff
                           (glUniform1f (glGetUniformLocation program "alphaCutoff") alphaCutoff))

                        ; finally, render!
                        (glBindBuffer GL_ELEMENT_ARRAY_BUFFER (bufferView 'vbo))
                        (glDrawElements
                           (case (mesh 'mode 4)
                              (0 GL_POINTS)
                              (1 GL_LINES)
                              (2 GL_LINE_LOOP)
                              (3 GL_LINE_STRIP)
                              (4 GL_TRIANGLES)
                              (5 GL_TRIANGLE_STRIP)
                              (6 GL_TRIANGLE_FAN))
                           (accessor 'count)
                           (accessor 'componentType)
                           (accessor 'byteOffset 0)) )))

               (glBindVertexArray 0))
            (mesh 'primitives [])))
      ; visit children
      (vector-for-each walk (node 'children []))
      (glPopMatrix)))
