(import
   (file wavefront obj)
   (file wavefront mtl))
(import (owl parse))


(define (read-model-file name)
   ;; LOAD

   (define resources "models/")
   ; todo: (or fasl-load (string-append "cache/" filename ".fasl")

   ; Load geometry,
   (define obj-filename (string-append resources name ".obj"))
   (print "Loading object file " obj-filename "...")
   (define obj (parse wavefront-obj-parser (file->bytestream obj-filename) obj-filename #t #empty))
   ; Load materials
   (define mtl-filename (string-append resources (obj 'mtllib "")))
   (print "Loading materials file " mtl-filename "...")
   (define mtl (parse wavefront-mtl-parser (file->bytestream mtl-filename) mtl-filename #t #empty))

   ; (todo: let's convert into inexacts)
   (define vertices (list->vector (obj 'v #null)))
   (define normals (list->vector (obj 'vn #null)))
   (define tcoords (list->vector (obj 'vt #null)))

   ; materials (list of )
   (define mtls (map (lambda (material) [
         (material 'name)     ; name
         (material 'kd)       ; diffuse color
         (material 'map_kd)]) ; diffuse color texture file
      mtl))

   ; geometry
   (define objects (map (lambda (object) [
         (object 'name)
         (object 'facegroups)])
      (obj 'o)))
   ;; todo: (fasl-save model (string-append "cache/" name ".fasl"))

   ;; COMPILE
   ; map of name -> [name Kd map_Kd]
   (define materials (fold (lambda (materials material)
         (vector-apply material (lambda (name kd map_kd)
            (define sname (string->symbol name))
            (put materials sname [
               sname
               (vector-map inexact kd)
               (if map_kd then
                  (define id (let ((buffer (file->bytevector map_kd)))
                     (SOIL_load_OGL_texture_from_memory buffer (size buffer) SOIL_LOAD_RGBA SOIL_CREATE_NEW_ID SOIL_FLAG_INVERT_Y))) ; invert y for bmp
                  ;; (print "map_kd: " map_kd ", id: " id)
                     ; SOIL_FLAG_MIPMAPS
                  (glBindTexture GL_TEXTURE_2D id)
                  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
                  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
                  ;; (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
                  ;; (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
                  (glBindTexture GL_TEXTURE_2D 0)
                  id
               else
                  0)
            ]))))
      {}
      mtls))

   ;; (define mesh (NewtonMeshCreate newtonian-world))
   ;; (NewtonMeshBeginBuild mesh)

   (define submodels (reverse
   (fold (lambda (submodels o)
         (vector-apply o (lambda (name facegroups)
            (define index (glGenLists (length facegroups)))
            (print "compiling object " name "...")

            ;; (define mesh (NewtonMeshCreate newtonian-world))
            ;; (NewtonMeshBeginBuild mesh)

            (define gllists (reverse
               (fold (lambda (o group i)
                        (vector-apply (materials (string->symbol (car group))) (lambda (name kd map_kd)
                           (glNewList i GL_COMPILE)

                           (glColor4fv kd)
                           (glBegin GL_TRIANGLES)
                           (for-each (lambda (face)
                                 ;; (NewtonMeshBeginFace mesh)
                                 (for-each (lambda (vertex)
                                       (vector-apply vertex (lambda (xyz uv n)
                                          (define vertex (ref vertices xyz))
                                          (define normal (ref normals n))

                                          (glTexCoord2dv (ref tcoords uv))
                                          (glNormal3dv normal)
                                          (glVertex3dv vertex)

                                          ;; (apply NewtonMeshAddPoint (cons mesh (vector->list vertex)))
                                          ;; (NewtonMeshAddMaterial mesh 0)
                                          ;; (apply NewtonMeshAddNormal (cons mesh (vector->list normal)))
                                       )))
                                       face)
                                 ;; (NewtonMeshEndFace mesh)
                                 )
                              (cdr group))
                           (glEnd)
                           (glEndList)
                           (cons [i map_kd] o)))) ; [list texture]
                     #null
                     facegroups
                     (iota (length facegroups) index))))

            ;; (NewtonMeshEndBuild mesh)

            ;; (define convex (NewtonCreateConvexHullFromMesh newtonian-world mesh 0.0 0))
            ;; (NewtonCompoundCollisionAddSubCollision collision convex)
            ;; (NewtonDestroyCollision convex)

            (cons [name gllists] submodels))))
      #null
      objects)))

   ;; ;; (NewtonCompoundCollisionEndAddRemove collision)

   ;; (NewtonMeshEndBuild mesh)
   ;; ;; не работает под андроидом:
   ;; ;; (define collision (NewtonCreateCompoundCollisionFromMesh newtonian-world mesh 0.1 0 0))
   ;; (define collision (NewtonCreateNull newtonian-world))

   ;; ;; ; NewtonCollisionCreateInstance + NewtonCollisionGetUserData?
   ;; ;; (NewtonCompoundCollisionBeginAddRemove collision)
   ;; ;; (define collision-radius 12) ; todo: брать из свойств корабля
   ;; ;; (define sphere (NewtonCreateSphere newtonian-world
   ;; ;;       (* collision-radius PROXIMITY-WARNING-DISTANCE)
   ;; ;;       PROXIMITY-SPHERE-ID #f)) ; это некая большая сфера,
   ;; ;;       ; которая должна вызывать "avoid-collision" событие
   ;; ;; (NewtonCompoundCollisionAddSubCollision collision sphere)
   ;; ;; (NewtonCompoundCollisionEndAddRemove collision)

   [submodels #|collision|#]) ; todo: add materials? add something else?


(define (draw-textured model)
   (for-each (lambda (submodel)
         (vector-apply submodel (lambda (name groups)
            (for-each (lambda (group)
                  (vector-apply group (lambda (i map_kd)
                     (glBindTexture GL_TEXTURE_2D map_kd)
                     (glCallList i))))
               groups))))
      (ref model 1)))

(define (draw-geometry model)
   (for-each (lambda (submodel)
         (vector-apply submodel (lambda (name groups)
            (for-each (lambda (group)
                  (vector-apply group (lambda (i map_kd)
                     (glCallList i))))
               groups))))
      (ref model 1)))
