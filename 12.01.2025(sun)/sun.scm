(define-library (sun)
   (version 1.0)
   (license MIT/LGPL3)
   (keywords (otus ol))

   (export
      prerender
      draw-corona ; todo: remove
      draw)

   (import
      (otus lisp)
      (lib gl-2)
      (olite math))

(cond-expand
   (Android
      (import (OpenGL OES framebuffer_object)))
   (else
      (import (OpenGL EXT framebuffer_object))))

(begin
   ; config
   (define SUN-RENDER-DISTANCE 32000)

   ; программа рендеринга поверхности солнца
   ; todo: сделать поверхность процедурной
   (setq shader-program (gl:create-program
   "  #version 120 // OpenGL 2.1
   #define my_ModelMatrix gl_TextureMatrix[0]

   varying vec4 vertexPosition;
   void main()
   {
      // наша сфера не сферична (чтобы быстрее считать)
      // поэтому мы делаем ее сферичной в шейдере:
      vec4 vertex = vec4(normalize(gl_Vertex.xyz), 1.0);

      vertexPosition = my_ModelMatrix * vertex;

      gl_Position = gl_ModelViewProjectionMatrix * vertexPosition;
      gl_TexCoord[0] = vertex; // положение вершины в модельном пространстве (для текстурирования)
   }"

   "  #version 120 // OpenGL 2.1
   #define PI 3.1415927410125732421875 // IEEE754 Pi Approximation

   uniform sampler2D tex0;
   varying vec4 vertexPosition;
   varying float depth;
   void main()
   {
      float x = gl_TexCoord[0].x,
            y = gl_TexCoord[0].y,
            z = gl_TexCoord[0].z;
      float phi = 1.0 - (1.0 + atan(x, -z)/PI) / 2.0;
      float theta = acos(y) / PI;

      gl_FragColor = texture2D(tex0, vec2(phi, theta));
   }"))
   (setq tex0 (glGetUniformLocation shader-program "tex0"))

   ; textures
   (import (lib soil))
   (define texture (let ((buffer (file->bytevector "media/2k_sun.jpg")))
         (SOIL_load_OGL_texture_from_memory buffer (size buffer) SOIL_LOAD_RGB SOIL_CREATE_NEW_ID SOIL_FLAG_MIPMAPS)))
   ; настроим бесшовность текстуры и сглаживание
   (glBindTexture GL_TEXTURE_2D texture)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
   (glBindTexture GL_TEXTURE_2D 0)

   (define PI #i3.14159265)
   (import (only (olite planet) geoid))

   ; -=( draw )=------------------------------
   (import (lib GLU))
   (define (draw radius position rotation)
      (glPushMatrix)
      ;; (glMultMatrixd (vector-apply matrix (lambda (a b c d  e f g h  i j k l  x y z w)
      ;;    [a b c d  e f g h  i j k l  0 0 0 w])))

      ; так как мы не делаем z-буфер линейным (мы запускаемся и на мобильных девайсах),
      ;  и солнце у нас всегда "далеко-далеко", то поместим его поближе. так, чтобы не
      ;  калечилась при выводе моделька:
      (define multiplier (/ SUN-RENDER-DISTANCE (magnitude position)))
      (define loc (vec3scale position multiplier))
      (define r (* radius multiplier))

      ; подготовим модельную матрицу
      (vector-apply loc (lambda (x y z)
         (glTranslated x y z)))
      (glScalef r r r)
      ; todo: сделать правильный rotation из кватерниона
      (glRotatef (* rotation -3) 0 1 0)

      (glUseProgram shader-program)
      (glColor3f 1 1 1)
      (glUniform1i tex0 0)
      (glBindTexture GL_TEXTURE_2D texture)
      (glCallList geoid)
      
      (glPopMatrix))

   ; солнечная корона
   ; текстура 256*256
   (define corona-texture (let* ((id (box 0)) ; todo: rename to corona-texture, or sun-texture, etc.
                                 (ok (glGenTextures 1 id)))
                      (unbox id)))
   (glBindTexture GL_TEXTURE_2D corona-texture)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR) ; nearest??
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
   (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA 256 256 0 GL_RGBA GL_UNSIGNED_BYTE 0)
   (glBindTexture GL_TEXTURE_2D 0)

   ; текстурный буфер нам нужен, а вот буфер глубины - нет
   (define framebuffer (let* ((id (box 0))
                              (ok (glGenFramebuffers 1 id)))
                          (unbox id)))
   (glBindFramebuffer GL_FRAMEBUFFER framebuffer)
   (glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D corona-texture 0)
   (glBindFramebuffer GL_FRAMEBUFFER 0)

   ; попытка в процедурное солнце
   (define shader-program (gl:create-program
   "  #version 120 // OpenGL 2.1
   void main()
   {
      gl_Position = gl_Vertex;
   }"

   "  #version 120 // OpenGL 2.1
   float snoise(vec3 uv, float res)	// by trisomie21
   {
      const vec3 s = vec3(1e0, 1e2, 1e4);
      
      uv *= res;
      
      vec3 uv0 = floor(mod(uv, res))*s;
      vec3 uv1 = floor(mod(uv+vec3(1.), res))*s;
      
      vec3 f = fract(uv); f = f*f*(3.0-2.0*f);
      
      vec4 v = vec4(uv0.x+uv0.y+uv0.z, uv1.x+uv0.y+uv0.z,
                  uv0.x+uv1.y+uv0.z, uv1.x+uv1.y+uv0.z);
      
      vec4 r = fract(sin(v*1e-3)*1e5);
      float r0 = mix(mix(r.x, r.y, f.x), mix(r.z, r.w, f.x), f.y);
      
      r = fract(sin((v + uv1.z - uv0.z)*1e-3)*1e5);
      float r1 = mix(mix(r.x, r.y, f.x), mix(r.z, r.w, f.x), f.y);
      
      return mix(r0, r1, f.z)*2.-1.;
   }

   uniform sampler2D tex0;
   uniform float iTime;
   float freqs[4];

   void main()
   {
      freqs[0] = texture2D(tex0, vec2( 0.01, 0.25 )).x;
      freqs[1] = texture2D(tex0, vec2( 0.07, 0.25 )).x;
      freqs[2] = texture2D(tex0, vec2( 0.15, 0.25 )).x;
      freqs[3] = texture2D(tex0, vec2( 0.30, 0.25 )).x;

      float brightness	= freqs[1] * 0.25 + freqs[2] * 0.25;
      float radius		= 0.24 + brightness * 0.2;
      float invRadius 	= 1.0/radius;
      
      vec3 orange			= vec3( 0.8, 0.65, 0.3 );
      vec3 orangeRed		= vec3( 0.8, 0.35, 0.1 );
      float time		= iTime * 0.1;
      float aspect	= 1.0;
      vec2 uv			= gl_FragCoord.xy / vec2(256, 256);
      vec2 p 			= -0.5 + uv;
      p.x *= aspect;

      float fade		= pow( length( 2.0 * p ), 0.5 );
      float fVal1		= 1.0 - fade;
      float fVal2		= 1.0 - fade;
      
      float angle		= atan( p.x, p.y )/6.2832;
      float dist		= length(p);
      vec3 coord		= vec3( angle, dist, time * 0.1 );
      
      float newTime1	= abs( snoise( coord + vec3( 0.0, -time * ( 0.35 + brightness * 0.001 ), time * 0.015 ), 15.0 ) );
      float newTime2	= abs( snoise( coord + vec3( 0.0, -time * ( 0.15 + brightness * 0.001 ), time * 0.015 ), 45.0 ) );	
      for( int i=1; i<=7; i++ ){
         float power = pow( 2.0, float(i + 1) );
         fVal1 += ( 0.5 / power ) * snoise( coord + vec3( 0.0, -time, time * 0.2 ), ( power * ( 10.0 ) * ( newTime1 + 1.0 ) ) );
         fVal2 += ( 0.5 / power ) * snoise( coord + vec3( 0.0, -time, time * 0.2 ), ( power * ( 25.0 ) * ( newTime2 + 1.0 ) ) );
      }
      
      float corona		= pow( fVal1 * max( 1.1 - fade, 0.0 ), 2.0 ) * 50.0;
      corona				+= pow( fVal2 * max( 1.1 - fade, 0.0 ), 2.0 ) * 50.0;
      corona				*= 1.2 - newTime1;
      vec3 sphereNormal = vec3( 0.0, 0.0, 1.0 );
      vec3 dir 			= vec3( 0.0 );
      vec3 center			= vec3( 0.5, 0.5, 1.0 );
      vec3 starSphere	= vec3( 0.0 );

      vec2 sp = -1.0 + 2.0 * uv;
      sp.x *= aspect;
      sp *= ( 2.0 - brightness );
      float r = dot(sp,sp);
      float f = (1.0-sqrt(abs(1.0-r)))/(r) + brightness * 0.5;
      if( dist < radius ){
         corona			*= pow( dist * invRadius, 24.0 );
         vec2 newUv;
         newUv.x = sp.x*f;
         newUv.y = sp.y*f;
         newUv += vec2( time, 0.0 );
         
         vec3 texSample 	= texture2D( tex0, newUv ).rgb;
         float uOff		= ( texSample.g * brightness * 4.5 + time );
         vec2 starUV		= newUv + vec2( uOff, 0.0 );
         starSphere		= texture2D( tex0, starUV ).rgb;
      }
      
      float starGlow	= min( max( 1.0 - dist * ( 1.0 - brightness ), 0.0 ), 1.0 );
      vec3 rgb	= vec3( f * ( 0.75 + brightness * 0.3 ) * orange ) + starSphere + corona * orange + starGlow * orangeRed;
      float lr = length(r);
      float a = clamp(1.0 - ((length(r) - 1.0) / 2.6), 0.0, 1.0);

      gl_FragColor = vec4(rgb, a);
   }"))

   (define (prerender)
      (glBindFramebuffer GL_FRAMEBUFFER framebuffer)
      (glViewport 0 0 256 256)
      (glClearColor 0.2 0.2 0.2 1)
      (glClear GL_COLOR_BUFFER_BIT)

      (glDisable GL_BLEND)
      (glDisable GL_DEPTH_TEST)

      (glMatrixMode GL_PROJECTION)
      (glLoadIdentity)
      (glMatrixMode GL_MODELVIEW)
      (glLoadIdentity)

      (glUseProgram shader-program)
      (glUniform1i (glGetUniformLocation shader-program "tex0") 0)
      (glUniform1f (glGetUniformLocation shader-program "iTime") (/ (mod (time-ms) 1000000) 1000))
      (glBindTexture GL_TEXTURE_2D corona-texture)
      (glBegin GL_QUADS)
         (glVertex2f -1 -1)
         (glVertex2f -1  1)
         (glVertex2f  1  1)
         (glVertex2f  1 -1)
      (glEnd)
      (glBindFramebuffer GL_FRAMEBUFFER 0)
   )

   (define (get-corona-texture)
      corona-texture)

   (define simple-texturing-program (gl:create-program
      "  #version 120 // OpenGL 2.1
      #define my_ModelMatrix gl_TextureMatrix[0]
      void main()
      {
         gl_Position = gl_ModelViewProjectionMatrix * my_ModelMatrix * gl_Vertex;
         gl_TexCoord[0] = gl_MultiTexCoord0;
      }"

      "  #version 120 // OpenGL 2.1
      uniform sampler2D tex0;
      void main()
      {
         gl_FragColor = texture2D(tex0, gl_TexCoord[0].st);
      }"))

   (define (draw-corona radius position)
      ; так как мы не делаем z-буфер линейным (мы запускаемся и на мобильных девайсах),
      ;  и солнце у нас всегда "далеко-далеко", то поместим его поближе. так, чтобы не
      ;  калечилась при выводе моделька:
      (define multiplier (/ SUN-RENDER-DISTANCE (magnitude position)))
      (define loc (vec3scale position multiplier))
      (define r (* radius multiplier 2)) ; размер с учетом короны
      ; loc - место в пространстве, где мы собираемся разместить солнце

      ; правильно перенесем туда наш билборд
      (define xyz (normalize loc))
      (define rotation (rotation-between xyz [0 0 -1]))
      (define M (rotation-matrix3 rotation))

      (define R r)
      (define +R R) (define -R (negate R))
      (define -SUN-RENDER-DISTANCE (negate SUN-RENDER-DISTANCE))

      (glUseProgram simple-texturing-program)
      (glActiveTexture GL_TEXTURE0)
      (glUniform1i (glGetUniformLocation simple-texturing-program "tex0") 0)
      (glBindTexture GL_TEXTURE_2D corona-texture)
      (glBegin GL_QUADS)
         (glTexCoord2f 0 0)
         (glVertex3dv (mat3*vec3 M [-R -R -SUN-RENDER-DISTANCE]))
         (glTexCoord2f 0 1)
         (glVertex3dv (mat3*vec3 M [-R +R -SUN-RENDER-DISTANCE]))
         (glTexCoord2f 1 1)
         (glVertex3dv (mat3*vec3 M [+R +R -SUN-RENDER-DISTANCE]))
         (glTexCoord2f 1 0)
         (glVertex3dv (mat3*vec3 M [+R -R -SUN-RENDER-DISTANCE]))
      (glEnd))


))
