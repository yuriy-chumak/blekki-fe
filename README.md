# work plan

### 20 nov 2024
* інсталювати наступні ліби
  * `libglew-dev`, `libglfw3-dev`, `libsoil-dev`
  * пошук ліб у убунті (та інших apt-based дистрибутивах) можна робити через `apt list | grep glfw` (для glfw, наприклад)
* додати це в рідмі в розділ Prerequisites
* написати програму, яка створює просте OpenGL віконечко без рендерінгу та іншого, але закривається по кнопці esc чи хрестику.
  * використати для цього бібліотеку GLFW
  * взяти готовий приклад з https://www.glfw.org/docs/3.3/quick.html, викинути з нього усе стосовно рендерінгу, лишити тільки віконну і клавіатурну логіку
* компіляцію оформити у вигляді Makefile

### 21 nov 2024
* .gitignore
* setup `ol`
* glBegin/glEnd, glMatrixMode, gluLookAt, glClear, etc.

### 22 nov 2024
* rotate triangle with matrices
* draw the sphere with triangles (simplest case - with sin and cos)
* draw the sphere with triangles (smart case - based on tetrahedron)

### backlog
* splash screen (basic texture)
* OpenGL Lists
* sphere transformations (scale + translate)
* viewer position and moves
* texturing (spherical skybox)
* texturing with shaders
* planet texturing
* space dust
* spherical skybox with custom sky textures (bilboarding)
* ...