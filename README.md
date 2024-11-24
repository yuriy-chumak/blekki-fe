# work plan

### Prerequisites
- [x] C++ compiler (g++)
- [x] CMake
- [x] GLFW library (`apt install glfw-dev`)
- [x] Otus Lisp (https://github.com/yuriy-chumak/ol)
- [ ] MangoHud: https://github.com/flightlessmango/MangoHud
- ...

### 24 nov 2024
- [ ] move sphere creation to the "sphere.c" and "sphere.h".

### 23 nov 2024
- [ ] make master main github branch
- [ ] update CMakeLists.txt (remove glfw folder)
- [ ] add CMakeFiles/ to .gitignore

### 22 nov 2024
- [x] rotate triangle with matrices
- [ ] draw the sphere with triangles or quads (simplest case - with sin and cos)
  - [x] draw the circle with triangles (simplest case - with sin and cos)
  - [ ] use gluLookAt and gluPerspective to view the real 3D
- [ ] draw the sphere with triangles (smart case - based on tetrahedron)

### 21 nov 2024
- [ ] .gitignore
- [x] setup `ol`
- [x] glBegin/glEnd, glMatrixMode, gluLookAt, glClear, etc.

### 20 nov 2024
- [x] інсталювати наступні ліби
  * `libglew-dev`, `libglfw3-dev`, `libsoil-dev`
  * пошук ліб у убунті (та інших apt-based дистрибутивах) можна робити через `apt list | grep glfw` (для glfw, наприклад)
- [ ] додати це в рідмі в розділ Prerequisites
- [x ] написати програму, яка створює просте OpenGL віконечко без рендерінгу та іншого, але закривається по кнопці esc чи хрестику.
  - [x] використати для цього бібліотеку GLFW
  - [x] взяти готовий приклад з https://www.glfw.org/docs/3.3/quick.html, викинути з нього усе стосовно рендерінгу, лишити тільки віконну і клавіатурну логіку
- [x] компіляцію оформити у вигляді ~~Makefile~~ CMake

### backlog
- [ ] splash screen (basic texture)
- [ ] OpenGL Lists
- [ ] sphere transformations (scale + translate)
- [ ] viewer position and moves
- [ ] texturing (spherical skybox)
- [ ] texturing with shaders
- [ ] planet texturing
- [ ] space dust
- [ ] spherical skybox with custom sky textures (bilboarding)
- [ ] newton-dynamics library
  - [ ] collisions
  - [ ] movinf physics
- ...
