#include <cube.h>

#include <GL/gl.h>

float vertices[] = {
    1,  1,  1, //1
    1,  1, -1, //2
   -1,  1, -1, //3
   -1,  1,  1, //4
   
    1, -1,  1, //5
    1, -1, -1, //6
   -1, -1, -1, //7
   -1, -1,  1, //8
};

int indices[] = {
   1, 2, 3, 4, // top (yellow)
   1, 5, 6, 2, // right (orange)
   1, 4, 8, 5, // front (green)
   4, 3, 7, 8, // red (left)
   2, 6, 7, 3, // back (blue)
   5, 8, 7, 6, // while (bottom)
};

float colors[] = {
   1, 1, 0,
   1, 0.31, 0,
   0, 1, 0,
   1, 0, 0,
   0, 0, 1,
   1, 1, 1,
};

void cube()
{
	// glBegin(GL_QUADS);
	glBegin(GL_TRIANGLES);
	for (int i = 0; i < sizeof(indices)/sizeof(indices[0])/4; i++) {
		glColor3fv(&colors[i*3]);
		// for (int j = 0; j < 4; j++) {
		// 	int k = indices[i*4 + j] - 1;
		// 	glVertex3fv(&vertices[k * 3]);
		// }

		int k;
		k = indices[i*4 + 0] - 1;
		glVertex3fv(&vertices[k * 3]);
		k = indices[i*4 + 1] - 1;
		glVertex3fv(&vertices[k * 3]);
		k = indices[i*4 + 2] - 1;
		glVertex3fv(&vertices[k * 3]);

		k = indices[i*4 + 0] - 1;
		glVertex3fv(&vertices[k * 3]);
		k = indices[i*4 + 2] - 1;
		glVertex3fv(&vertices[k * 3]);
		k = indices[i*4 + 3] - 1;
		glVertex3fv(&vertices[k * 3]);

	}
	glEnd();
}
