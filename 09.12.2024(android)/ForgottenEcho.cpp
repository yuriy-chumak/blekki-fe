#include <vr.h>

#define GLFW_INCLUDE_NONE
#include <GLFW/glfw3.h>
#include <GL/gl.h>
#include <GL/glu.h>
// #include <SOIL/SOIL.h>

#include <cube.h>

#include <unistd.h>

#include <cmath>
#include <iostream>
#include <stdlib.h>

// global constants
int width = 600;
int height = 400;

using namespace std;


// renderer
float angle = 0.0f;

void draw(int eye) {
    int viewport[] = {0, 0, 0, 0};
    glGetIntegerv(GL_VIEWPORT, viewport);
    float aspect = (viewport[2]-viewport[0]) / (float)(viewport[3]-viewport[1]);

    glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPerspective(45.0f, aspect, 0.1f, 1000.0f);

    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    gluLookAt(0,  0,  5,
              0,  0,  0,
              0,  1,  0);

    angle = glfwGetTime() * 31.415f;
    glRotatef(angle, 0, 1, 0);
    glTranslatef(0, -2, 0);

    cube();
}

//##############################################
//<><><><><><><><> MAIN PROGRAM <><><><><><><><>
int main(void)
{
    // check did glfw run or not
    if (!glfwInit())
    {
        cout << "error: glfw didn'd run" << endl;
        exit(EXIT_FAILURE);
    }

    // time and fps
    glfwSetTime(0.0f);
    // double time = glfwGetTime();
    glfwSwapInterval(1);

    // enable VR mode
    int vrPresent = 0;
    glGetIntegerv(GL_VR, &vrPresent);
    if (vrPresent) {
        if (!vrInit()) {
            cout << "error: no VR library found, disable VR mode" << endl;
            glHint(GL_VR, 0);
        }
    }

    // create window
    GLFWwindow *basicWindow = glfwCreateWindow(width, height, "Basic Window", NULL, NULL);
    if (!basicWindow)
    {
        cout << "error: window can't be created" << endl;
        exit(EXIT_FAILURE);
    }
    glfwMakeContextCurrent(basicWindow);

    // init
    glClearColor(0.2, 0.2, 0.2, 0.2);

    //enable gl functions
    glEnable(GL_DEPTH_TEST);
    // glEnable(GL_TEXTURE_2D);

    // loop
    while (!glfwWindowShouldClose(basicWindow))
    {
        if (vrPresent) {
            // VR mode
            glHint(GL_VR_HINT, 1); // enable VR matrices use
            vrBegin(); // begin VR rendering
            for (int eye = 1; eye < 3; eye++) {
                vrEye(eye); // activate left or right VR eye
                draw(eye);  // render scene
                vrFinish(); // notify VR we are finished
            }
            vrEnd(); // end VR rendering
            glHint(GL_VR_HINT, 0); // disable VR matrices use
        }
        // non VR mode
        else {
            int width, height;
            glfwGetFramebufferSize(basicWindow, &width, &height);
            glViewport(0, 0, width, height);
            draw(0);
        }

        // other needy actions
        glfwSwapBuffers(basicWindow);
        glfwPollEvents();
    }

    // close everything
    glfwDestroyWindow(basicWindow);
    glfwTerminate();
}
