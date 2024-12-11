#pragma once
#include <dlfcn.h>

// # VR support
#define GL_VR_HINT           (0x12001)
#define GL_VR                (0x10C33)

// VR functions
void (*vrBegin)() = 0;
void (*vrEye)(int eye) = 0;
void (*vrFinish)() = 0;
void (*vrEnd)() = 0;

// VR global init
int vrInit() {
    void* self = dlopen("libovr.so", RTLD_NOW);

    vrBegin = (void (*)())dlsym(self, "vrBegin");
    vrEye = (void (*)(int))dlsym(self, "vrUpdate");
    vrFinish = (void (*)())dlsym(self, "vrFinish");
    vrEnd = (void (*)())dlsym(self, "vrEnd");

    return vrBegin && vrEye && vrFinish && vrEnd;
}
