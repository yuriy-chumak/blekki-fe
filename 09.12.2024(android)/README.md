Android Build
=============

Prerequisites
-------------

* Android SDK (just SDK, Android Studio is not needed)
  - Default path is `/opt/android/sdk`, can be changed with `ANDROID_SDK=new-path-here` environment variable
* Android NDK (android-ndk-r21)
  - Default path is `/opt/android/ndk`, can be changed with `NDK_ROOT=new-path-here` environment variable
* jni/gl2es, jni/GLU, jni/SOIL thirdparties
  - use configure script once like `cd jni; ./configure`

Building
--------

### Android
`make android`

Thats it. You should get `debug.apk`.

### Linux
`make ForgottenEcho`

Thats it. You should get `ForgottenEcho`.


Running
-------

### Android
`make install; make start`

### Linux
`./ForgottenEcho`

