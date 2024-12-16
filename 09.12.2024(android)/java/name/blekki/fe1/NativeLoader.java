package name.blekki.fe1; // Forgotten Echo

import android.util.Log;

public class NativeLoader extends android.app.NativeActivity {
    static {
		// thirdparty libraries
        System.loadLibrary("vrapi"); // vrApi (Oculus + Meta)
        System.loadLibrary("gl2es"); // OpenGL 2.1 over GLES
        System.loadLibrary("GLU"); // OpenGL Utility Library  (depends on gl2es)
        System.loadLibrary("SOIL"); // Image loading Library  (depends on gl2es)

        System.loadLibrary("egl"); // gles
        System.loadLibrary("ovr"); // vr

		System.loadLibrary("android_app"); // Android Native Layer
		System.loadLibrary("application"); // Yours App Code
		System.loadLibrary("main"); // Android Activity Main
    }
}
