## android build
NDK_ROOT ?=/opt/android/ndk
android:
	$(NDK_ROOT)/ndk-build

ForgottenEcho: ForgottenEcho.cpp $(wildcard *.cpp) $(wildcard src/*.c)
	g++ $^ -o $@ -Iinclude -lglfw -lGL -lGLU

all: android ForgottenEcho

.PHONY: android logcat
.PHONY: build clean
.PHONY: install uninstall reinstall
.PHONY: start stop restart

build: android
	./build

clean:
	rm -rf dex lib obj
	find src -name "R.java" -exec rm {} \;
	rm -f debug.apk debug.keystore

install:
	adb -d install -r debug.apk
	@# grant default permissions
	@# adb shell pm grant name.blekki.fe1 android.permission.READ_EXTERNAL_STORAGE

uninstall:
	adb -d uninstall name.blekki.fe1

reinstall:
	@$(MAKE) uninstall
	@$(MAKE) install

start:
	adb shell am start -n name.blekki.fe1/name.blekki.fe1.NativeLoader
stop:
	adb shell am force-stop name.blekki.fe1

restart:
	@$(MAKE) stop
	@$(MAKE) start


logcat:
	adb logcat -v color \
	    name.blekki.fe1:V \
	    ol-egl:V ol-ovr:V gl2es:V \
	    threaded_app:V nativeloader:E \
	    AndroidRuntime:E DEBUG:V *:F
