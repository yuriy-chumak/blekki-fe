#include <sources/android/native_app_glue/android_native_app_glue.c>

void anlMain(struct android_app *app);
void anlDone(struct android_app *app);

int main(int argc, char** argv);

void android_main(struct android_app *app)
{
	anlMain(app);
	main(0, 0);
	anlDone(app);
}
