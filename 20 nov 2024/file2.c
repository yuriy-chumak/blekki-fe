#include <stdio.h>

#include "file1.h"
int main()
{
	printf("main2\n");
	int y = fnc1(123);
	printf("result = %d\n", y);

	return 0;
}