#include <windows.h>

int main() {
	char exepath[MAX_PATH];
	GetModuleFileName(0, exepath, MAX_PATH);
	printf(exepath);
	return 0;
}
