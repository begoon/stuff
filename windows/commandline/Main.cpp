#include <windows.h>

#include <iostream>
#include <string>

int main(int argc, char* argv[])
{
	std::string cmdline = GetCommandLine();
	std::cout << GetCommandLine() << std::endl;
	std::cout << cmdline.substr(0, cmdline.find(" ")) << std::endl;
	return 0;
}
