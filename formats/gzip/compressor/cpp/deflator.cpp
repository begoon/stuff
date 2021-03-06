#include "compressor.h"
#include <iostream>
#include <iomanip>
#include <io.h>
#include <fcntl.h>

int main(int argc, char* argv[])
{
	std::string raw;

	setmode(fileno(stdin), _O_BINARY);
	setmode(fileno(stdout), _O_BINARY);

	while (std::cin)
	{
		char buf[10240];
		std::cin.read(buf, sizeof buf);
		raw += std::string(buf, std::cin.gcount());
	}

	GZipCompressor compressor;

	std::string packed;

	try 
	{
		packed = compressor.compress(raw);
	} 
	catch (GZipCompressor::Error& e)
	{
		std::cerr << "error: " << e.what() << std::endl;
		std::exit(1);
	}

	std::cout.write(packed.data(), (std::streamsize)packed.length());

	return 0;
}
