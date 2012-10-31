/*
	Console access class.
	by Alexander S. Pristenski 2007

	Change log:

	29.01.2007	Alexander S. Pristenski - initial creation.
*/

#ifndef _CONSOLE_H_
#define _CONSOLE_H_

class console
{
public:

	console();
	~console();

	bool init(const char *name);
	bool read(char *buf, unsigned long size);
	bool write(const char *buf, unsigned long left);
	bool is_alive();
	void destroy();

private:

	// For connection through pipes
	STARTUPINFO			si;
	SECURITY_ATTRIBUTES sa;
	SECURITY_DESCRIPTOR sd;
	PROCESS_INFORMATION pi;
	HANDLE				newstdin, newstdout;
	HANDLE				read_stdout, write_stdin;
};


#endif
