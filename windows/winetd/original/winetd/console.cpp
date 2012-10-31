/*
	Console access class.
	by Alexander S. Pristenski 2007

	Change log:

	29.01.2007	Alexander S. Pristenski - initial creation.
*/

#include <windows.h>
#include "console.h"

/*
	Name: console()

	Description: default constructor.

	Parameters: none.

	Return value: none.

	Revision: 29.01.2007
*/
console::console()
{
}

/*
	Name: ~console()

	Description: default destructor.

	Parameters: none.

	Return value: none.

	Revision: 29.01.2007
*/
console::~console()
{
}

/*
	Name: init()

	Description: initialize console application.

	Parameters:
				[in] name - a name of the console application
							with a full path.

	Return value:
					ON SUCCESS: TRUE
					ON   ERROR: FALSE

	Revision: 29.01.2007
*/
bool console::init(const char *name)
{
	// init security descriptor
	InitializeSecurityDescriptor(&sd, SECURITY_DESCRIPTOR_REVISION);
	SetSecurityDescriptorDacl(&sd, true, NULL, false);

	// init security attributes
	sa.lpSecurityDescriptor = &sd;
	sa.nLength = sizeof(SECURITY_ATTRIBUTES);
	sa.bInheritHandle = true; // endble handle inherit

	// create pipes and I/O redirect
	CreatePipe(&newstdin, &write_stdin, &sa, 0);
	CreatePipe(&read_stdout, &newstdout, &sa, 0);

	// init startupinfo
	GetStartupInfo(&si);
	si.dwFlags = STARTF_USESTDHANDLES | STARTF_USESHOWWINDOW;
	si.wShowWindow = SW_HIDE;
	si.hStdOutput = newstdout;
	si.hStdError  = newstdout;
	si.hStdInput  = newstdin;

	if(0 == CreateProcess(name, NULL, NULL, NULL, true, CREATE_NEW_CONSOLE, NULL, NULL, &si, &pi))
	{
		return false;
	}

	return true;
}

/*
	Name: read()

	Description: read from stdout & stderr of the console application.

	Parameters:
				[in] buf  - buffer to place data read.
				[in] size - size of the buffer.

	Return value:
					ON SUCCESS: TRUE
					ON   ERROR: FALSE

	Revision: 29.01.2007
*/
bool console::read(char *buf, unsigned long size)
{
	int res;
	unsigned int  idx = 0;
	unsigned long received;

	unsigned long bread = 0;
	unsigned long avail = 0;

	if(false == PeekNamedPipe(read_stdout, buf, (size - 1), &bread, &avail, NULL))
	{
		return false;
	}

	while(bread > 0)
	{
		if(0 == (res = ReadFile(read_stdout, &buf[idx], bread, &received, NULL)))
		{
			return false;
		}
		if(res && (received == 0))
		{
			return false; // EOF
		}
		bread -= received;
		idx   += received;
	}

	return true;
}

/*
	Name: write()

	Description: write to stdin of the console application.

	Parameters:
				[in] buf  - buffer to write to stdin.
				[in] left - bytes to write.

	Return value:
					ON SUCCESS: TRUE
					ON   ERROR: FALSE

	Revision: 29.01.2007
*/
bool console::write(const char *buf, unsigned long left)
{
	int res;
	unsigned int  idx = 0;
	unsigned long sent;

	while(left > 0)
	{
		if(0 == (res = WriteFile(write_stdin, &buf[idx], left, &sent, NULL)))
		{
			return false;
		}
		left -= sent;
		idx  += sent;
	}

	return true;
}

/*
	Name: is_alive()

	Description: check if console application is alive.

	Parameters: none.

	Return value:
					ON SUCCESS: TRUE
					ON   ERROR: FALSE

	Revision: 29.01.2007
*/
bool console::is_alive()
{
	unsigned long state = 0;
	GetExitCodeProcess(pi.hProcess, &state);
	return (STILL_ACTIVE == state) ? true : false;
}

/*
	Name: destroy()

	Description: kill console application instance.

	Parameters: none.

	Return value: none.

	Revision: 29.01.2007
*/
void console::destroy()
{
	if(pi.hProcess) TerminateProcess(pi.hProcess, 0);
	if(pi.hThread)  CloseHandle(pi.hThread);
	if(pi.hProcess) CloseHandle(pi.hProcess);
	if(newstdin)    CloseHandle(newstdin);
	if(newstdout)   CloseHandle(newstdout);
	if(read_stdout) CloseHandle(read_stdout);
	if(write_stdin) CloseHandle(write_stdin);
}
