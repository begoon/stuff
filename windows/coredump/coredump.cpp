#include <windows.h>
#include <dbghelp.h>   
#include <stdio.h>       // _snprintf

static LONG WINAPI ExceptionFilter(EXCEPTION_POINTERS* ExceptionInfo);

static struct CoredumpInitializer {
	CoredumpInitializer() {
		SetUnhandledExceptionFilter(&ExceptionFilter);
	}
} coredumpInitializer;

LONG WINAPI ExceptionFilter(EXCEPTION_POINTERS* ExceptionInfo) {
	char fname[_MAX_PATH];

	SYSTEMTIME st;
	GetLocalTime(&st);

	HANDLE proc = GetCurrentProcess();

	_snprintf(
		fname, _MAX_PATH, 
		"coredump-%ld-%ld-%04d%02d%02d%02d%02d%02d%03d.dmp", 
		GetProcessId(proc), GetCurrentThreadId(),
		st.wYear, st.wMonth, st.wDay, 
		st.wHour, st.wMinute, st.wSecond, st.wMilliseconds
	);

	HANDLE file = CreateFile(
		fname, 
		GENERIC_READ|GENERIC_WRITE, 
		FILE_SHARE_READ, 
		NULL,
		CREATE_ALWAYS, 
		FILE_ATTRIBUTE_NORMAL, 
		NULL
	);

	MINIDUMP_EXCEPTION_INFORMATION info;
	info.ExceptionPointers = ExceptionInfo;
	info.ThreadId = GetCurrentThreadId();
	info.ClientPointers = NULL;

	MiniDumpWriteDump(	
		proc, 
		GetProcessId(proc), 
		file,
		MiniDumpWithFullMemory,
		ExceptionInfo ? &info : NULL,
		NULL, NULL
	);

	CloseHandle(file);

	return EXCEPTION_CONTINUE_SEARCH;
} 
