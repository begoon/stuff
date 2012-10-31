#ifndef DllExport
# ifdef WIN32
#  define DllExport __declspec(dllexport)
# else
#  define DllExport
# endif
#endif
