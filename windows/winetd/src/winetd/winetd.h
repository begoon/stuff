/*
  Winetd main module.
  by Alexander S. Pristenski 2007

  Change log:

  29.01.2007  Alexander S. Pristenski - initial creation.
  20.12.2007  Alexander S. Pristenski - bit changes in service description.
*/

#ifndef _WINETD_H_
#define _WINETD_H_



#define SERVICE_NAME           "Winetd"
#define SERVICE_DISPLAY_NAME   "Winetd"

#define SERVICE_DESCRIPTION_SHORT "Winetd (inetd for Windows) - UNIX's inetd daemon analog for windows.\n" \
                                  "Winetd acts as a control service for other internet services.\n" \
                                  "http://sourceforge.net/projects/winetd "
#define SERVICE__VERSION       "Version 1.0.3"

#define SERVICE_AUTHORS         "Orginally written by Alexandr Pristenskiy\n" \
                                "(<rekod@users.sourceforge.net>, ICQ 150211485)\n" \
                                "Lastest patches by Denis Golovan\n(<denis_golovan@mail.ru>, ICQ 105051989)"

#define SERVICE_DESCRIPTION_EN  SERVICE_DESCRIPTION_SHORT \
                                "When Winetd accepts a connection, it determines for which service this connection belongs, " \
                                "then calls corresponding service and provides a socket to the service. " \
                                "Essentially, allows running one service to invoke several others, " \
                                "reducing load on the system." \
                                SERVICE_AUTHORS

#define SERVICE_DESCRIPTION_RU "Winetd (inetd for Windows) - аналог демона inetd из UNIX. " \
                               "Winetd выступает в качестве управляющего сервиса для других интернет сервисов. " \
                               "Когда Winetd принимает соединение, он определяет, для какого сервиса предназначено соединение, " \
                               "вызывает соответствующий сервис и предоставляет ему сокет. " \
                               "Запуск одного экземпляра Winetd уменьшает общую нагрузку на систему " \
                               "по сравнению с запуском каждого сервиса индивидуально в выделенном режиме. " \
                               "Автор: Александр Сергеевич Пристенский " \
                               "email: <rekod@users.sourceforge.net> " \
                               "ICQ UIN (предпочтительнее): 150211485 " \
                               "Свежие патчи - Денис Головань (denis_golovan@mail.ru, #105051989)"

#define CONFIG_FILE     "winetd.conf"
#define CONFIG_IFACE      "iface"
#define CONFIG_PORT     "port"
#define CONFIG_RUN      "run"
#define CONFIG_ARGS      "args"
#define CONFIG_DESCRIPTION  "description"

#define MAX_STRING 32000 // ~32k

typedef LANGID (* lpGetSystemDefaultUILanguage)(void);

void WINAPI svc_main(DWORD dwArgc, LPTSTR *pszArgv);
void WINAPI svc_handler(DWORD fdwControl);

void __cdecl svc_main_thread(LPVOID tParam);
void __cdecl svc_listen_thread(LPVOID tParam);
void __cdecl svc_rw_thread(LPVOID tParam);

#endif
