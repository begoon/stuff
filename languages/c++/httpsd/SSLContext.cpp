#include "SSLContext.h"

namespace monitor {

SSLContext::SSLContext(const char* certfname, const char* keyfname) {
	__ctx = SSL_CTX_new(SSLv23_method());

	SSL_CTX_use_certificate_file(__ctx, certfname, SSL_FILETYPE_PEM);
	SSL_CTX_use_PrivateKey_file(__ctx, keyfname, SSL_FILETYPE_PEM);

	if(!SSL_CTX_check_private_key(__ctx))
		throw Error("Private key verification failed");
}

SSLContext::~SSLContext() {
	SSL_CTX_free(__ctx);	
}

void SSLContext::bootstrap() {
	SSL_library_init();
	SSL_load_error_strings();
	OpenSSL_add_all_algorithms();
}

} // namespace monitor
