#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <fcntl.h>
#include <io.h>
#include "zlib.h"

#ifdef WIN32
#define ZLIB_MODIFIER 100
#else
#define ZLIB_MODIFIER 1000
#endif

#define OS_CODE			0x03
#define GZIP_HEADER_LENGTH		10
#define GZIP_FOOTER_LENGTH		8

static const int gz_magic[2] = {0x1f, 0x8b};	/* gzip magic header */

static int gunzip_string(const char *src, int src_sz, char** dst)
{
    Byte* s2 = (Byte *)src;
	int status;
	z_stream stream;
    
    if (s2[0] != gz_magic[0] || s2[1] != gz_magic[1] || s2[2] != Z_DEFLATED)
    {
        return Z_DATA_ERROR;
    }
    
    int dst_sz = (s2[src_sz - 1] << 24) | (s2[src_sz - 2] << 16) | (s2[src_sz - 3] << 8) | s2[src_sz - 4];
    
	stream.zalloc = Z_NULL;
	stream.zfree = Z_NULL;
	stream.opaque = Z_NULL;

	stream.next_in = (Bytef *) src + GZIP_HEADER_LENGTH;
	stream.avail_in = src_sz - GZIP_HEADER_LENGTH - GZIP_FOOTER_LENGTH;

	stream.avail_out = dst_sz;
	*dst = (char *) malloc(dst_sz);

	stream.next_out = (Bytef *)*dst;

	/* windowBits is passed < 0 to suppress zlib header & trailer */
	if ((status = inflateInit2(&stream, -MAX_WBITS)) != Z_OK) {
        return status;
	}
	
	status = inflate(&stream, Z_FINISH);
	if (status != Z_STREAM_END) {
		inflateEnd(&stream);
		if (status == Z_OK) {
			status = Z_BUF_ERROR;
		}
	} else {
		status = inflateEnd(&stream);
	}

	if (status != Z_OK)
        return status;
    
	uLong actual_crc = crc32(0L, Z_NULL, 0);
	actual_crc = crc32(actual_crc, (const Bytef *) *dst, dst_sz);
    
    uLong crc = (s2[src_sz - 5] << 24) | (s2[src_sz - 6] << 16) | (s2[src_sz - 7] << 8) | s2[src_sz - 8];
    
    if (crc != actual_crc)
        return Z_DATA_ERROR;
    
    return dst_sz;
}

int main(int argc, char* argv[])
{
    char raw[256*1024];

    char* unpacked;
    
    int raw_sz;
    int unpacked_sz;
    
    setmode(0, _O_BINARY);
    raw_sz = read(0, raw, sizeof raw);
    
    unpacked_sz = gunzip_string(raw, raw_sz, &unpacked);
    if (unpacked_sz < 0) {
        printf("%d\n", unpacked_sz);
        exit(1);
    }

    setmode(1, _O_BINARY);
    write(1, unpacked, unpacked_sz);
    
    free(unpacked);
    
    return 0;
}
