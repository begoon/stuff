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

static int gzip_string(const char *src, int src_sz, char** dst)
{
    char* s2;
	long level = Z_DEFAULT_COMPRESSION;
	int status;
	z_stream stream;
    
    int dst_sz = -1;

	stream.zalloc = Z_NULL;
	stream.zfree = Z_NULL;
	stream.opaque = Z_NULL;

	stream.next_in = (Bytef *) src;
	stream.avail_in = src_sz;

	stream.avail_out = stream.avail_in + (stream.avail_in / ZLIB_MODIFIER) + 15 + 1; /* room for \0 */
	s2 = (char *) malloc(stream.avail_out + GZIP_HEADER_LENGTH + GZIP_FOOTER_LENGTH);

	/* add gzip file header */
	s2[0] = gz_magic[0];
	s2[1] = gz_magic[1];
	s2[2] = Z_DEFLATED;
	s2[3] = s2[4] = s2[5] = s2[6] = s2[7] = s2[8] = 0; /* time set to 0 */
	s2[9] = OS_CODE;

	stream.next_out = (Bytef *)&(s2[GZIP_HEADER_LENGTH]);

	/* windowBits is passed < 0 to suppress zlib header & trailer */
	if ((status = deflateInit2(&stream, level, Z_DEFLATED, -MAX_WBITS, MAX_MEM_LEVEL, Z_DEFAULT_STRATEGY)) != Z_OK) {
        return status;
	}
	
	status = deflate(&stream, Z_FINISH);
	if (status != Z_STREAM_END) {
		deflateEnd(&stream);
		if (status == Z_OK) {
			status = Z_BUF_ERROR;
		}
	} else {
		status = deflateEnd(&stream);
	}

	if (status == Z_OK) {
        char* trailer;
        uLong crc;
        
		/* resize to buffer to the "right" size */
		s2 = (char *)realloc(s2, stream.total_out + GZIP_HEADER_LENGTH + GZIP_FOOTER_LENGTH + 1);

		trailer = s2 + (stream.total_out + GZIP_HEADER_LENGTH);
		crc = crc32(0L, Z_NULL, 0);
		crc = crc32(crc, (const Bytef *) src, src_sz);
        
		/* write crc & stream.total_in in LSB order */
		trailer[0] = (char) crc & 0xFF;
		trailer[1] = (char) (crc >> 8) & 0xFF;
		trailer[2] = (char) (crc >> 16) & 0xFF;
		trailer[3] = (char) (crc >> 24) & 0xFF;
		trailer[4] = (char) stream.total_in & 0xFF;
		trailer[5] = (char) (stream.total_in >> 8) & 0xFF;
		trailer[6] = (char) (stream.total_in >> 16) & 0xFF;
		trailer[7] = (char) (stream.total_in >> 24) & 0xFF;
		trailer[8] = '\0';
		
        dst_sz = stream.total_out + GZIP_HEADER_LENGTH + GZIP_FOOTER_LENGTH;
        *dst = s2;
        
	} else {
		free(s2);
	}
    return dst_sz;
}

int main(int argc, char* argv[])
{
    char raw[256*1024];

    char* packed;
    
    int raw_sz;
    int packed_sz;
    
    setmode(0, _O_BINARY);
    raw_sz = read(0, raw, sizeof raw);
    
    packed_sz = gzip_string(raw, raw_sz, &packed);
    if (packed_sz < 0) {
        printf("%d\n", packed_sz);
        exit(1);
    }

    setmode(1, _O_BINARY);
    write(1, packed, packed_sz);
    
    free(packed);
    
    return 0;
}
