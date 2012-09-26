package net.java.benchmark.sort;

import java.io.FileInputStream;
import java.io.InputStream;
import java.nio.ByteBuffer;

/**
 * @author Stas
 * @date 2/19/12
 */
public class DataBuffer {
	private static final int BUFF_SIZE = 1024*8;
	
	private final long[] data;
	private final String verFileName;

	public DataBuffer(long[] data, String verFileName) {
		this.data = data;
		this.verFileName = verFileName;
	}

	public long[] getData() {
		return data;
	}
	
	public boolean verify() throws Exception {
		boolean isValid = true;
		InputStream in = new FileInputStream(verFileName);
		try {
			byte[] buff = new byte[BUFF_SIZE];
			int dataPos = 0;

			int read;
			while ( isValid & (read = in.read(buff)) != -1 ) {
				ByteBuffer bb = ByteBuffer.wrap(buff, 0, read);
				while ( bb.position() != read ) {
					final long val = bb.getLong();
					if ( val != data[dataPos++] ) {
						isValid = false;
						break;
					}
				}
			}
		} finally {
			in.close();
		}

		return isValid;
	}
}
