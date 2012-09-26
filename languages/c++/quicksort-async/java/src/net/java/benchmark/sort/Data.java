package net.java.benchmark.sort;

import java.io.*;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.LongBuffer;
import java.nio.channels.FileChannel;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

/**
 * Created by IntelliJ IDEA.
 * User: Stas
 * Date: 9/20/11
 * Time: 9:28 PM
 */
public class Data {
	public static final String DATA_FILE_NAME_FORMAT = "trash_for_sort_%1$d.bin";
	public static final String VERIF_DATA_FILE_NAME_FORMAT = "trash_for_sort_%1$d_sorted.bin";
	public static final ByteOrder BYTE_ORDER = ByteOrder.BIG_ENDIAN;
	public static final int LONG_BYTES = Long.SIZE/Byte.SIZE;

	public int 		currFileIdx = 0;
	public long[] 	longBuffer 	= null;

	public static void generateFiles(int filesNum, int size) {
		for(int i = 0; i != filesNum; ++i) {
			File dataFile = new File(String.format(DATA_FILE_NAME_FORMAT, i));
			if ( dataFile.exists() && !dataFile.delete() ) {
				throw new RuntimeException("Failed to overwrite file [" + dataFile + "]");
			}
			File verificationFile = new File(String.format(VERIF_DATA_FILE_NAME_FORMAT, i));
			if ( dataFile.exists() && !dataFile.delete() ) {
				throw new RuntimeException("Failed to overwrite file [" + verificationFile + "]");
			}

			System.out.printf("Generating file with random long array of size %1$d...", size);
			long[] data = generateData(size);
			flushDataToFile(dataFile, data);
			System.out.printf(" generated file [%1$s]\n", dataFile.getName());

			System.out.printf("Generating sorted file for data with size %1$d...", size);
			Arrays.sort(data);
			flushDataToFile(verificationFile, data);
			System.out.printf(" generated file [%1$s]\n", verificationFile.getName());
		}
	}

	public DataBuffer getNextBuffer() {
		DataBuffer result = null;

		File dataFile = new File(String.format(DATA_FILE_NAME_FORMAT, currFileIdx));
		if ( dataFile.exists() && dataFile.canRead() ) {
			readDataFromFile(dataFile);
			result = new DataBuffer(longBuffer, String.format(VERIF_DATA_FILE_NAME_FORMAT, currFileIdx));
			++currFileIdx;
		} else {
			longBuffer = null;
			currFileIdx = 0;
		}

		return result;
	}

	private static void flushDataToFile(File file, long[] data) {
		RandomAccessFile out = null;
		FileChannel channel = null;
		try{
			out = new RandomAccessFile(file, "rw");
			channel = out.getChannel();
			ByteBuffer buffer = channel.map(FileChannel.MapMode.READ_WRITE, 0, LONG_BYTES * data.length);
			buffer.order(BYTE_ORDER);
			LongBuffer lb = buffer.asLongBuffer();
			lb.put(data);
		} catch (Exception e) {
			throw new RuntimeException("Sorry, sh*t happened while trying to write into the [" + file + "]", e);
		} finally {
			try { if ( channel != null ) {channel.close();} } catch (Exception e) {
				// ignore
			}
			try { if ( out != null ) {out.close();} } catch (Exception e) {
				// ignore
			}
		}
	}

	private void readDataFromFile(File file) {
		FileInputStream in = null;
		FileChannel channel = null;
		try {
			in = new FileInputStream(file);
			channel = in.getChannel();
			ByteBuffer buffer = channel.map(FileChannel.MapMode.READ_ONLY, 0, file.length());
			buffer.order(BYTE_ORDER);
			LongBuffer lb = buffer.asLongBuffer();

			final int reqSize = (int) file.length() / LONG_BYTES;
			if ( longBuffer == null || reqSize != longBuffer.length) {
				longBuffer = new long[reqSize];
			}

			lb.get(longBuffer);
		} catch (Exception e) {
			throw new RuntimeException("Sorry, sh*t happened while trying to read file [" + file + "]", e);
		} finally {
			try { if ( channel != null ) {channel.close();} } catch (Exception e) {
				// ignore
			}
			try { if ( in != null ) {in.close();} } catch (Exception e) {
				// ignore
			}
		}
	}

	private static long[] generateData(int size) {
		final long[] result = new long[size];
		final Random rnd = new Random();
		for(int i = 0; i != result.length; ++i) {
			result[i] = rnd.nextLong();
		}
		return result;
	}
}
