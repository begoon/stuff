package net.java.benchmark;

import java.io.*;
import java.util.Scanner;

/**
 * Created by IntelliJ IDEA.
 * User: Stas
 * Date: 12/17/11
 * Time: 3:58 PM
 * To change this template use File | Settings | File Templates.
 */
public class FileBenchmark {
    public static void main(String[] args) throws Exception {
		test1();
		System.out.println();
		test2();
    }

	private static void test1() throws FileNotFoundException {
		long start = System.currentTimeMillis();
		long sum = 0, lines = 0, chars = 0;
		for ( int k = 0; k < 100; k++) {
			Scanner scanner = new Scanner(new FileReader("c:\\temp\\a.txt"));
			while (scanner.hasNextLine()){
				 String s = scanner.nextLine();
				 for ( int i = 0; i < s.length(); i++) {
					 sum += s.charAt ( i );
					 chars++;
				 }
				 lines++;
			}
			scanner.close();
		}
		long end = System.currentTimeMillis();
		System.out.println ( sum );
		System.out.println ( lines );
		System.out.println ( chars );
		System.out.println ( .001* ( end - start) );
	}

	private static void test2() throws Exception {
		long start = System.currentTimeMillis();
		long sum = 0, lines = 0, chars = 0;
		char[] buff = new char[1024];
		StringBuilder sb = new StringBuilder(80);
		for ( int k = 0; k < 100; k++) {
			InputStream is = new BufferedInputStream(new FileInputStream("c://temp//a.txt"), 10240);
			MyReader reader = new MyReader(new InputStreamReader(is));

			int nch;
			boolean eof = false;
			while ((nch = reader.read(buff)) != -1){
				for(int i = 0;i != nch; ++i) {
					final char ch = buff[i];
					if ( isEof(ch) ) {
						eof = true;
					}
				}
			}

			reader.close();
		}
		long end = System.currentTimeMillis();
		System.out.println ( sum );
		System.out.println ( lines );
		System.out.println ( chars );
		System.out.println ( .001* ( end - start) );
	}

	private static boolean isEof(char ch) {
		return ch == '\n' || ch == '\n';
	}

}
