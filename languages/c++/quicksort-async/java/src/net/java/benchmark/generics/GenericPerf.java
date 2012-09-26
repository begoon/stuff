package net.java.benchmark.generics;

import java.util.Random;
import java.util.concurrent.Future;

/**
 * Created by IntelliJ IDEA.
 * User: Stas
 * Date: 11/26/11
 * Time: 10:50 AM
 * To change this template use File | Settings | File Templates.
 */
public class GenericPerf {

	private static Object array[] = new Object[10];

	public static void main(String[] args) {
		funct();
		System.exit(1);

		boolean isLong = false; //System.currentTimeMillis()%2 == 0 ? true: false;
		System.out.println("isLong [" + isLong + "]");

		Random rnd = new Random();
		for(int i = 0; i != array.length; ++i) {
			array[i] = isLong ? (Object)(new Long(rnd.nextLong())): (Object)(new Integer(rnd.nextInt()));
		}

		long val = 0;
		long begin = System.currentTimeMillis();
		for(int i = 0; i != array.length; ++i) {
			val += isLong ? (Long)array[i] : (Integer)array[i];
		}
		long end = System.currentTimeMillis();

		System.out.println("Total time [" + (end-begin) + "]. Val [" + val + "]");
	}

	private static void funct() {
		Object obj = false ? new Long(1): new Integer(1);
		System.out.println("Clazz [" + obj.getClass().getName() + "]");
	}
}
