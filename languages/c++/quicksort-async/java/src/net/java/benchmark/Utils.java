package net.java.benchmark;

/**
 * Created by IntelliJ IDEA.
 * User: Stas
 * Date: 9/19/11
 * Time: 7:39 PM
 */
public class Utils {
	public static void swap(long[] arr, int idx1, int idx2) {
		long tmp = arr[idx1];
		arr[idx1] = arr[idx2];
		arr[idx2] = tmp;
	}
}
