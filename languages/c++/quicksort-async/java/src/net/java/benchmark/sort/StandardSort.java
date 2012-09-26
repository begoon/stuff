package net.java.benchmark.sort;

import net.java.benchmark.sort.Sort;

import java.util.Arrays;

/**
 * Created by IntelliJ IDEA.
 * User: Stas
 * Date: 9/20/11
 * Time: 9:20 PM
 */
public class StandardSort implements Sort {
	public void sort(long[] arr) {
		Arrays.sort(arr);
	}
}
