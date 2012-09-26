package net.java.benchmark.sort;

import net.java.benchmark.Utils;

/**
 * Created by IntelliJ IDEA.
 * User: Stas
 * Date: 9/19/11
 * Time: 7:39 PM
 */
public class BasicQuicksort implements Sort {
	public void sort(long[] arr) {
		basicQuickSort(arr, 0, arr.length);
	}

	protected static void basicQuickSort(long arr[], int beginIdx, int len) {
		if ( len <= 1 )
			return;

		final int endIdx = beginIdx + len - 1;
		final int pivotIdx = beginIdx+len/2;
		final long pivot = arr[pivotIdx];

		Utils.swap(arr, pivotIdx, endIdx);
		int p = partition(arr, beginIdx, len, pivot);
		Utils.swap(arr, p, endIdx);

		basicQuickSort(arr, beginIdx, p-beginIdx);
		basicQuickSort(arr, p+1,  endIdx-p);
	}

	protected static int partition(long[] arr, int beginIdx, int len, long pivot) {
		 final int endIdx = beginIdx + len - 1;
		 int p = beginIdx;
		 for(int i = beginIdx; i != endIdx; ++i) {
			 if ( arr[i] <= pivot ) {
				 Utils.swap(arr, i, p++);
			 }
		 }
		 return p;
	}
}
