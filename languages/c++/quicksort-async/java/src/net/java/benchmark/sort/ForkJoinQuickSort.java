package net.java.benchmark.sort;

import net.java.benchmark.Utils;

import java.util.concurrent.*;

/**
 * @author Stas
 * @date 2/20/12
 */
public class ForkJoinQuickSort implements Sort {
	private static final int THRESHOLD = 1024*4;
	private static final int N_THREADS = 8; //Runtime.getRuntime().availableProcessors();
	private ForkJoinPool pool = new ForkJoinPool(N_THREADS);

	public void sort(final long[] arr) {
		pool.invoke(new ForkJoinSortTask(arr, 0, arr.length));
	}

	protected static class ForkJoinSortTask extends RecursiveAction  {
		private final long arr[];
		private final int beginIdx;
		private final int len;

		public ForkJoinSortTask(long[] arr, int beginIdx, int len) {
			this.arr = arr;
			this.beginIdx = beginIdx;
			this.len = len;
		}

		@Override
		protected void compute() {
			basicQuickSort(arr, beginIdx, len);
		}

		protected void basicQuickSort(final long arr[], final int beginIdx, int len) {
			if ( len <= 1 )
				return;

			final int endIdx = beginIdx + len - 1;
			final int pivotIdx = beginIdx+len/2;
			final long pivot = arr[pivotIdx];

			Utils.swap(arr, pivotIdx, endIdx);
			final int p = partition(arr, beginIdx, len, pivot);
			Utils.swap(arr, p, endIdx);

			if ( len >= THRESHOLD ) {
				ForkJoinSortTask lTask = new ForkJoinSortTask(arr, beginIdx, p-beginIdx);
				ForkJoinSortTask rTask = new ForkJoinSortTask(arr, p+1, endIdx-p);
				invokeAll(lTask, rTask);
			} else {
				basicQuickSort(arr, beginIdx, p - beginIdx);
				basicQuickSort(arr, p+1,  endIdx-p);
			}
		}
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
