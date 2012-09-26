package net.java.benchmark.sort;

import net.java.benchmark.Utils;

import java.util.concurrent.*;

/**
 * @author Stas
 * @date 2/19/12
 */
public class ThreadPoolQuickSort implements Sort {
	private static final int THRESHOLD = 1024*4;
	private static final int N_THREADS = Runtime.getRuntime().availableProcessors();

	private final BlockingQueue<Runnable> queue = new LinkedBlockingQueue<Runnable>();
	private final ThreadPoolExecutor threadPool = new ThreadPoolExecutor(N_THREADS, N_THREADS, 0L, TimeUnit.MILLISECONDS, queue);

	public void sort(final long[] arr) {
		threadPool.submit(new Runnable() {
			public void run() {
				basicQuickSort(arr, 0, arr.length);
			}
		});

		while (true) {
			if ( queue.size() == 0 && threadPool.getActiveCount() == 0 ) {
				break;
			}
			try {
				Thread.sleep(10);
			} catch (Exception e) {
				// ignore
			}
		}
		
		//System.out.println("Completed tasks: " + threadPool.getCompletedTaskCount());
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

		if ( len >= THRESHOLD) {
			threadPool.submit(new Runnable() {
				public void run() {
					basicQuickSort(arr, beginIdx, p - beginIdx);
				}
			});
			threadPool.submit(new Runnable() {
				public void run() {
					basicQuickSort(arr, p+1,  endIdx-p);
				}
			});
		} else {
			basicQuickSort(arr, beginIdx, p - beginIdx);
			basicQuickSort(arr, p+1,  endIdx-p);
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
