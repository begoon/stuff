package net.java.benchmark.sort;

import java.util.*;

/**
 * Created by IntelliJ IDEA.
 * User: Stas
 * Date: 9/19/11
 * Time: 7:39 PM
 */
public class Benchmark {
	public static int ITERATIONS_NUM = 10;
	public static int DATA_SIZE = 50000000;

	public static void main(String args[]) throws Exception {
		if ( args.length < 1 ) {
			Data data = new Data();
			System.out.println("Running test...");
			//List<Long> results = runBenchmark(data, new BasicQuicksort());
			//List<Long> results = runBenchmark(data, new StandardSort());
			List<Long> results = runBenchmark(data, new ThreadPoolQuickSort());
			System.out.println("Avg test run [" + getAverage(results) + "]ms");
		} else {
			Data.generateFiles(ITERATIONS_NUM, DATA_SIZE);
		}
	}

	public static List<Long> runBenchmark(Data data, Sort sort) throws Exception {
		List<Long> times = new ArrayList<Long>();
		while ( true ) {
			DataBuffer longData = data.getNextBuffer();
			if (longData == null ) {
				break;
			}

			System.out.printf("Running test with long array of size %1$d...", longData.getData().length);
			final long start = System.currentTimeMillis();
	    	sort.sort(longData.getData());
			final long total = System.currentTimeMillis() - start;
			System.out.printf(" done. Test time %1$dms. ", total);

			System.out.print("Verification started... ");
			boolean res = longData.verify();
			System.out.println(" verification " + (res ? "succeeded." : "failed."));

			times.add(total);
		}
		return times;
	}

	public static long getAverage(List<Long> results) {
		long result = -1;

		if ( results.size() > 0 ) {
			int biggestIdx = 0;
			int smallestIdx = 0;
			for(int i = 0; i != results.size(); ++i) {
				if ( results.get(biggestIdx) < results.get(i) ) {
					biggestIdx = i;
				}
				if ( results.get(smallestIdx) > results.get(i) ) {
					smallestIdx = i;
				}
			}

			if ( smallestIdx != biggestIdx ) {
				results.remove(biggestIdx);
				if ( smallestIdx > biggestIdx ) {
					--smallestIdx;
				}
				results.remove(smallestIdx);
			}

			long sum = 0;
			for(Long curr: results) {
				sum += curr;
			}

			result = sum/results.size();
		}

		return result;
	}
}
