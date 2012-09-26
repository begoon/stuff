package net.java.benchmark;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Created by IntelliJ IDEA.
 * User: Stas
 * Date: 2/6/12
 * Time: 9:27 PM
 * To change this template use File | Settings | File Templates.
 */
public class TimePerf {
	public static final int ITERATIONS = 500000000;
	public static void main(String args[]) {
		final int nCPUs = Runtime.getRuntime().availableProcessors();
		final CountDownLatch initLatch = new CountDownLatch(nCPUs);
		final CountDownLatch startLatch = new CountDownLatch(1);
		final CountDownLatch stopLatch = new CountDownLatch(nCPUs);

		final AtomicLong totalMs = new AtomicLong(0);
		for(int i = 0; i != nCPUs; ++i) {
			new Thread() {
				@Override
				public void run() {
					initLatch.countDown();
					try {
						startLatch.await(Long.MAX_VALUE, TimeUnit.DAYS);
					} catch (Exception e) {}
					long start = System.currentTimeMillis();
					doRunMillis();
					//doRunNanos();
					long end = System.currentTimeMillis();
					totalMs.addAndGet(end-start);
					stopLatch.countDown();
				}
			}.start();
		}

		try {
			initLatch.await(Long.MAX_VALUE, TimeUnit.DAYS);
		} catch (Exception e) {}
		startLatch.countDown();
		try {
			stopLatch.await(Long.MAX_VALUE, TimeUnit.DAYS);
		} catch (Exception e) {}
		
		System.out.println("Avarage ms: " +totalMs.get()/nCPUs);
	}
	
	private static void doRunMillis() {
		long val = 0;
//		long start = System.currentTimeMillis();
		for(int i = 0; i != ITERATIONS; ++i) {
			val += System.currentTimeMillis();
		}
		long end = System.currentTimeMillis();
		System.out.println(val);
//		System.out.println("Time millis " + (end-start));
	}

	private static void doRunNanos() {
		long val = 0;
		//long start = System.currentTimeMillis();
		for(int i = 0; i != ITERATIONS; ++i) {
			val += System.nanoTime();
		}
		//long end = System.currentTimeMillis();
		System.out.println(val);
		//System.out.println("Time nanos " + (end-start));
	}
}
