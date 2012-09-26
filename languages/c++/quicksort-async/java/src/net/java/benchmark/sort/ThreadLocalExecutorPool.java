package net.java.benchmark.sort;


import java.util.*;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

/**
 * @author Stas
 * @date 2/23/12
 */
public class ThreadLocalExecutorPool {
	private final Map<Long, TaskThread> threadsMap;
	private final TaskThread[] threadsArray;
	
	private final Random rnd = new Random();

	private final ReentrantLock tasksLock = new ReentrantLock();
	private final Condition taskAdded = tasksLock.newCondition();

	public ThreadLocalExecutorPool(int concurrencyLevel) {
		this.threadsMap = new HashMap<> (concurrencyLevel);
		this.threadsArray = new TaskThread[concurrencyLevel];
		for(int i = 0; i != concurrencyLevel; ++i) {
			final TaskThread thread = new TaskThread();
			threadsMap.put(thread.getId(), thread);
			threadsArray[i] = thread;
		}
	}

	public void submitTask(Runnable task) {
		long threadId = Thread.currentThread().getId();
		TaskThread thread = threadsMap.get(threadId);
		if ( thread == null ) {
			thread = threadsArray[rnd.nextInt(threadsArray.length)];
		}

		thread.submitTask(task);
		taskAdded.signalAll();
	}

	public void shutdown() {
		for(int i = 0; i != threadsArray.length; ++i) {
			threadsArray[i].interrupt();
			try {
				threadsArray[i].join(1000);
			} catch (InterruptedException e) {
				// ok, ok
			}
		}
	}

	protected class TaskThread extends Thread {
		private final BlockingQueue<Runnable> tasks = new LinkedBlockingQueue<>();

		@Override
		public void run() {
			try {
				while ( !isInterrupted() ) {
					Runnable task = tasks.poll();
					if ( task == null ) {
						for(int i = 0; i != threadsArray.length; ++i) {
							task = threadsArray[i].steal();
							if ( task != null ) {
								break;
							}
						}
					}

					if ( task != null ) {
						task.run();
					} else {
						taskAdded.await();
					}
				}
			} catch (InterruptedException e) {
				// shutdown
			}
		}

		public void submitTask(Runnable task) {
			tasks.add(task);
		}

		public Runnable steal() {
			return tasks.poll();
		}
	}
}
