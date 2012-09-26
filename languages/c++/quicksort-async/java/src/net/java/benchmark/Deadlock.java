package net.java.benchmark;

/**
 * Created by IntelliJ IDEA.
 * User: Stas
 * Date: 12/29/11
 * Time: 7:25 PM
 * To change this template use File | Settings | File Templates.
 */
public class Deadlock {
	static int value;

	static {
		final Thread t = new Thread() {
			@Override
			public void run() {
				value = 1;
			}
		};
		t.start();
		System.out.println("Dead locking");
		try {
			t.join();
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		System.out.println("Released");
	}
	
	public static void main(String args[]) {

	}
}
