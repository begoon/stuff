package net.java.benchmark.singleton;

/**
 * Created by IntelliJ IDEA.
 * User: Stas
 * Date: 10/29/11
 * Time: 4:52 PM
 * To change this template use File | Settings | File Templates.
 */
public class SingletonSample {
	public static void main(String[] args) {
		try {
			SingletonTest.getInstance();
		}
		catch (TestException e) {
			System.out.println("1");
		}
		catch (ExceptionInInitializerError e) {
			System.out.println("2");
		}
	}
}
