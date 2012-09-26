package net.java.benchmark.singleton;

/**
 * Created by IntelliJ IDEA.
 * User: Stas
 * Date: 10/29/11
 * Time: 4:52 PM
 * To change this template use File | Settings | File Templates.
 */
public class SingletonTest {

	private static class LazyHolder {
                public static final SingletonTest INSTANCE = new SingletonTest();
        }

	public SingletonTest() {
		throw new TestException();
	}

	public static SingletonTest getInstance() {
		return LazyHolder.INSTANCE;
	}
}
