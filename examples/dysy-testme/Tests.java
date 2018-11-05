
import org.junit.Test;
import static org.junit.Assert.*;
import examples.*;

public class Tests {
	@Test(expected = IllegalArgumentException.class) 
	public void testMeBreaking() {
		TestMe.testme(-2, 2);
	}

	@Test
	public void testZero() {
		assertEquals(TestMe.testme(0, 0), 0);
		assertEquals(TestMe.testme(0, 1), 0);
		assertEquals(TestMe.testme(1, 0), 0);
	}

	@Test
	public void testLargerThanZero() {
		assertTrue(TestMe.testme(1, 2) > 0);
		assertTrue(TestMe.testme(2, 5) > 0);
		assertTrue(TestMe.testme(5, 7) > 0);
		assertTrue(TestMe.testme(10, 20) > 0);
	}

	@Test
	public void testEquality() {
		final java.util.Random ints = new java.util.Random();
	
		for(int i = 0; i < 100; i++) {
			final int a = ints.nextInt(100);
			final int b = ints.nextInt(100);
			assertTrue("testme(" + a + ", " + b + ") is commutative.", TestMe.testme(a, b) == TestMe.testme(b, a));

		}
	}
}
