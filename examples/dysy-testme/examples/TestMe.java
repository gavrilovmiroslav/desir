
package examples;

public class TestMe {
	public static int testme(int x, int y) {
		int prod = x * y;
		if(prod < 0)
			throw new IllegalArgumentException();
		
		if(x < y) {
			int tmp = x;
			x = y;
			y = tmp;
		}

		int sqry = y * y;
		return prod * prod - sqry * sqry;
	}

	public static void main(String[] args) {
		return;
	}
}

