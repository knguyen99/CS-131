import java.util.concurrent.atomic.*;


class AcmeSafeState implements State{
	private AtomicLongArray value;

	AcmeSafeState(int length) {this.value = new AtomicLongArray(length);}

	public int size() { return value.length(); }

	public long[] current() { 
		int length = this.size();
		long[] temp = new long[length];
		for(int i = 0; i < length; i++)
		{
			temp[i] = this.value.get(i);
		}
		return temp;
	}

	public void swap(int i, int j){
		value.decrementAndGet(i);
		value.incrementAndGet(j);
	}
}
