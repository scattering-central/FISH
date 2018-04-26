package fish;

import java.io.IOException;

public class LoqDataSet implements DataSet {
	private String f;
	private String t;
	private String tooltip;
	private int idx;
	private int p;
	private int n;
	private int min=-1;
	private int max=-1;
	
	LoqDataSet(String filename, int index, int pointer, String title,String tooltip){
		f=filename;
		idx=index;
		p=pointer;
		t=title;
		this.tooltip=tooltip;
		n=-1;
	}
	
	public boolean isUsed() {
		if(n==-1){
			return false;
		}
		return true;
	}
	
	public int getSetNumber() {
		return n;
	}

	public void setSetNumber(int number) {
		n=number;
	}

	public String getFilename() {
		return f;
	}

	public String getLabel() {
		return t;
	}

	public void setLabel(String label) {
		t=label;
	}

	public String getToolTip() {
		return tooltip;
	}
	
	public void activate(int dataSet) {
		try {
			System.out.println("Activate data "+f+" "+idx+" into set "+dataSet);
			Common_ch ch_ = Core.getCh_();
			int id=ch_.getId();
			if(Core.setup(id, f) == 0){
				System.out.println("Open file OK");
			}
			else {
				throw new IOException("Error opening data file");
			}
			Core.datin(idx, dataSet);
		}
		catch(Exception e){
			System.out.println(e.getMessage());
		}
	}
	
	public boolean hasErrors(){
		return true;
	}

	public String getQUnit(){
		return defaultQUnit;
	}

	public String getIUnit(){
		return defaultIUnit;
	}
	
	public int getMin(){
		return min;
	}
	
	public int getMax(){
		return max;
	}
	
	public void setLimits(int min,int max){
		this.min=min;
		this.max=max;
	}
}
