package fish;

import java.io.IOException;

public class RnilsDataSet implements DataSet {

	private String filename;
	private String label;
	private String tooltip;
	private int n; //set number
	private int min=-1;
	private int max=-1;
	
	RnilsDataSet(String filename,String label,String tooltip) {
		this.filename=filename;
		this.label=label;
		this.tooltip=tooltip;
		n=-1;
	}
	
	public void activate(int dataSet) {
		try {
			System.out.println("Activate data "+filename+" into set "+dataSet);
			Common_ch ch_ = Core.getCh_();
			int id=ch_.getId();
			if(Core.setup(id, filename) == 0){
				System.out.println("Open file OK");
			}
			else {
				throw new IOException("Error opening data file");
			}
			Core.rnilsill(dataSet);
		}
		catch(Exception e){
			System.out.println(e.getMessage());
		}
	}

	public String getFilename() {
		return filename;
	}

	public String getLabel() {
		return label;
	}

	public int getSetNumber() {
		return n;
	}

	public String getToolTip() {
		return tooltip;
	}

	public boolean isUsed() {
		if(n==-1){
			return false;
		}
		return true;
	}

	public void setLabel(String label) {
		this.label=label;
	}

	public void setSetNumber(int number) {
		n=number;
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
