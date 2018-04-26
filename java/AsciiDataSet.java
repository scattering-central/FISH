package fish;

import java.io.File;
import java.io.IOException;

public class AsciiDataSet implements DataSet {

	private String filename;
	private String label;
	private int columns;
	private String tooltip;
	private int skipHeaders;
	private int n; //set number
	private int min=-1;
	private int max=-1;
	
	AsciiDataSet(String filename,int columns,String tooltip,int skipHeaders) {
		this.filename=filename;
		this.columns=columns;
		this.tooltip=tooltip;
		this.skipHeaders=skipHeaders;
		this.label=(new File(filename).getName())+" ("+(new Integer(columns).toString())+"-column Ascii data)";
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
			Core.datin3(columns, dataSet,skipHeaders);
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
	
	public boolean hasErrors() {
		if(columns==2){
			return false;
		}
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
