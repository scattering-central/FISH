package fish;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.XMLReaderFactory;

public class XmlDataSet extends DefaultHandler implements DataSet {
	private String filename;
	private String title;
	private String tooltip;
	private int idx;
	private boolean hasErrors;
	private String qUnit;
	private String iUnit;
	private int n;
	private int min=-1;
	private int max=-1;
	
	XmlDataSet(String filename, int index, String title,String tooltip,boolean hasErrors,
			String qUnit,String iUnit){
		this.filename=filename;
		this.idx=index;
		this.title=title;
		this.tooltip=tooltip;
		this.hasErrors=hasErrors;
		this.qUnit=qUnit;
		this.iUnit=iUnit;
		this.n=-1;
		System.out.println("Q is in "+qUnit+", I is in "+iUnit);
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
		return filename;
	}

	public String getLabel() {
		return title;
	}

	public void setLabel(String label) {
		title=label;
	}

	public String getToolTip() {
		return tooltip;
	}
	
	public void activate(int dataSet) {
		System.out.println("Activate data "+filename+" "+idx+" into set "+dataSet);
		try {
			BufferedReader file = new BufferedReader(new FileReader(filename));
			XMLReader xr = XMLReaderFactory.createXMLReader();
			XmlDataHandler handler = new XmlDataHandler(idx,dataSet,title);
			xr.setContentHandler(handler);
			xr.setErrorHandler(handler);
			xr.parse(new InputSource(file));
		}
		catch(IOException ioex) {
			System.err.println("Could not read from XML DataFile "+filename);
		}
		catch(SAXException ex) {
			System.out.println("Error while parsing XML file "+filename);
		}
	}
	
	
	
	public boolean hasErrors(){
		return hasErrors;
	}

	public String getQUnit(){
		return qUnit;
	}
	
	public String getIUnit(){
		return iUnit;
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
