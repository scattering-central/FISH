package fish;

import java.util.Vector;

import org.xml.sax.Attributes;
import org.xml.sax.helpers.DefaultHandler;

/**
 * Parses a SAS XML data file for its details to be added to the DataList
 * @author dqr75132
 *
 */
public class XmlDataListHandler extends DefaultHandler {

	private boolean sasEntry = false;
	private boolean inTitle = false;
	private boolean inRun = false;
	private int index = -1;
	private String filename;
	private String title;
	private String run;
	private String tooltip;
	private String qUnit;
	private String iUnit;
	private int tooltipCount;
	private boolean hasErrors;
	private Vector<DataSet> dataSet = null;

	XmlDataListHandler(String filename,Vector<DataSet> dataSet){
		this.dataSet=dataSet;
		this.filename=filename;
	}

	public void startElement(String uri, String name, String qName,
			Attributes atts) {
		if(name.equals("SASentry")){
			sasEntry=true;
			index++;
			run="";
			title="";
			tooltip="<html><pre>";
			tooltipCount=0;
			hasErrors=false;
			qUnit="1/A";
			iUnit="1/cm";
		}
		else if(name.equals("Title")){
			inTitle=true;
		}
		else if(name.equals("Run")){
			inRun=true;
		}
		else if(name.equals("Idev")){
			hasErrors=true;
		}
		else if(name.equals("Q") || name.equals("I")){
			String unit = atts.getValue("unit");
			if(unit!=null){
				if(name.equals("Q")){
					qUnit=unit;
				}
				else if(name.equals("I")){
					iUnit=unit;
				}
			}
		}
	}

	public void endElement(String uri, String name, String qName) {
		if(name.equals("SASentry")){
			if(sasEntry){
				tooltip+=" [...] </pre></html>";
				title=strip(run)+" "+strip(title);
				dataSet.add(new XmlDataSet(filename,index,title,tooltip,hasErrors,
						specialChars(qUnit),specialChars(iUnit)));
			}
			sasEntry=false;
		}		
		else if(name.equals("Title")){
			inTitle=false;
		}
		else if(name.equals("Run")){
			inRun=false;
		}	
	}

	/**
	 * Replaces characters in a unit with special ones (Angstrom, inverse, etc.)
	 * @param unit the string to change
	 * @return string containing special characters
	 */
	private String specialChars(String unit){
		String s=unit;
		if(s.substring(0,2).equals("1/")){
			s=s.substring(2,s.length())+"\u207B\u00b9";
		}
		return s;
	}
	
	/**
	 * Strip spaces from either end of a string
	 * @param string
	 */
	private String strip(String string){
		String s=string;
		char c=32;
		while(s.length()>0 && s.charAt(0)==c){
			//System.out.println("Strip: "+s.length()+" '"+s.substring(0, 1)+"'");
			s=s.substring(1, s.length());
		}
		while(s.length()>0 && s.charAt(s.length()-1)==c){
			s=s.substring(0, s.length()-1);
		}
		return s;
	}
	
	public void characters (char ch[], int start, int length)
	{
		if(inRun){
			run+=String.valueOf(ch).substring(start, start+length);
		}
		else if(inTitle){
			title+=String.valueOf(ch).substring(start, start+length);
		}
		if(sasEntry && tooltipCount<30){
			String c = String.valueOf(ch).substring(start, start+length);
			c.replaceAll("\n", "<br>");
			tooltip+=c;
			tooltipCount++;
		}
	}
}
