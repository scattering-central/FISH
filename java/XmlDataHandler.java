package fish;

import org.xml.sax.Attributes;
import org.xml.sax.helpers.DefaultHandler;

/**
 * Parses a SAS XML data file and loads (activates) the data into an internal set
 * @author dqr75132
 *
 */
public class XmlDataHandler extends DefaultHandler {

	private static final Common_one one_ = Core.getOne_();
	private static final int maxRows = CoreConstants.MN;
	private static final fish.SWIGTYPE_p_a_1024__float q=one_.getQ();
	private static final fish.SWIGTYPE_p_a_1024__float c=one_.getC();
	private static final fish.SWIGTYPE_p_a_1024__float e=one_.getE();
	private boolean theData = false;
	private int index;
	private int indexCount = -1;
	private int dataSet;
	private int rowCount;
	private int points=0;
	private String title;
	private String element = "";
	private String chars = "";

	XmlDataHandler(int index,int dataSet,String title){
		this.dataSet=dataSet;
		this.index=index;
		this.title=title;
	}

	public void startElement(String uri, String name, String qName,
			Attributes atts) {
		if(name.equals("SASentry")){
			indexCount++;
		}
		else if(theData){
			element=name;
			if(name.equals("Idata")){
				rowCount++;
			}
			else if(element.equals("Q") || element.equals("I") || element.equals("Idev")){
				chars="";
			}
		}
		else if(name.equals("SASdata") && indexCount==index){
			theData=true;
			rowCount=-1;
		}
	}

	public void endElement(String uri, String name, String qName) {
		if(name.equals("SASdata")){
			if(theData){
				points=rowCount+1;
				theData=false;
			}
		}
		if(theData){
			if(element.equals("Q")){
				Core.q_set(q, dataSet-1, rowCount, Float.parseFloat(chars));
			}
			if(element.equals("I")){
				Core.c_set(c, dataSet-1, rowCount, Float.parseFloat(chars));
			}
			if(element.equals("Idev")){
				Core.e_set(e, dataSet-1, rowCount, Float.parseFloat(chars));
			}
		}
	}

	public void characters (char ch[], int start, int length)
	{
		// Read the data between start and end elements. In most cases, this can be done in one operation.
		// However, sometimes this method is called twice in succession and the two values must be
		// concatenated to give the entire data. This may be due to the length of an internal buffer or
		// something. The data is actually processed above when the end element is reached.
		if(theData){
			if(element.equals("Q") || element.equals("I") || element.equals("Idev")){
				chars+=String.valueOf(ch).substring(start, start+length);
			}
		}
	}
	
    public void endDocument()
    {
    	System.out.println("Requested dataset had "+points+" points");
    	int[] nch=one_.getNch();
       	int[] nc1=one_.getNc1();
       	int[] nc2=one_.getNc2();
       	int[] nmc=one_.getNmc();
       	int[] nc3=one_.getNc3();
       	int[] nc4=one_.getNc4();
       	nch[dataSet-1]=points;
      	nc1[dataSet-1]=0;
      	nc2[dataSet-1]=0;
      	nmc[dataSet-1]=0;
      	nc3[dataSet-1]=1;
      	nc4[dataSet-1]=points;
      	one_.setNch(nch);
      	one_.setNc1(nc1);
      	one_.setNc2(nc2);
      	one_.setNmc(nmc);
      	one_.setNc3(nc3);
      	one_.setNc4(nc4);
      	
      	Core.setlab2(1, dataSet, title, title.length());
      	Core.setlab2(2, dataSet, "", 0);
    }
}
