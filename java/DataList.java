package fish;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.InputMismatchException;
import java.util.Scanner;
import java.util.Vector;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.table.AbstractTableModel;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.XMLReaderFactory;

/**
 * Manages the list of data sets and provides content for the table in the Data tab
 * @author dqr75132
 *
 */
public class DataList extends AbstractTableModel {
	
	// maximum number of simultaneous active datasets (MF in the FORTRAN code)
	private static final int MAXSETS=CoreConstants.MF;
	
	// array to store the [MAXSETS] active datasets
	private static DataSet[] setNo=new DataSet[MAXSETS];
	
	// vector to store all the datasets shown in the list
	private Vector<DataSet> dataSet=new Vector<DataSet>();
	
	private FishFrame frame;
	
	DataList(FishFrame frame) {
		super();
		this.frame = frame;
	}
	
	/**
	 * Returns the number of datasets seen by FISH
	 * @return number of datasets
	 */
	public int getSize() {
		return dataSet.size();
	}
	
	public int getColumnCount() {
		return 4;
	}

	public int getRowCount() {
		return getSize();
	}

	public Class<?> getColumnClass(int col){
		if(col==0){
			return Boolean.class;
		}
		return super.getColumnClass(col);
	}
	
	public Object getValueAt(int row, int col) {
		if(col==0){
			return dataSet.elementAt(row).isUsed();
		}
		else if(col==1)
		{
			if(dataSet.elementAt(row).isUsed()){
				return dataSet.elementAt(row).getSetNumber();
			}
			else {
				return "";
			}
		}
		else if(col==2){
			return dataSet.elementAt(row).getFilename();
		}
		else if(col==3){
			return dataSet.elementAt(row).getLabel();
		}
		return "";
	}

	/**
	 * Return the tooltip for a cell in the fitting table
	 * @param row row containing pointer
	 * @param col column containing pointer
	 * @return tooltip
	 */
	public String getToolTipAt(int row, int col) {
		if(col==2){
			return getValueAt(row,col).toString();
		}
		if(col==3){
			return dataSet.elementAt(row).getToolTip();
		}
		return null;
	}
	
	public void setValueAt(Object val, int row, int col)
	{
		if(col==0){
			// we need to check the state of the checkbox has actually changed becasse
			// sometimes this method is called even when the state is unchanged.
			// Without checks, this could lead to some sets being wasted
			if((Boolean)val==true && !dataSet.elementAt(row).isUsed()){
				int n=getFreeSetNumber();
				if(n>0)
				{
					dataSet.elementAt(row).setSetNumber(n);
					setNo[n-1]=dataSet.elementAt(row);
					frame.fitInit();
				}
			}
			else if((Boolean)val==false && dataSet.elementAt(row).isUsed()){
				int n=dataSet.elementAt(row).getSetNumber();
				if(n>0){
					setNo[n-1]=null;
					dataSet.elementAt(row).setSetNumber(-1);
				}
			}
			super.fireTableCellUpdated(row, 1);
		}
		if(col==1){
			try {
				int n=Integer.valueOf((String)val);
				int oldn=dataSet.elementAt(row).getSetNumber();
				if(n>0 && n<=MAXSETS && setNo[n-1]==null){
					dataSet.elementAt(row).setSetNumber(n);
					setNo[oldn-1]=null;
					setNo[n-1]=dataSet.elementAt(row);
					frame.fitInit();
					super.fireTableCellUpdated(row,1);
				}
			}
			catch(NumberFormatException e) {
				System.out.println("Incorrect set number input in datafile table");
			}
		}

		if(col==3){
			dataSet.elementAt(row).setLabel((String)val);
			if(dataSet.elementAt(row).isUsed()){
				Core.setlabel((dataSet.elementAt(row).getSetNumber()-1)*10, (String)val,((String)val).length());
			}
		}
	}
	//}

	public boolean isCellEditable(int row, int col) {
		if(col==0 || col==3){
			return true;
		}
		else if(col==1 && dataSet.elementAt(row).isUsed()){
			return true;
		}
		return false;
	}

	public void addDataFile(File file) {
		if(!addRnilsFile(file)) {
			System.out.println("Not an Rnils file, reading as Loq");
			addLoqDataFile(file);
		}
	}
	
	private void addLoqDataFile(File loqFile) {		
		String filename=loqFile.getAbsolutePath().toString();
		try {
			BufferedReader file = new BufferedReader(new FileReader(filename));
			try {
				String line;
				int ptr=0;
				int count=0;
				try {				
					while((line = file.readLine()) != null) {
						String readLine;
						String title=line.trim();
						String tooltip="<html><pre>"+line;
						readLine=file.readLine();
						title+="; "+readLine.trim();
						tooltip+="<br>"+readLine;
						readLine=file.readLine();
						line=readLine.trim();						
						title+="; "+line;
						tooltip+="<br>"+readLine;
						Scanner sc = new Scanner(line);

						int skip=sc.nextInt();
						int tooltipcount=0;
						for(int i=0; i<(skip+2); i++){
							readLine=file.readLine();
							if(tooltipcount<5){
								tooltip+="<br>"+readLine;
							}
							tooltipcount++;
						}
						tooltip+="<br>  [...]</pre></html>";
						dataSet.add(new LoqDataSet(filename,count,ptr,title,tooltip));
						ptr+=skip+5;
						count++;

					}
				} catch(InputMismatchException imex) {
					file.close();
					System.out.println("Not a LOQ file, trying to read as XML");
					addXmlDataFile(loqFile);					
				} catch (NullPointerException npex) {
					file.close();
					System.out.println("Incorrectly formatted .q file? File may contain extra newlines or similar");
				}
			}
			catch(IOException ioex) {
				System.err.println("Could not read from LoqDataFile");
			}
		}
		catch(FileNotFoundException fnf) {
			System.err.println("Could not open LoqDataFile");
		}
	}

	private boolean addRnilsFile(File rnilsFile){
		String filename=rnilsFile.getAbsolutePath().toString();
		Boolean ok = true;
		String line;
		String title="";
		String tooltip="<html><pre>";
		try {
			BufferedReader file = new BufferedReader(new FileReader(filename));
			int count=0;
			
			while(((line = file.readLine()) != null) && count<7) {
				if(count==0) {
					title=line;
				}
				if(count==2 || count==3) {
					Scanner sc = new Scanner(line);
					int val;
					for(int i=0;i<6;i++){
						if(sc.hasNextInt()){
							val=sc.nextInt();
						}
						else {
							ok=false;
						}
					}
				}
				tooltip+=line+"<br>";
				count++;
			}
			tooltip+="  [...]</pre></html>";
		}
		catch(IOException ex) {
			System.err.println("Could not read from RnilsFile");
			ok=false;
		}
		catch(InputMismatchException imex){
			System.err.println("Not an Rnils file");
			ok=false;
		}
		if(ok){
			System.out.println("Loading Rnils file");
			dataSet.add(new RnilsDataSet(filename,title,tooltip));
			return true;
		}
		return false;
	}

	private void addXmlDataFile(File xmlFile){
		String filename=xmlFile.getAbsolutePath().toString();
		try {
			BufferedReader file = new BufferedReader(new FileReader(filename));
			XMLReader xr = XMLReaderFactory.createXMLReader();
			XmlDataListHandler handler = new XmlDataListHandler(filename,dataSet);
			xr.setContentHandler(handler);
			xr.setErrorHandler(handler);
			xr.parse(new InputSource(file));
		}
		catch(IOException ioex) {
			System.err.println("Could not read from XML DataFile");
		}
		catch(SAXException ex) {
			System.out.println("Not an XML file, reading as ASCII");
			addAsciiDataFile(xmlFile);
		}

	
	}
	
	void addAsciiDataFile(File ascFile){
		String filename=ascFile.getAbsolutePath().toString();
		try {
			BufferedReader file = new BufferedReader(new FileReader(filename));
			try {
				String line;
				try {		
					// check if it's a valid Ascii file
					int tooltipcount=0;
					int cols=2;
					float number;
					// skip header line at start of file - currently one 1 supported
					int headers=0;
					boolean start=true;
					String tooltip="<html><pre>";
					while((line = file.readLine()) != null) {
						Scanner sc = new Scanner(line)
							.useDelimiter("\\p{javaWhitespace}+|\\p{javaWhitespace}*,\\p{javaWhitespace}*");

						if(sc.hasNextFloat()) {
							number=sc.nextFloat();
							number=sc.nextFloat();
							if(sc.hasNextFloat()){
								cols=3;
							}
							else{
								cols=2;
							}
						}
						else {
							if(start) {
								headers++;
								start=false;
							}
							else {
								throw(new InputMismatchException());
							}
						}
						if(tooltipcount<5){
							tooltip+=line.replaceAll("\t", " ")+"<br>";
						}
						tooltipcount++;
					}
					tooltip+="  [...]</pre></html>";
					System.out.println("Loading "+cols+"-column Ascii file");
					System.out.println(headers+" header lines skipped");
					dataSet.add(new AsciiDataSet(filename,cols,tooltip,headers));
				} catch(InputMismatchException imex) {
					System.err.println("Not a valid Ascii file");
					file.close();
				}
			}
			catch(IOException ioex) {
				System.err.println("Could not read from Ascii DataFile");
			}
		}
		catch(FileNotFoundException fnf) {
			System.err.println("Could not open Ascii DataFile");
		}
	}
	
	/* The following methods are for the static list of datasets 
	 * that are actually used for the fitting, rather than the list
	 * of all the datasets that have been "seen" by the program
	 */

	/**
	 * Finds a currently unused set number
	 * @return set number, or 0 if none free
	 */
	private static int getFreeSetNumber(){
		for(int i=0;i<MAXSETS;i++){
			if(setNo[i]==null){
				return i+1;
			}
		}
		return 0;
	}

	/**
	 * Returns the number of sets currently in use
	 * @return number of sets
	 */
	public static int countSets(){
		int count=0;
		for(int i=0;i<MAXSETS;i++){
			if(setNo[i]!=null){
				count++;
			}
		}
		return count;
	}	

	/**
	 * Returns the highest set number currently in use
	 * @return max maximum set number
	 */
	public static int maxSet(){
		int max=0;
		for(int i=0;i<MAXSETS;i++){
			if(setNo[i]!=null){
				max=i+1;
			}
		}
		return max;		
	}

	/**
	 * Activates a data set, i.e. loads it as an internal set ready for use 
	 * @param number set number of data set to activate
	 * @param set the internal set into which to load the data
	 */
	public static void activate(int number, int set){
		if(setNo[number]!=null){
			setNo[number].activate(set);
			String label=setNo[number].getLabel();
			try{
				Matcher m = Pattern.compile("^\\s*\\w\\w\\w\\s+\\w\\w\\w\\s+\\d\\d?-\\w\\w\\w-\\d\\d\\d?\\d?\\s+\\d\\d?:\\d\\d(:\\d\\d)?\\s+\\w+:\\s*(.+)$").matcher(label);
				if(m.find()){
					label=m.group(2);
				}
			}
			catch(IllegalStateException ex){}
//			System.out.println("Sample: "+m.group(2));
			Core.setlabel(set,label,label.length());
			
			int min=setNo[number].getMin();
			int max=setNo[number].getMax();
			if(min>-1 || max>-1){
				for(int i=1;i<11;i++){
					Core.range((set-1)*10+i,min,max);
				}
			}
		}
		else {
			// set OBS = -1 is dataset not used
			Common_three three_=Core.getThree_();
			int[] jd=three_.getJd();
			jd[number]=-1;
			three_.setJd(jd);
		}
	}

	/**
	 * Returns whether a data set has errors
	 * @param number set number
	 * @return true or false
	 */
	public static boolean hasErrors(int number){
		if(setNo[number]!=null && !setNo[number].hasErrors()){
			return false;
		}
		return true;
	}
	
	/**
	 * Returns whether a data set is in use
	 * @param number set number
	 * @return true or false
	 */
	public static boolean isSetUsed(int number){
		if(setNo[number]!=null){
			return true;
		}
		return false;
	}

	/**
	 * Returns a currently active data set
	 * @param number set number
	 * @return the dataSet
	 */
	public static DataSet getDataSet(int number){
		return setNo[number];
	}
}