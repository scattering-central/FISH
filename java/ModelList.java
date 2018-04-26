package fish;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.InputMismatchException;
import java.util.NoSuchElementException;
import java.util.Scanner;
import java.util.Vector;

import javax.swing.AbstractListModel;

public class ModelList extends AbstractListModel {
	private Vector<Model> model=new Vector<Model>();
	//private int selected=-1;
	private Model currentModel = null;

	/**
	 * Returns the number of models in the list
	 * 
	 * @return
	 */
	public int getSize() {
		return model.size();
	}

	/**
	 * Returns the model at the index passed as a parameter
	 */
	public Object getElementAt(int index) {
		return model.elementAt(index);
	}

	/**
	 * Adds all the models contained in an lsqfile to the model list
	 * 
	 * @param filename
	 */
	public void addlsq(String filename)
	throws IOException, InputMismatchException, NoSuchElementException, IllegalStateException
	{
		try {
			File inpFile = new File(filename);
			BufferedReader file = new BufferedReader(new FileReader(inpFile));
//			try {
			String line;
			int ptr=-1;
			int count=0;
			while((line = file.readLine()) != null) {
				Scanner sc = new Scanner(line);
				ptr++;
				int[] params=new int[5];
				for(int i=0;i<5;i++){
					sc.next();
					params[i]=sc.nextInt();
				}
				file.readLine();
				String title="";
				String tooltip="<html><pre>";
				String readLine="";
				// join all the title lines together into a single line
				for(int i=0;i<params[0];i++)
				{
					if(i>0){
						title+="; ";
						tooltip+="<br>";
					}
					readLine=file.readLine();
					title+=readLine.trim();
					tooltip+=readLine;
				}
				// skip to start of next model
				// 2 lines per Constraint
				// 8 Numerical constraints per line
				int tooltipcount=0;
				int skip=params[1]+params[2]+params[3]*2+(int)((params[4]+7)/8);
				ptr=ptr+params[0]+skip+1;
				for(int i=0;i<skip;i++){
					readLine=file.readLine();
					if(tooltipcount<3){
						tooltip+="<br>"+readLine;
					}
					tooltipcount++;
				}
				tooltip+="<br>  [...]   ("+inpFile.getName()+")</pre></html>";
				model.add(new Lsqmodel(filename,count,ptr,title,tooltip));
				count++;
				//System.out.println(model.size()+" "+count+" "+model.elementAt(model.size()-1).getParameterLabel(0));
			}
			fireIntervalAdded(this, getSize()-count, getSize()-1);
//			}
//			catch(IOException ioex) {
//			System.err.println("Could not read from lsqfile");
//			System.exit(1);
//			}
//			}
//			catch(FileNotFoundException fnf) {
//			System.err.println("Could not open lsqfile");
//			System.exit(1);
		}
		finally {}
	}
	
//	public void addListDataListener(ListDataListener l) {}

//	public void removeListDataListener(ListDataListener l) {}

	/**
	 * Sets the currently selected model
	 * @param index the index of the required model within the model list
	 */
	public void setSelectedIndex(int index) {
		//selected=index;
		currentModel = model.get(index);
	}

	/**
	 * Sets the currently selected model
	 * @param selectModel the model to select
	 */
	public void setSelected(Model selectModel) {
		currentModel = selectModel;
	}
	
	/**
	 * Returns the currently selected model
	 * @return index of the model within the model list
	 */
	public int getSelectedIndex() {
		//return selected;
		return model.indexOf(currentModel);
	}

	/**
	 * Returns the model at the index supplied as an argument
	 * @param index
	 * @return Model
	 */
	public Model getModel(int index) {
		try {		  
			return model.elementAt(index);
		}
		catch (ArrayIndexOutOfBoundsException e) {
			//System.out.println("Model index out of bounds");
			return null;
		}		  
	}

	/**
	 * Returns the currently selected model
	 * @return Model
	 */
	public Model getModel() {
		return currentModel;
	}

	/**
	 * Removes a model from the list
	 * @param index model to remove
	 */
	public void remove(int index) {
		model.remove(index);
		fireIntervalRemoved(this, index, index);
	}
}
