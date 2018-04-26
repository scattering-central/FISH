package fish;

import java.util.Formatter;

import javax.swing.table.AbstractTableModel;

import fish.plot.PlotFrame;

/**
 * Custom table model for the fitting table
 * 
 * @author dqr75132
 *
 */
public class FittingTable extends AbstractTableModel {
	
	private FishFrame frame;
	private static final Common_two two_=Core.getTwo_();
	private static final Common_jfit jfit_=Core.getJfit_();
	
	FittingTable(FishFrame fishFrame) {
		frame=fishFrame;
		
	}
	
	/**
	 * Always returns seven columns as the fitting table always has this number
	 * (although some may be hidden)
	 */
	public int getColumnCount() {
		return 7;
	}

	/**
	 * Returns the row count, which is the number of parameters in the current model
	 */
	public int getRowCount() {
		return two_.getNp();
	}

	/**
	 * Returns whether a cell is editable
	 */
	public boolean isCellEditable(int row, int col){
		float[] ps=two_.getPs();
		int[] lm=two_.getLm();
		if ((col==3 || (col==5 && (ps[row]==0 || ps[row]==1))) && ps[row]>=0 && lm[row]!=88){
			return true;
		}
		else {
			return false;
		}
	}
	
	/**
	 * Find the data value to display in a particular table cell
	 */
	public Object getValueAt(int row, int col) {
		Formatter f = new Formatter();
		int[] lm=two_.getLm();
		if(lm[row]==88 && col>2) {
			f.format("");
		}
		else if(col==0){
			f.format("%d",lm[row]);
		}
		else if(col==1){
			int[] ltyp=two_.getLtyp();
			f.format("%d",ltyp[row]);
		}
		else if(col==2) {
			ModelList models=frame.getModelList();
			Model model=models.getModel();
			String s=model.getParameterLabel(row);
			if(s.equals("            ")){
				int[] ltyp=two_.getLtyp();
				if(ltyp[row]==0){
					f.format("(All sets)",ltyp[row]);
				}
				else{
					f.format("(Set %d)",ltyp[row]);
				}
			}
			else{
				f.format("%s",s);
			}

		}
		else if(col==3) {		
			float[] v=two_.getV();
			f.format("%e", v[row]);			
		}
		else if(col==4) {
			float[] esd=two_.getEsd();
			f.format("%e", esd[row]);			
		}
		else if(col==5) {
			float[] ps=two_.getPs();
			if(ps[row]==-1.0){
				f.format("Constrained");
			}
			else if(ps[row]==-2.0){
				f.format("Polydisperse");
			}
			else if(ps[row]==0){
				return false;
			}
			else if(ps[row]==1){
				return true;
			}
			else {
				f.format("%f", ps[row]);
			}
		}
		else if(col==6) {
			float[] dv=two_.getDv();
			f.format("%e", dv[row]);			
		}		
		
		return f;
	}

	/**
	 * Changes the data value corresponding to a point in the table
	 */
	public void setValueAt(Object val, int row, int col){
		
		if(col==5 && val.getClass().getName().equals("java.lang.Boolean")){
			if((Boolean)val==true){
				float[] ps=two_.getPs();
				ps[row]=1;
				two_.setPs(ps);
			}
			if((Boolean)val==false){
				float[] ps=two_.getPs();
				ps[row]=0;
				two_.setPs(ps);		
			}				
		}
		else if(col==3){
			float[] v=two_.getV();
			try {
				v[row]=Float.valueOf((String)val);
				int inFit=jfit_.getInfit();
				two_.setV(v);
				int cycle=two_.getNyc();
				// if we're not in a fit, set up data sets etc. to enable calculation
				if(inFit==0){
					frame.fitInit();
				}
				// calculate the new CALC data
				Core.talk("OFF");
				Core.talk("R");
				Core.talk("ON");
				// for the moment, we don't count each edit as a separate cycle
				// so decrement the cycle number
				two_.setNyc(cycle);
				// if we weren't already in a fit, set infit back to 0 as it
				// will have been set to 1
				if(inFit==0){
					jfit_.setInfit(0);
				}
				
				PlotFrame.refreshAll(false);
				
			}
			catch(NumberFormatException e) {
				System.out.println("Incorrectly formatted number input in fitting table");
			}
		}
	}
}
