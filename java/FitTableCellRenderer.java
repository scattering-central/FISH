package fish;

import java.awt.Color;
import java.awt.Component;

import javax.swing.JCheckBox;
import javax.swing.JTable;
import javax.swing.table.DefaultTableCellRenderer;

/**
 * Custom table cell renderer for the fitting table.
 * Displays a checkbox if the value is boolean (parameter fixed/refining)
 * otherwise returns the text string to display in the cell as usual.
 * Also colours the cell appropriately.
 * 
 * @author dqr75132
 *
 */
public class FitTableCellRenderer extends DefaultTableCellRenderer {

	public Component getTableCellRendererComponent
	(JTable table, Object value, boolean isSelected,
			boolean hasFocus, int row, int viewcolumn) {
		// this method is passed the view's column index
		// we must convert that to the TableModel index
		int column=table.convertColumnIndexToModel(viewcolumn);
		if(value.getClass().getName().equals("java.lang.Boolean")){
			JCheckBox checkBox=new JCheckBox();
			checkBox.setBackground(Color.white);			
			if((Boolean)value==true) {
				checkBox.setSelected(true);
			}
			else {
				checkBox.setSelected(false);				
			}
			return checkBox;
		}
		else {
			Component cell = super.getTableCellRendererComponent
			(table, value, isSelected, hasFocus, row, column);

			Common_two two_=Core.getTwo_();
			int[] lm=two_.getLm();
			if((column==5 && value.toString().equals("Polydisperse"))
					|| (column==2 && lm[row]==6 && hasPolydisperse(two_,row)) ) {
				cell.setBackground(Color.pink);
			}
			else if(lm[row]==88) {
				cell.setBackground(Color.lightGray);
			}
			else {
				cell.setBackground(Color.white);			
			}
			//cell.setBackground( table.getBackground() );
			cell.setForeground(Color.black);	
			return cell;
		}
	}

	/**
	 * Returns true if the current model has a polydisperse parameter before the
	 * parameter using model 6
	 * @param two_ COMMON TWO
	 * @param row row containing model 6 parameter
	 * @return 	true or false
	 */
	private boolean hasPolydisperse(Common_two two_, int row){
		if(row==0)
			return false;
		float ps[]=two_.getPs();
		for(int i=0;i<row;i++){
			if(ps[i]==-2.0)
				return true;
		}
		return false;
		
	}
}
