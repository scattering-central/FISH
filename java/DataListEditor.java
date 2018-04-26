package fish;

import javax.swing.DefaultCellEditor;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import fish.plot.PlotData;
import fish.plot.PlotFrame;

/**
 * A subclass of the DefaultCellEditor that allows us to check whether the user meant to change datasets
 * during a fit if the fit has already started.
 * @author dqr75132
 *
 */
public class DataListEditor extends DefaultCellEditor {

	private JPanel panel;
	
	public DataListEditor(JCheckBox arg0,JPanel panel) {
		super(arg0);
		this.panel=panel;
	}

	public DataListEditor(JComboBox arg0,JPanel panel) {
		super(arg0);
		this.panel=panel;
	}

	public DataListEditor(JTextField arg0,JPanel panel) {
		super(arg0);
		this.panel=panel;
	}

	@Override
	public boolean stopCellEditing() {
		Common_jfit jfit_=Core.getJfit_();
		int infit=jfit_.getInfit();
		if(infit!=0){
			// if datasets changed during a fit, make sure the user wanted to do that
			Object[] options = { "Continue", "Cancel" };
			JOptionPane optionPane = new JOptionPane(
					"A fit is in progress. Changing dataset will lose the results\n"+
					"obtained so far. Have you saved the data and/or parameters\n"+
					"as required?\n\n"+
					"Click Cancel if you need to save files or if you wish to\n"+
					"continue the fit with the current model.\n\n"+
					"Click Continue if you wish to start a new fit with different\n"+
					"datasets.",
					JOptionPane.WARNING_MESSAGE,JOptionPane.OK_CANCEL_OPTION,
					null,options,options[1]);
			JDialog message=optionPane.createDialog(panel, "Warning");
			message.setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);
			FishFrame.setCancelDefault(message, optionPane);
			message.setVisible(true);
			Object choice=optionPane.getValue();

			// cancel - resets to original value 
			if(choice.equals(options[1])){
				super.cancelCellEditing();
				return false;
			}
			
			// continue - accept new value and end the fit
			else if(choice.equals(options[0])){
				jfit_.setInfit(0);
			}
		}
//		PlotFrame.refreshAll(true);
		return super.stopCellEditing();
	}
}
