package fish;

import java.awt.Component;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;
import javax.swing.border.Border;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

public class SaveData extends JDialog implements ActionListener {

	private static final long serialVersionUID = 1L;

	private JPanel jContentPane = null;

	private JPanel pane = null;

	private JPanel pane2 = null;

	private JCheckBox obsBox = null;

	private JCheckBox calcBox = null;

	private JCheckBox pBox = null;

	private JCheckBox sBox = null;

	private JCheckBox betaBox = null;

	private JPanel buttonsPanel = null;

	private JButton cancelButton = null;

	private JButton nextButton = null;

	private JPanel pane3 = null;

	private JPanel pane4 = null;

	private JSpinner startSet = null;
	private SpinnerNumberModel startSetModel = null;

	private JLabel jLabel = null;

	private JSpinner endSet = null;
	private SpinnerNumberModel endSetModel = null;	

	private JLabel jLabel1 = null;

	private JLabel jLabel2 = null;
	
	final int maxSets=fish.CoreConstants.MF;

	/**
	 * @param owner
	 */
	public SaveData(Frame owner) {
		super(owner,true);
		initialize();
	}

	/**
	 * This method initializes this
	 * 
	 * @return void
	 */
	private void initialize() {
		this.setSize(300, 319);
		this.setTitle("Save Data");
		this.setContentPane(getJContentPane());
	}

	/**
	 * This method initializes jContentPane
	 * 
	 * @return javax.swing.JPanel
	 */
	private JPanel getJContentPane() {
		if (jContentPane == null) {
			GridBagConstraints gridBagConstraints3 = new GridBagConstraints();
			gridBagConstraints3.gridx = 4;
			gridBagConstraints3.gridy = 0;
			GridBagConstraints gridBagConstraints13 = new GridBagConstraints();
			gridBagConstraints13.gridx = 1;
			gridBagConstraints13.gridwidth = 5;
			gridBagConstraints13.anchor = GridBagConstraints.EAST;
			gridBagConstraints13.insets = new Insets(0, 0, 0, 8);
			gridBagConstraints13.gridy = 3;
			GridBagConstraints gridBagConstraints6 = new GridBagConstraints();
			gridBagConstraints6.gridx = 0;
			gridBagConstraints6.gridwidth = 5;
			gridBagConstraints6.fill = GridBagConstraints.HORIZONTAL;
			gridBagConstraints6.gridy = 2;
			jContentPane = new JPanel();
			jContentPane.setLayout(new GridBagLayout());
			jContentPane.add(getPane(), gridBagConstraints6);
			jContentPane.add(getButtonsPanel(), gridBagConstraints13);
			jContentPane.add(getPane3(), gridBagConstraints3);
		}
		return jContentPane;
	}

	/**
	 * This method initializes pane	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getPane() {
		if (pane == null) {
			GridBagConstraints gridBagConstraints7 = new GridBagConstraints();
			gridBagConstraints7.fill = GridBagConstraints.HORIZONTAL;
			gridBagConstraints7.weightx = 1.0;
			gridBagConstraints7.gridwidth = 1;
			pane = new JPanel();
			pane.setLayout(new GridBagLayout());
			Border paneEdge = BorderFactory.createEmptyBorder(8,8,8,8);
			pane.setBorder(paneEdge);
			pane.add(getPane2(), gridBagConstraints7);
			
		}
		return pane;
	}

	/**
	 * This method initializes pane2	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getPane2() {
		if (pane2 == null) {
			GridBagConstraints gridBagConstraints12 = new GridBagConstraints();
			gridBagConstraints12.gridx = 0;
			gridBagConstraints12.anchor = GridBagConstraints.WEST;
			gridBagConstraints12.weightx = 1.0;
			gridBagConstraints12.gridy = 4;
			GridBagConstraints gridBagConstraints11 = new GridBagConstraints();
			gridBagConstraints11.gridx = 0;
			gridBagConstraints11.anchor = GridBagConstraints.WEST;
			gridBagConstraints11.weightx = 1.0;
			gridBagConstraints11.gridy = 3;
			GridBagConstraints gridBagConstraints10 = new GridBagConstraints();
			gridBagConstraints10.gridx = 0;
			gridBagConstraints10.anchor = GridBagConstraints.WEST;
			gridBagConstraints10.weightx = 1.0;
			gridBagConstraints10.gridy = 2;
			GridBagConstraints gridBagConstraints9 = new GridBagConstraints();
			gridBagConstraints9.gridx = 0;
			gridBagConstraints9.anchor = GridBagConstraints.WEST;
			gridBagConstraints9.weightx = 1.0;
			gridBagConstraints9.gridy = 1;
			GridBagConstraints gridBagConstraints8 = new GridBagConstraints();
			gridBagConstraints8.gridx = 0;
			gridBagConstraints8.anchor = GridBagConstraints.WEST;
			gridBagConstraints8.fill = GridBagConstraints.NONE;
			gridBagConstraints8.weightx = 1.0;
			gridBagConstraints8.gridy = 0;
			pane2 = new JPanel();
			pane2.setLayout(new GridBagLayout());
			TitledBorder border = BorderFactory.createTitledBorder("Save Data Type");
			pane2.setBorder(border);
			pane2.add(getObsBox(), gridBagConstraints8);
			pane2.add(getCalcBox(), gridBagConstraints9);
			pane2.add(getPBox(), gridBagConstraints10);
			pane2.add(getSBox(), gridBagConstraints11);
			pane2.add(getBetaBox(), gridBagConstraints12);
		}
		return pane2;
	}

	/**
	 * This method initializes obsBox	
	 * 	
	 * @return javax.swing.JCheckBox	
	 */
	private JCheckBox getObsBox() {
		if (obsBox == null) {
			obsBox = new JCheckBox();
			obsBox.setText("OBS");
		}
		return obsBox;
	}

	/**
	 * This method initializes calcBox	
	 * 	
	 * @return javax.swing.JCheckBox	
	 */
	private JCheckBox getCalcBox() {
		if (calcBox == null) {
			calcBox = new JCheckBox();
			calcBox.setText("CALC");
			calcBox.setSelected(true);
		}
		return calcBox;
	}

	/**
	 * This method initializes pBox	
	 * 	
	 * @return javax.swing.JCheckBox	
	 */
	private JCheckBox getPBox() {
		if (pBox == null) {
			pBox = new JCheckBox();
			pBox.setText("P(Q)");
		}
		return pBox;
	}

	/**
	 * This method initializes sBox	
	 * 	
	 * @return javax.swing.JCheckBox	
	 */
	private JCheckBox getSBox() {
		if (sBox == null) {
			sBox = new JCheckBox();
			sBox.setText("S(Q)");
		}
		return sBox;
	}

	/**
	 * This method initializes betaBox	
	 * 	
	 * @return javax.swing.JCheckBox	
	 */
	private JCheckBox getBetaBox() {
		if (betaBox == null) {
			betaBox = new JCheckBox();
			betaBox.setText("beta(Q)");
		}
		return betaBox;
	}

	/**
	 * This method initializes buttonsPanel	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getButtonsPanel() {
		if (buttonsPanel == null) {
			GridBagConstraints gridBagConstraints15 = new GridBagConstraints();
			gridBagConstraints15.insets = new Insets(0, 0, 8, 0);
			GridBagConstraints gridBagConstraints14 = new GridBagConstraints();
			gridBagConstraints14.gridx = 1;
			gridBagConstraints14.insets = new Insets(0, 8, 8, 0);
			gridBagConstraints14.gridy = 0;
			buttonsPanel = new JPanel();
			buttonsPanel.setLayout(new GridBagLayout());
			buttonsPanel.add(getCancelButton(), gridBagConstraints15);
			buttonsPanel.add(getNextButton(), gridBagConstraints14);
		}
		return buttonsPanel;
	}

	/**
	 * This method initializes cancelButton	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getCancelButton() {
		if (cancelButton == null) {
			cancelButton = new JButton();
			cancelButton.setText("Cancel");
			cancelButton.setName("cancel");
			cancelButton.addActionListener(this);
		}
		return cancelButton;
	}

	/**
	 * This method initializes nextButton	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getNextButton() {
		if (nextButton == null) {
			nextButton = new JButton();
			nextButton.setText("Next >");
			nextButton.setName("next");
			nextButton.addActionListener(this);
		}
		return nextButton;
	}

	/**
	 * This method initializes pane3	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getPane3() {
		if (pane3 == null) {
			pane3 = new JPanel();
			pane3.setLayout(new GridBagLayout());
			Border paneEdge = BorderFactory.createEmptyBorder(8,8,8,8);
			pane3.setBorder(paneEdge);
			pane3.add(getPane4(), new GridBagConstraints());
		}
		return pane3;
	}

	/**
	 * This method initializes pane4	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getPane4() {
		if (pane4 == null) {
			GridBagConstraints gridBagConstraints5 = new GridBagConstraints();
			gridBagConstraints5.gridx = 0;
			gridBagConstraints5.gridwidth = 4;
			gridBagConstraints5.insets = new Insets(8, 8, 8, 8);
			gridBagConstraints5.gridy = 1;
			jLabel2 = new JLabel();
			jLabel2.setText("(unused sets will not be saved)");
			GridBagConstraints gridBagConstraints4 = new GridBagConstraints();
			gridBagConstraints4.gridx = 3;
			gridBagConstraints4.anchor = GridBagConstraints.WEST;
			gridBagConstraints4.gridy = 0;
			jLabel1 = new JLabel();
			jLabel1.setText("inclusive");
			GridBagConstraints gridBagConstraints2 = new GridBagConstraints();
			gridBagConstraints2.fill = GridBagConstraints.NONE;
			gridBagConstraints2.gridy = 0;
			gridBagConstraints2.weightx = 0.0;
			gridBagConstraints2.ipadx = 12;
			gridBagConstraints2.insets = new Insets(8, 8, 8, 8);
			gridBagConstraints2.ipady = 6;
			gridBagConstraints2.gridx = 2;
			GridBagConstraints gridBagConstraints1 = new GridBagConstraints();
			gridBagConstraints1.gridx = 1;
			gridBagConstraints1.gridy = 0;
			jLabel = new JLabel();
			jLabel.setText("to");
			GridBagConstraints gridBagConstraints = new GridBagConstraints();
			gridBagConstraints.fill = GridBagConstraints.NONE;
			gridBagConstraints.gridy = 0;
			gridBagConstraints.weightx = 0.0;
			gridBagConstraints.ipadx = 12;
			gridBagConstraints.insets = new Insets(8, 8, 8, 8);
			gridBagConstraints.ipady = 6;
			gridBagConstraints.gridx = 0;
			pane4 = new JPanel();
			pane4.setLayout(new GridBagLayout());
			TitledBorder border = BorderFactory.createTitledBorder("Save Data Sets");
			pane4.setBorder(border);
			pane4.add(getStartSet(), gridBagConstraints);
			pane4.add(jLabel, gridBagConstraints1);
			pane4.add(getEndSet(), gridBagConstraints2);
			pane4.add(jLabel1, gridBagConstraints4);
			pane4.add(jLabel2, gridBagConstraints5);
		}
		return pane4;
	}

	/**
	 * This method initializes startSet	
	 * 	
	 * @return javax.swing.JTextField	
	 */
	private JSpinner getStartSet() {
		if (startSet == null) {
			startSetModel=new SpinnerNumberModel(1,1,maxSets,1);
			startSet = new JSpinner(startSetModel);
			startSet.addChangeListener(new ChangeListener(){
				public void stateChanged(ChangeEvent e) {
					int startMin=startSetModel.getNumber().intValue();
					endSetModel.setMinimum(startMin);
					if(endSetModel.getNumber().intValue() < startMin){
						endSetModel.setValue(startMin);
					}
				}
			});
		}
		return startSet;
	}

	/**
	 * This method initializes endSet	
	 * 	
	 * @return javax.swing.JTextField	
	 */
	private JSpinner getEndSet() {
		if (endSet == null) {
			endSetModel=new SpinnerNumberModel(maxSets,1,maxSets,1);
			endSet = new JSpinner(endSetModel);
			endSet.addChangeListener(new ChangeListener(){
				public void stateChanged(ChangeEvent e) {
					int endMax=endSetModel.getNumber().intValue();
					startSetModel.setMaximum(endMax);
					if(startSetModel.getNumber().intValue() > endMax){
						startSetModel.setValue(endMax);
					}
				}
			});			
		}
		return endSet;
	}

	public void actionPerformed(ActionEvent e) {
		String name=((Component)e.getSource()).getName();
		if(name.equals("cancel")){
			this.setVisible(false);
		}
		else if(name.equals("next")){
			FishFrame owner = (FishFrame)this.getOwner();
			this.setVisible(false);
			owner.saveFileChooser(startSetModel.getNumber().intValue(),
					endSetModel.getNumber().intValue(),obsBox.isSelected(),
					calcBox.isSelected(),pBox.isSelected(),sBox.isSelected(),betaBox.isSelected());
		}
	}

	public void reset(int start, int end, boolean o, boolean c, boolean p, boolean s, boolean b){
		startSetModel.setValue(start);
		endSetModel.setValue(end);
		obsBox.setSelected(o);
		calcBox.setSelected(c);
		pBox.setSelected(p);
		sBox.setSelected(s);
		betaBox.setSelected(b);
	}
	
	
}  //  @jve:decl-index=0:visual-constraint="10,10"
