package fish.plot;

import java.awt.Component;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.JTextField;
import javax.swing.SpinnerNumberModel;

public class SeriesEdit extends JDialog implements ActionListener, ItemListener, WindowListener {

	private static final long serialVersionUID = 1L;

	private JPanel jContentPane = null;

	private JComboBox setList = null;

	private JComboBox typeList = null;

	private JLabel setLabel = null;

	private JLabel typeLabel = null;

	private JLabel keyLabel = null;

	private JTextField keyField = null;

	private JLabel yshiftLabel = null;

	private JCheckBox lineCheckBox = null;

	private JCheckBox shapeCheckBox = null;

	private JCheckBox yerrorCheckBox = null;

	private JSpinner shiftSpinner = null;
	private SpinnerNumberModel shiftSpinnerModel = null;

	private JButton okButton = null;

	private JButton cancelButton = null;
	
	private Series series;
	private SeriesList seriesList;
	
	private boolean added;

	private JCheckBox autoLabelCheckBox = null;

	/**
	 * @param owner
	 */
	public SeriesEdit(SeriesList parent,Series series,boolean added) {
		super(parent,true);
		this.series=series;
		this.seriesList=parent;
		this.added=added;
		initialize();
		setList.setSelectedItem(Integer.toString(series.getSet()));
		typeList.setSelectedIndex(series.getType());
		keyField.setText(series.getKey());
		shiftSpinnerModel.setValue(series.getYshift());
		lineCheckBox.setSelected(series.lines);
		shapeCheckBox.setSelected(series.shapes);
		yerrorCheckBox.setSelected(series.yerror);
		if(typeList.getSelectedIndex()!=0){
			yerrorCheckBox.setEnabled(false);
		}
		autoLabelCheckBox.setSelected(series.autoLabel);
		keyField.setEnabled(!series.autoLabel);
	}

	/**
	 * This method initializes this
	 * 
	 * @return void
	 */
	private void initialize() {
		this.setSize(325, 323);
		this.setTitle("Edit Series");
		this.setContentPane(getJContentPane());
		this.setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);
		this.addWindowListener(this);
	}

	/**
	 * This method initializes jContentPane
	 * 
	 * @return javax.swing.JPanel
	 */
	private JPanel getJContentPane() {
		if (jContentPane == null) {
			GridBagConstraints gridBagConstraints1 = new GridBagConstraints();
			gridBagConstraints1.gridx = 0;
			gridBagConstraints1.gridwidth = 5;
			gridBagConstraints1.anchor = GridBagConstraints.WEST;
			gridBagConstraints1.insets = new Insets(8, 8, 8, 8);
			gridBagConstraints1.gridy = 1;
			GridBagConstraints gridBagConstraints18 = new GridBagConstraints();
			gridBagConstraints18.gridx = 3;
			gridBagConstraints18.insets = new Insets(8, 0, 8, 2);
			gridBagConstraints18.weighty = 0.0;
			gridBagConstraints18.anchor = GridBagConstraints.SOUTH;
			gridBagConstraints18.gridy = 7;
			GridBagConstraints gridBagConstraints17 = new GridBagConstraints();
			gridBagConstraints17.gridx = 4;
			gridBagConstraints17.insets = new Insets(8, 2, 8, 8);
			gridBagConstraints17.fill = GridBagConstraints.NONE;
			gridBagConstraints17.weighty = 0.0;
			gridBagConstraints17.anchor = GridBagConstraints.SOUTH;
			gridBagConstraints17.gridy = 7;
			GridBagConstraints gridBagConstraints16 = new GridBagConstraints();
			gridBagConstraints16.fill = GridBagConstraints.BOTH;
			gridBagConstraints16.gridy = 3;
			gridBagConstraints16.weightx = 1.0;
			gridBagConstraints16.insets = new Insets(8, 8, 8, 8);
			gridBagConstraints16.ipady = 4;
			gridBagConstraints16.gridx = 1;
			GridBagConstraints gridBagConstraints15 = new GridBagConstraints();
			gridBagConstraints15.gridx = 0;
			gridBagConstraints15.gridwidth = 5;
			gridBagConstraints15.anchor = GridBagConstraints.WEST;
			gridBagConstraints15.insets = new Insets(8, 8, 8, 8);
			gridBagConstraints15.gridy = 6;
			GridBagConstraints gridBagConstraints14 = new GridBagConstraints();
			gridBagConstraints14.gridx = 0;
			gridBagConstraints14.gridwidth = 5;
			gridBagConstraints14.anchor = GridBagConstraints.WEST;
			gridBagConstraints14.insets = new Insets(0, 8, 0, 8);
			gridBagConstraints14.gridy = 5;
			GridBagConstraints gridBagConstraints13 = new GridBagConstraints();
			gridBagConstraints13.gridx = 0;
			gridBagConstraints13.gridwidth = 5;
			gridBagConstraints13.anchor = GridBagConstraints.WEST;
			gridBagConstraints13.insets = new Insets(8, 8, 8, 8);
			gridBagConstraints13.gridy = 4;
			GridBagConstraints gridBagConstraints12 = new GridBagConstraints();
			gridBagConstraints12.gridx = 0;
			gridBagConstraints12.anchor = GridBagConstraints.EAST;
			gridBagConstraints12.insets = new Insets(0, 8, 0, 0);
			gridBagConstraints12.gridy = 3;
			yshiftLabel = new JLabel();
			yshiftLabel.setText("Y-shift");
			GridBagConstraints gridBagConstraints11 = new GridBagConstraints();
			gridBagConstraints11.fill = GridBagConstraints.BOTH;
			gridBagConstraints11.gridy = 2;
			gridBagConstraints11.weightx = 1.0;
			gridBagConstraints11.gridwidth = 4;
			gridBagConstraints11.ipadx = 0;
			gridBagConstraints11.insets = new Insets(0, 8, 8, 8);
			gridBagConstraints11.ipady = 4;
			gridBagConstraints11.gridx = 1;
			GridBagConstraints gridBagConstraints10 = new GridBagConstraints();
			gridBagConstraints10.gridx = 0;
			gridBagConstraints10.anchor = GridBagConstraints.EAST;
			gridBagConstraints10.insets = new Insets(0, 8, 8, 0);
			gridBagConstraints10.gridy = 2;
			keyLabel = new JLabel();
			keyLabel.setText("Label");
			GridBagConstraints gridBagConstraints9 = new GridBagConstraints();
			gridBagConstraints9.gridx = 2;
			gridBagConstraints9.anchor = GridBagConstraints.EAST;
			gridBagConstraints9.insets = new Insets(0, 8, 0, 0);
			gridBagConstraints9.gridy = 0;
			typeLabel = new JLabel();
			typeLabel.setText("Variable");
			GridBagConstraints gridBagConstraints8 = new GridBagConstraints();
			gridBagConstraints8.gridx = 0;
			gridBagConstraints8.anchor = GridBagConstraints.EAST;
			gridBagConstraints8.insets = new Insets(0, 8, 0, 0);
			gridBagConstraints8.gridy = 0;
			setLabel = new JLabel();
			setLabel.setText("Data set");
			GridBagConstraints gridBagConstraints7 = new GridBagConstraints();
			gridBagConstraints7.fill = GridBagConstraints.HORIZONTAL;
			gridBagConstraints7.gridy = 0;
			gridBagConstraints7.weightx = 1.0;
			gridBagConstraints7.weighty = 0.0;
			gridBagConstraints7.insets = new Insets(8, 8, 8, 8);
			gridBagConstraints7.gridwidth = 2;
			gridBagConstraints7.ipady = 4;
			gridBagConstraints7.gridx = 3;
			GridBagConstraints gridBagConstraints = new GridBagConstraints();
			gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
			gridBagConstraints.gridy = 0;
			gridBagConstraints.weightx = 1.0;
			gridBagConstraints.weighty = 0.0;
			gridBagConstraints.insets = new Insets(8, 8, 8, 8);
			gridBagConstraints.ipady = 4;
			gridBagConstraints.gridx = 1;
			jContentPane = new JPanel();
			jContentPane.setLayout(new GridBagLayout());
			jContentPane.add(getSetList(), gridBagConstraints);
			jContentPane.add(getTypeList(), gridBagConstraints7);
			jContentPane.add(setLabel, gridBagConstraints8);
			jContentPane.add(typeLabel, gridBagConstraints9);
			jContentPane.add(keyLabel, gridBagConstraints10);
			jContentPane.add(getKeyField(), gridBagConstraints11);
			jContentPane.add(yshiftLabel, gridBagConstraints12);
			jContentPane.add(getLineCheckBox(), gridBagConstraints13);
			jContentPane.add(getShapeCheckBox(), gridBagConstraints14);
			jContentPane.add(getYerrorCheckBox(), gridBagConstraints15);
			jContentPane.add(getShiftSpinner(), gridBagConstraints16);
			jContentPane.add(getOkButton(), gridBagConstraints17);
			jContentPane.add(getCancelButton(), gridBagConstraints18);
			jContentPane.add(getAutoLabelCheckBox(), gridBagConstraints1);
		}
		return jContentPane;
	}

	/**
	 * This method initializes setList	
	 * 	
	 * @return javax.swing.JList	
	 */
	private JComboBox getSetList() {
		if (setList == null) {
			final int maxSets=fish.CoreConstants.MF;
			String[] list=new String[fish.DataList.countSets()];
			int j=0;
			for(int i=0;i<maxSets;i++){
				if(fish.DataList.isSetUsed(i)){
					list[j]=new Integer(i+1).toString();
					j++;
				}
			}
			setList = new JComboBox(list);
		}
		return setList;
	}

	/**
	 * This method initializes typeList	
	 * 	
	 * @return javax.swing.JList	
	 */
	private JComboBox getTypeList() {
		if (typeList == null) {
			final int maxTypes=Series.countTypes();
			String[] list=new String[maxTypes];
			for(int i=0;i<maxTypes;i++){
				list[i]=Series.getTypeDesc(i);
			}
			typeList = new JComboBox(list);
			typeList.addItemListener(this);
		}
		return typeList;
	}

	/**
	 * This method initializes keyField	
	 * 	
	 * @return javax.swing.JTextField	
	 */
	private JTextField getKeyField() {
		if (keyField == null) {
			keyField = new JTextField();
		}
		return keyField;
	}

	/**
	 * This method initializes lineCheckBox	
	 * 	
	 * @return javax.swing.JCheckBox	
	 */
	private JCheckBox getLineCheckBox() {
		if (lineCheckBox == null) {
			lineCheckBox = new JCheckBox();
			lineCheckBox.setText("Draw line");
		}
		return lineCheckBox;
	}

	/**
	 * This method initializes shapeCheckBox	
	 * 	
	 * @return javax.swing.JCheckBox	
	 */
	private JCheckBox getShapeCheckBox() {
		if (shapeCheckBox == null) {
			shapeCheckBox = new JCheckBox();
			shapeCheckBox.setText("Draw symbols");
		}
		return shapeCheckBox;
	}

	/**
	 * This method initializes yerrorCheckBox	
	 * 	
	 * @return javax.swing.JCheckBox	
	 */
	private JCheckBox getYerrorCheckBox() {
		if (yerrorCheckBox == null) {
			yerrorCheckBox = new JCheckBox();
			yerrorCheckBox.setText("Draw Y-error bars");
		}
		return yerrorCheckBox;
	}

	/**
	 * This method initializes shiftSpinner	
	 * 	
	 * @return javax.swing.JTextField	
	 */
	private JSpinner getShiftSpinner() {
		if (shiftSpinner == null) {
			shiftSpinnerModel=new SpinnerNumberModel(0,-100,100,0.5);
			shiftSpinner = new JSpinner(shiftSpinnerModel);
		}
		return shiftSpinner;
	}

	/**
	 * This method initializes okButton	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getOkButton() {
		if (okButton == null) {
			okButton = new JButton();
			okButton.setText("OK");
			okButton.setName("ok");
			okButton.addActionListener(this);
		}
		return okButton;
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

	public void windowActivated(WindowEvent arg0) {}

	public void windowClosed(WindowEvent arg0) {}

	public void windowClosing(WindowEvent arg0) {
		closeWindow();
	}

	public void windowDeactivated(WindowEvent arg0) {}

	public void windowDeiconified(WindowEvent arg0) {}

	public void windowIconified(WindowEvent arg0) {}

	public void windowOpened(WindowEvent arg0) {}

	private void closeWindow() {
		if(added){
			seriesList.plotData.deleteSeries(seriesList.seriesTable.getRowCount()-1);
			seriesList.updatePlot();
		}
		dispose();		
	}
	
	public void actionPerformed(ActionEvent e) {
		String name=((Component)e.getSource()).getName();
		if(name.equals("cancel")) {
			closeWindow();
		}
		else if(name.equals("ok")){
			//int set=valueOf(setList.getSelectedItem().toString()));
			series.setSet(new Integer(setList.getSelectedItem().toString()));
			series.setType(typeList.getSelectedIndex());
			series.setYshift(shiftSpinnerModel.getNumber().intValue());
			series.lines=lineCheckBox.isSelected();
			series.shapes=shapeCheckBox.isSelected();
			series.yerror=yerrorCheckBox.isSelected();
			if(!yerrorCheckBox.isEnabled()){
				series.yerror=false;
			}
			if(autoLabelCheckBox.isSelected()){
				series.autoLabel=true;
				series.setKey(series.getAutoLabel());
			}
			else {
				series.autoLabel=false;
				series.setKey(keyField.getText());
			}
			series.setStyle();
			if(added){
				seriesList.updatePlot();
			}
			seriesList.repaint();
			dispose();			
		}
	}

	public void itemStateChanged(ItemEvent e) {
		Component c=(Component)e.getSource();
		// enable y-error bar checkbox only when OBS is selected
		if(c.equals(typeList)){
			if(typeList.getSelectedIndex()==0){
				yerrorCheckBox.setEnabled(true);
			}
			else{
				yerrorCheckBox.setEnabled(false);
			}
		}
		else if(c.equals(autoLabelCheckBox)){
			keyField.setEnabled(!autoLabelCheckBox.isSelected());
		}
	}

	/**
	 * This method initializes autoLabelCheckBox	
	 * 	
	 * @return javax.swing.JCheckBox	
	 */
	private JCheckBox getAutoLabelCheckBox() {
		if (autoLabelCheckBox == null) {
			autoLabelCheckBox = new JCheckBox();
			autoLabelCheckBox.setText("Auto label");
			autoLabelCheckBox.addItemListener(this);
		}
		return autoLabelCheckBox;
	}

}  //  @jve:decl-index=0:visual-constraint="10,10"
