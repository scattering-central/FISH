package fish;

import java.awt.Component;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JSpinner;
import javax.swing.JTextField;
import javax.swing.SpinnerNumberModel;
import javax.swing.border.Border;
import javax.swing.border.TitledBorder;

public class Constants extends JFrame implements ActionListener {

	private static final long serialVersionUID = 1L;

	private FishFrame owner;
	
	private JPanel jContentPane = null;

	private JLabel label1 = null;

	private JTextField field1 = null;

	private JLabel label2 = null;

	private JTextField field2 = null;

	private JLabel label3 = null;

	private JLabel label4 = null;

	private JLabel label5 = null;

	private JTextField field3 = null;

	private JTextField field4 = null;

	private JTextField field5 = null;

	private JLabel text2 = null;

	private JLabel text3 = null;

	private JLabel text4 = null;

	private JLabel text5 = null;

	private JButton okButton = null;

	private JButton cancelButton = null;

	private JPanel weightsPanel = null;

	private JRadioButton weightButton1 = null;

	private JRadioButton weightButton2 = null;

	private JRadioButton weightButton3 = null;

	private JLabel weightLabel = null;

	private JLabel fitLabel = null;

	private JPanel fitPanel = null;

	private JRadioButton fitButton1 = null;

	private JRadioButton fitButton2 = null;

	private JCheckBox predicateBox = null;

	private JCheckBox betaBox = null;

	private JLabel missLabel = null;

	private JSpinner missSpinner = null;
	private SpinnerNumberModel missSpinnerModel = null;

	private JLabel missLabel2 = null;

	private static final String[] label = {
		"\u03bb",
		"V\u2080",
		"\u03C3<sub>r</sub>&nbsp;/&nbsp;rbar",
		"\u0394r",
		"r<sub>max</sub>"
	};


	/**
	 * @param owner
	 */
	public Constants(Frame owner) {
		initialize();
	}

	/**
	 * This method initializes this
	 * 
	 * @return void
	 */
	private void initialize() {
		this.setSize(349, 484);
		this.setTitle("Fit Constants and Flags");
		this.setIconImage(Toolkit.getDefaultToolkit().getImage("bin/fish/fish.png"));
		this.setContentPane(getJContentPane());
		this.setDefaultCloseOperation(JDialog.HIDE_ON_CLOSE);
	}

	private JSpinner getMissSpinner() {
		if (missSpinner == null) {
			missSpinnerModel=new SpinnerNumberModel(1,1,10000,1);
			missSpinner = new JSpinner(missSpinnerModel);
		}
		return missSpinner;
	}
	
	/**
	 * This method initializes jContentPane
	 * 
	 * @return javax.swing.JPanel
	 */
	private JPanel getJContentPane() {
		if (jContentPane == null) {
			GridBagConstraints gridBagConstraints26 = new GridBagConstraints();
			gridBagConstraints26.insets = new Insets(0, 0, 0, 0);
			gridBagConstraints26.gridy = 4;
			gridBagConstraints26.ipadx = 0;
			gridBagConstraints26.ipady = 0;
			gridBagConstraints26.fill = GridBagConstraints.NONE;
			gridBagConstraints26.anchor = GridBagConstraints.EAST;
			gridBagConstraints26.gridx = 0;
			GridBagConstraints gridBagConstraints25 = new GridBagConstraints();
			gridBagConstraints25.gridx = 0;
			gridBagConstraints25.ipadx = 325;
			gridBagConstraints25.gridy = 3;
			GridBagConstraints gridBagConstraints24 = new GridBagConstraints();
			gridBagConstraints24.gridx = 0;
			gridBagConstraints24.ipadx = 0;
			gridBagConstraints24.ipady = 0;
			gridBagConstraints24.fill = GridBagConstraints.HORIZONTAL;
			gridBagConstraints24.gridy = 2;
			GridBagConstraints gridBagConstraints23 = new GridBagConstraints();
			gridBagConstraints23.gridx = 0;
			gridBagConstraints23.ipadx = 325;
			gridBagConstraints23.gridy = 1;
			GridBagConstraints gridBagConstraints221 = new GridBagConstraints();
			gridBagConstraints221.gridx = 0;
			gridBagConstraints221.ipadx = 0;
			gridBagConstraints221.ipady = 0;
			gridBagConstraints221.fill = GridBagConstraints.HORIZONTAL;
			gridBagConstraints221.gridy = 0;
			GridBagConstraints gridBagConstraints211 = new GridBagConstraints();
			gridBagConstraints211.insets = new Insets(0, 8, 0, 0);
			gridBagConstraints211.gridy = 0;
			gridBagConstraints211.anchor = GridBagConstraints.EAST;
			gridBagConstraints211.fill = GridBagConstraints.HORIZONTAL;
			gridBagConstraints211.gridx = 1;
			GridBagConstraints gridBagConstraints20 = new GridBagConstraints();
			gridBagConstraints20.insets = new Insets(0, 0, 0, 0);
			gridBagConstraints20.gridy = 0;
			gridBagConstraints20.fill = GridBagConstraints.NONE;
			gridBagConstraints20.anchor = GridBagConstraints.CENTER;
			gridBagConstraints20.gridx = 0;
			GridBagConstraints gridBagConstraints17 = new GridBagConstraints();
			gridBagConstraints17.gridx = 2;
			gridBagConstraints17.anchor = GridBagConstraints.WEST;
			gridBagConstraints17.insets = new Insets(0, 4, 8, 0);
			gridBagConstraints17.gridy = 4;
			missLabel2 = new JLabel();
			missLabel2.setText("data points");
			GridBagConstraints gridBagConstraints141 = new GridBagConstraints();
			gridBagConstraints141.gridx = 0;
			gridBagConstraints141.insets = new Insets(0, 0, 8, 0);
			gridBagConstraints141.gridy = 4;
			missLabel = new JLabel();
			missLabel.setText("Use every");
			GridBagConstraints gridBagConstraints998 = new GridBagConstraints();
			gridBagConstraints998.gridx = 1;
			gridBagConstraints998.insets = new Insets(0, 8, 8, 0);
			gridBagConstraints998.fill = GridBagConstraints.NONE;
			gridBagConstraints998.anchor = GridBagConstraints.WEST;
			gridBagConstraints998.ipady = 4;
			gridBagConstraints998.gridy = 4;
			GridBagConstraints gridBagConstraints131 = new GridBagConstraints();
			gridBagConstraints131.gridx = 1;
			gridBagConstraints131.anchor = GridBagConstraints.WEST;
			gridBagConstraints131.gridwidth = 2;
			gridBagConstraints131.insets = new Insets(0, 8, 4, 0);
			gridBagConstraints131.gridy = 3;
			GridBagConstraints gridBagConstraints121 = new GridBagConstraints();
			gridBagConstraints121.gridx = 1;
			gridBagConstraints121.gridwidth = 2;
			gridBagConstraints121.anchor = GridBagConstraints.WEST;
			gridBagConstraints121.insets = new Insets(0, 8, 4, 0);
			gridBagConstraints121.gridy = 2;
			GridBagConstraints gridBagConstraints112 = new GridBagConstraints();
			gridBagConstraints112.gridx = 1;
			gridBagConstraints112.anchor = GridBagConstraints.WEST;
			gridBagConstraints112.gridwidth = 2;
			gridBagConstraints112.insets = new Insets(0, 8, 4, 0);
			gridBagConstraints112.gridy = 1;
			GridBagConstraints gridBagConstraints101 = new GridBagConstraints();
			gridBagConstraints101.gridx = 0;
			gridBagConstraints101.anchor = GridBagConstraints.EAST;
			gridBagConstraints101.insets = new Insets(0, 0, 4, 0);
			gridBagConstraints101.gridy = 1;
			fitLabel = new JLabel();
			fitLabel.setText("Fit type");
			GridBagConstraints gridBagConstraints91 = new GridBagConstraints();
			gridBagConstraints91.gridx = 0;
			gridBagConstraints91.anchor = GridBagConstraints.EAST;
			gridBagConstraints91.insets = new Insets(4, 0, 8, 0);
			gridBagConstraints91.gridy = 0;
			weightLabel = new JLabel();
			weightLabel.setText("Weights");
			GridBagConstraints gridBagConstraints41 = new GridBagConstraints();
			gridBagConstraints41.anchor = GridBagConstraints.WEST;
			gridBagConstraints41.gridy = -1;
			gridBagConstraints41.gridx = 3;
			GridBagConstraints gridBagConstraints14 = new GridBagConstraints();
			gridBagConstraints14.gridx = 1;
			gridBagConstraints14.anchor = GridBagConstraints.WEST;
			gridBagConstraints14.fill = GridBagConstraints.NONE;
			gridBagConstraints14.gridwidth = 2;
			gridBagConstraints14.ipady = 0;
			gridBagConstraints14.insets = new Insets(4, 8, 8, 0);
			gridBagConstraints14.gridy = 0;
			GridBagConstraints gridBagConstraints22 = new GridBagConstraints();
			gridBagConstraints22.gridx = 1;
			gridBagConstraints22.anchor = GridBagConstraints.EAST;
			gridBagConstraints22.insets = new Insets(24, 0, 8, 8);
			gridBagConstraints22.gridy = 5;
			GridBagConstraints gridBagConstraints13 = new GridBagConstraints();
			gridBagConstraints13.gridx = 2;
			gridBagConstraints13.insets = new Insets(24, 0, 8, 8);
			gridBagConstraints13.anchor = GridBagConstraints.WEST;
			gridBagConstraints13.fill = GridBagConstraints.HORIZONTAL;
			gridBagConstraints13.gridy = 5;
			GridBagConstraints gridBagConstraints12 = new GridBagConstraints();
			gridBagConstraints12.gridx = 2;
			gridBagConstraints12.insets = new Insets(4, 0, 8, 8);
			gridBagConstraints12.gridy = 4;
			text5 = new JLabel();
			text5.setText("(model 6)");
			GridBagConstraints gridBagConstraints111 = new GridBagConstraints();
			gridBagConstraints111.gridx = 2;
			gridBagConstraints111.insets = new Insets(4, 0, 8, 8);
			gridBagConstraints111.gridy = 3;
			text4 = new JLabel();
			text4.setText("(model 6)");
			GridBagConstraints gridBagConstraints10 = new GridBagConstraints();
			gridBagConstraints10.gridx = 2;
			gridBagConstraints10.insets = new Insets(4, 0, 8, 8);
			gridBagConstraints10.gridy = 2;
			text3 = new JLabel();
			text3.setText("(model 5)");
			GridBagConstraints gridBagConstraints9 = new GridBagConstraints();
			gridBagConstraints9.gridx = 2;
			gridBagConstraints9.insets = new Insets(4, 0, 8, 8);
			gridBagConstraints9.gridy = 1;
			text2 = new JLabel();
			text2.setText("(model 5)");
			GridBagConstraints gridBagConstraints7 = new GridBagConstraints();
			gridBagConstraints7.fill = GridBagConstraints.HORIZONTAL;
			gridBagConstraints7.gridy = 4;
			gridBagConstraints7.weightx = 1.0;
			gridBagConstraints7.insets = new Insets(4, 8, 8, 8);
			gridBagConstraints7.ipady = 4;
			gridBagConstraints7.gridx = 1;
			GridBagConstraints gridBagConstraints6 = new GridBagConstraints();
			gridBagConstraints6.fill = GridBagConstraints.HORIZONTAL;
			gridBagConstraints6.gridy = 3;
			gridBagConstraints6.weightx = 1.0;
			gridBagConstraints6.insets = new Insets(4, 8, 8, 8);
			gridBagConstraints6.ipady = 4;
			gridBagConstraints6.gridx = 1;
			GridBagConstraints gridBagConstraints5 = new GridBagConstraints();
			gridBagConstraints5.fill = GridBagConstraints.HORIZONTAL;
			gridBagConstraints5.gridy = 2;
			gridBagConstraints5.weightx = 1.0;
			gridBagConstraints5.insets = new Insets(4, 8, 8, 8);
			gridBagConstraints5.ipady = 4;
			gridBagConstraints5.gridx = 1;
			GridBagConstraints gridBagConstraints4 = new GridBagConstraints();
			gridBagConstraints4.gridx = 0;
			gridBagConstraints4.insets = new Insets(4, 8, 8, 0);
			gridBagConstraints4.anchor = GridBagConstraints.EAST;
			gridBagConstraints4.gridy = 4;
			label5 = new JLabel();
			label5.setText("<html>"+label[4]+"</html>");
			GridBagConstraints gridBagConstraints3 = new GridBagConstraints();
			gridBagConstraints3.gridx = 0;
			gridBagConstraints3.insets = new Insets(4, 8, 8, 0);
			gridBagConstraints3.anchor = GridBagConstraints.EAST;
			gridBagConstraints3.gridy = 3;
			label4 = new JLabel();
			label4.setText("<html>"+label[3]+"</html>");
			GridBagConstraints gridBagConstraints21 = new GridBagConstraints();
			gridBagConstraints21.gridx = 0;
			gridBagConstraints21.insets = new Insets(4, 8, 8, 0);
			gridBagConstraints21.anchor = GridBagConstraints.EAST;
			gridBagConstraints21.gridy = 2;
			label3 = new JLabel();
			label3.setText("<html>"+label[2]+"</html>");
			GridBagConstraints gridBagConstraints11 = new GridBagConstraints();
			gridBagConstraints11.fill = GridBagConstraints.HORIZONTAL;
			gridBagConstraints11.gridy = 1;
			gridBagConstraints11.weightx = 1.0;
			gridBagConstraints11.insets = new Insets(4, 8, 8, 8);
			gridBagConstraints11.ipady = 4;
			gridBagConstraints11.gridx = 1;
			GridBagConstraints gridBagConstraints2 = new GridBagConstraints();
			gridBagConstraints2.gridx = 0;
			gridBagConstraints2.insets = new Insets(4, 8, 8, 0);
			gridBagConstraints2.anchor = GridBagConstraints.EAST;
			gridBagConstraints2.gridy = 1;
			label2 = new JLabel();
			label2.setText("<html>"+label[1]+"</html>");
			GridBagConstraints gridBagConstraints1 = new GridBagConstraints();
			gridBagConstraints1.fill = GridBagConstraints.HORIZONTAL;
			gridBagConstraints1.gridy = 0;
			gridBagConstraints1.weightx = 1.0;
			gridBagConstraints1.ipadx = 0;
			gridBagConstraints1.ipady = 4;
			gridBagConstraints1.insets = new Insets(8, 8, 8, 8);
			gridBagConstraints1.gridx = 1;
			GridBagConstraints gridBagConstraints = new GridBagConstraints();
			gridBagConstraints.insets = new Insets(4, 8, 8, 0);
			gridBagConstraints.gridy = 0;
			gridBagConstraints.ipadx = 0;
			gridBagConstraints.anchor = GridBagConstraints.EAST;
			gridBagConstraints.gridx = 0;
			label1 = new JLabel();
			label1.setText("<html>"+label[0]+"</html>");
			TitledBorder border = BorderFactory.createTitledBorder("Numerical constants");
			jContentPane = new JPanel();
			Border paneEdge = BorderFactory.createEmptyBorder(8,8,8,8);
			jContentPane.setBorder(paneEdge);
			jContentPane.setLayout(new GridBagLayout());
			JPanel pane1 = new JPanel();
			pane1.setBorder(border);
			pane1.setLayout(new GridBagLayout());
			pane1.add(label1, gridBagConstraints);
			pane1.add(getField1(), gridBagConstraints1);
			pane1.add(label2, gridBagConstraints2);
			pane1.add(getField2(), gridBagConstraints11);
			pane1.add(label3, gridBagConstraints21);
			pane1.add(label4, gridBagConstraints3);
			pane1.add(label5, gridBagConstraints4);
			pane1.add(getField3(), gridBagConstraints5);
			pane1.add(getField4(), gridBagConstraints6);
			pane1.add(getField5(), gridBagConstraints7);
			pane1.add(text2, gridBagConstraints9);
			pane1.add(text3, gridBagConstraints10);
			pane1.add(text4, gridBagConstraints111);
			pane1.add(text5, gridBagConstraints12);
			JPanel pane2 = new JPanel();
			border = BorderFactory.createTitledBorder("Control flags");
			pane2.setBorder(border);
			pane2.setLayout(new GridBagLayout());
			pane2.add(getWeightsPanel(), gridBagConstraints14);
			pane2.add(weightLabel, gridBagConstraints91);
			pane2.add(fitLabel, gridBagConstraints101);
			pane2.add(getFitPanel(), gridBagConstraints112);
			pane2.add(getPredicateBox(), gridBagConstraints121);
			pane2.add(getBetaBox(), gridBagConstraints131);
			pane2.add(missLabel, gridBagConstraints141);
			pane2.add(getMissSpinner(), gridBagConstraints998);
			pane2.add(missLabel2, gridBagConstraints17);
			JPanel pane3 = new JPanel();
			pane3.setLayout(new GridBagLayout());
			pane3.add(getCancelButton(), gridBagConstraints20);
			pane3.add(getOkButton(), gridBagConstraints211);
			Component box = Box.createVerticalStrut(8);
			jContentPane.add(pane1, gridBagConstraints221);
			jContentPane.add(box, gridBagConstraints23);
			box = Box.createVerticalStrut(8);
			jContentPane.add(pane2, gridBagConstraints24);
			jContentPane.add(box, gridBagConstraints25);
			jContentPane.add(pane3, gridBagConstraints26);
		}
		return jContentPane;
	}

	/**
	 * This method initializes field1	
	 * 	
	 * @return javax.swing.JTextField	
	 */
	private JTextField getField1() {
		if (field1 == null) {
			field1 = new JTextField();
		}
		return field1;
	}

	/**
	 * This method initializes field2	
	 * 	
	 * @return javax.swing.JTextField	
	 */
	private JTextField getField2() {
		if (field2 == null) {
			field2 = new JTextField();
		}
		return field2;
	}

	/**
	 * This method initializes field3	
	 * 	
	 * @return javax.swing.JTextField	
	 */
	private JTextField getField3() {
		if (field3 == null) {
			field3 = new JTextField();
		}
		return field3;
	}

	/**
	 * This method initializes field4	
	 * 	
	 * @return javax.swing.JTextField	
	 */
	private JTextField getField4() {
		if (field4 == null) {
			field4 = new JTextField();
		}
		return field4;
	}

	/**
	 * This method initializes field5	
	 * 	
	 * @return javax.swing.JTextField	
	 */
	private JTextField getField5() {
		if (field5 == null) {
			field5 = new JTextField();
		}
		return field5;
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
	 * This is now the Apply button, but was originally a Cancel button!	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getCancelButton() {
		if (cancelButton == null) {
			cancelButton = new JButton();
			cancelButton.setText("Apply");
			cancelButton.setName("apply");
			cancelButton.addActionListener(this);
		}
		return cancelButton;
	}

	/**
	 * This method initializes weightsPanel	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getWeightsPanel() {
		if (weightsPanel == null) {

			GridBagConstraints gridBagConstraints16 = new GridBagConstraints();
			gridBagConstraints16.anchor = GridBagConstraints.WEST;
			gridBagConstraints16.gridy = -1;
			gridBagConstraints16.gridx = 3;
			GridBagConstraints gridBagConstraints8 = new GridBagConstraints();
			gridBagConstraints8.gridx = 1;
			gridBagConstraints8.anchor = GridBagConstraints.WEST;
			gridBagConstraints8.gridy = -1;
			GridBagConstraints gridBagConstraints41 = new GridBagConstraints();
			gridBagConstraints41.gridx = 2;
			gridBagConstraints41.anchor = GridBagConstraints.WEST;
			gridBagConstraints41.gridy = -1;
			weightsPanel = new JPanel();
			weightsPanel.setLayout(new GridBagLayout());
			weightsPanel.add(getWeightButton1(), gridBagConstraints8);
			weightsPanel.add(getWeightButton2(), gridBagConstraints41);
			weightsPanel.add(getWeightButton3(), gridBagConstraints16);
			ButtonGroup weightsGroup = new ButtonGroup();
			weightsGroup.add(weightButton1);
			weightsGroup.add(weightButton2);
			weightsGroup.add(weightButton3);
		}
		return weightsPanel;
	}

	/**
	 * This method initializes weightButton1	
	 * 	
	 * @return javax.swing.JRadioButton	
	 */
	private JRadioButton getWeightButton1() {
		if (weightButton1 == null) {
			weightButton1 = new JRadioButton();
			weightButton1.setText("Unit");
		}
		return weightButton1;
	}

	/**
	 * This method initializes weightButton2	
	 * 	
	 * @return javax.swing.JRadioButton	
	 */
	private JRadioButton getWeightButton2() {
		if (weightButton2 == null) {
			weightButton2 = new JRadioButton();
			weightButton2.setText("1 / (errors)\u00b2");
		}
		return weightButton2;
	}

	/**
	 * This method initializes weightButton3	
	 * 	
	 * @return javax.swing.JRadioButton	
	 */
	private JRadioButton getWeightButton3() {
		if (weightButton3 == null) {
			weightButton3 = new JRadioButton();
			weightButton3.setText("1 / data");
		}
		return weightButton3;
	}

	/**
	 * This method initializes fitPanel	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getFitPanel() {
		if (fitPanel == null) {
			GridBagConstraints gridBagConstraints15 = new GridBagConstraints();
			gridBagConstraints15.gridx = 1;
			gridBagConstraints15.gridy = 0;
			fitPanel = new JPanel();
			fitPanel.setLayout(new GridBagLayout());
			fitPanel.add(getFitButton1(), new GridBagConstraints());
			fitPanel.add(getFitButton2(), gridBagConstraints15);
			ButtonGroup fitGroup = new ButtonGroup();
			fitGroup.add(fitButton1);
			fitGroup.add(fitButton2);
		}
		return fitPanel;
	}

	/**
	 * This method initializes fitButton1	
	 * 	
	 * @return javax.swing.JRadioButton	
	 */
	private JRadioButton getFitButton1() {
		if (fitButton1 == null) {
			fitButton1 = new JRadioButton();
			fitButton1.setText("Least squares");
		}
		return fitButton1;
	}

	/**
	 * This method initializes fitButton2	
	 * 	
	 * @return javax.swing.JRadioButton	
	 */
	private JRadioButton getFitButton2() {
		if (fitButton2 == null) {
			fitButton2 = new JRadioButton();
			fitButton2.setText("Marquardt");
		}
		return fitButton2;
	}

	/**
	 * This method initializes predicateBox	
	 * 	
	 * @return javax.swing.JCheckBox	
	 */
	private JCheckBox getPredicateBox() {
		if (predicateBox == null) {
			predicateBox = new JCheckBox();
			predicateBox.setText("Use predicate observations");
		}
		return predicateBox;
	}

	/**
	 * This method initializes betaBox	
	 * 	
	 * @return javax.swing.JCheckBox	
	 */
	private JCheckBox getBetaBox() {
		if (betaBox == null) {
			betaBox = new JCheckBox();
			betaBox.setText("Include \u03b2(Q) correction");
		}
		return betaBox;
	}

	/**
	 * Set the fields in the window to reflect the current values
	 *
	 */
	void setup() {
		Common_two two_ = Core.getTwo_();
		
		boolean mod=false;
		int[] lm=two_.getLm();
		for(int i=0;i<two_.getNp();i++){
			if(lm[i]==5 || lm[i]==6){
				mod=true;
			}
		}
		field2.setEnabled(mod);
		field3.setEnabled(mod);
		field4.setEnabled(mod);
		field5.setEnabled(mod);

		
		float[] con = two_.getCon();
		field1.setText(String.format("%.6e",con[0]));
		field2.setText(String.format("%.6e",con[1]));
		field3.setText(String.format("%.6e",con[2]));
		field4.setText(String.format("%.6e",con[3]));
		field5.setText(String.format("%.6e",con[4]));
		
		weightButton1.setSelected(false);
		weightButton2.setSelected(false);
		weightButton3.setSelected(false);
		switch(two_.getIw()) {
		case 0: weightButton1.setSelected(true); break;
		case 1: weightButton2.setSelected(true); break;
		case 2: weightButton3.setSelected(true); break;
		}
		
		fitButton1.setSelected(false);
		fitButton2.setSelected(false);
		switch(two_.getIk()) {
		case 0: fitButton1.setSelected(true); break;
		case 1: fitButton2.setSelected(true); break;
		}
		
		if(two_.getIp()==1) {
			predicateBox.setSelected(true);
		} else {
			predicateBox.setSelected(false);
		}
		
		missSpinner.setValue(two_.getMs());
		
		int[] ls = two_.getLs();
		if(ls[2]==0) {
			betaBox.setSelected(true);
		} else {
			betaBox.setSelected(false);
		}
	}

	/**
	 * Refreshes the constants and flags window (only if window is visible)
	 *
	 */
	void refresh() {
		if(this.isVisible()){
			setup();
		}
	}
	
	private void submit() {
		float[] c = new float[5];
		int[] f = new int[8];
		boolean err = false;
		try {
			c[0]  = Float.valueOf(field1.getText());
		}
		catch (NumberFormatException e) {
			errorMessage("<html>The value entered for "+label[0]+" is invalid</html>");
			err = true;
		}
		
		try {
			c[1]  = Float.valueOf(field2.getText());
		}
		catch (NumberFormatException e) {
			errorMessage("<html>The value entered for "+label[1]+" is invalid</html>");
			err = true;
		}
		
		try {
			c[2]  = Float.valueOf(field3.getText());
		}
		catch (NumberFormatException e) {
			errorMessage("<html>The value entered for "+label[2]+" is invalid</html>");
			err = true;
		}
		
		try {
			c[3] = Float.valueOf(field4.getText());
		}
		catch (NumberFormatException e) {
			errorMessage("<html>The value entered for "+label[3]+" is invalid</html>");
			err = true;
		}
		
		try {
			c[4]  = Float.valueOf(field5.getText());
		}
		catch (NumberFormatException e) {
			errorMessage("<html>The value entered for "+label[4]+" is invalid</html>");
			err = true;
		}

		if(!err) {
			Common_two two_ = Core.getTwo_();
			float[] con = two_.getCon();
			// if constants have changed, set the new values
			for(int i=0;i<5;i++){
				if(con[i]!=c[i]){
					Core.talk("N"+(i+1)+"="+c[i]);
				}
			}
						
			// weight
			if(weightButton3.isSelected()){
				f[0]=2;
			} else if(weightButton2.isSelected()){
				f[0]=1;				
			} else {
				f[0]=0;
			}
			if(two_.getIw() != f[0]){
				Core.talk("K1="+f[0]);
			}
			
			// fit type
			if(fitButton2.isSelected()){
				f[1]=1;
			} else {
				f[1]=0;
			}
			if(two_.getIk() != f[1]){
				Core.talk("K2="+f[1]);
			}
			
			// predicate observations
			if(predicateBox.isSelected()){
				f[2]=1;
			} else {
				f[2]=0;
			}
			if(two_.getIp() != f[2]){
				Core.talk("K3="+f[2]);
			}
			
			// use every Nth data point
			f[3]=missSpinnerModel.getNumber().intValue();
			if(two_.getMs() != f[3]){
				Core.talk("K4="+f[3]);
			}
			
			// B(Q)
			if(betaBox.isSelected()){
				f[7]=0;
			} else {
				f[7]=1;
			}
			int[] ls = two_.getLs();
			if(ls[2] != f[7]){
				Core.talk("K8="+f[7]);
			}
			
		}
	}

	/**
	 * Show an error message dialog
	 * @param message
	 */
	private void errorMessage(String message) {
		System.err.println(message);
		JOptionPane optionPane = new JOptionPane(message,JOptionPane.ERROR_MESSAGE);
		JDialog box=optionPane.createDialog(this, "Error");
		box.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
		box.setVisible(true);
	}

	public void actionPerformed(ActionEvent e) {
		String name=((Component)e.getSource()).getName();
		if(name.equals("ok")){
			submit();
			this.setVisible(false);
		}
		else if(name.equals("apply")){
			submit();
		}
	}
	
}  //  @jve:decl-index=0:visual-constraint="19,4"
