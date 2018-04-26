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
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;
import javax.swing.border.Border;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

public class Range extends JDialog implements ActionListener {

	private static final long serialVersionUID = 1L;

	private JPanel jContentPane = null;

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

	final int maxSets=fish.CoreConstants.MF;

	private JLabel jLabel2 = null;

	private int set;
	private int points;
	
	/**
	 * @param owner
	 */
	public Range(Frame owner,int set) {
		super(owner,true);
		this.set=set;
		Common_one one_ = Core.getOne_();
		int[] nch = one_.getNch();
		points = nch[(set-1)*10];
		System.out.println("Data range window for set "+set+", which has "+points+ " points");
		initialize();
		jLabel2.setText("Set "+set+" has "+points+" data points");
		this.pack();
		this.setLocationRelativeTo(owner);
		this.setVisible(true);
	}

	/**
	 * This method initializes this
	 * 
	 * @return void
	 */
	private void initialize() {
		this.setSize(300, 319);
		this.setTitle("Set Data Range");
		this.setDefaultCloseOperation(DISPOSE_ON_CLOSE);
		this.setContentPane(getJContentPane());
	}

	/**
	 * This method initializes jContentPane
	 * 
	 * @return javax.swing.JPanel
	 */
	private JPanel getJContentPane() {
		if (jContentPane == null) {
			GridBagConstraints gridBagConstraints5 = new GridBagConstraints();
			gridBagConstraints5.gridx = 4;
			gridBagConstraints5.insets = new Insets(12, 0, 0, 0);
			gridBagConstraints5.gridy = 0;
			jLabel2 = new JLabel();
			
			jLabel2.setText("Set 8 has 999 data points");
			GridBagConstraints gridBagConstraints3 = new GridBagConstraints();
			gridBagConstraints3.gridx = 4;
			gridBagConstraints3.gridy = 1;
			GridBagConstraints gridBagConstraints13 = new GridBagConstraints();
			gridBagConstraints13.gridx = 1;
			gridBagConstraints13.gridwidth = 5;
			gridBagConstraints13.anchor = GridBagConstraints.EAST;
			gridBagConstraints13.insets = new Insets(0, 0, 0, 8);
			gridBagConstraints13.gridy = 4;
			jContentPane = new JPanel();
			jContentPane.setLayout(new GridBagLayout());
			jContentPane.add(getButtonsPanel(), gridBagConstraints13);
			jContentPane.add(getPane3(), gridBagConstraints3);
			jContentPane.add(jLabel2, gridBagConstraints5);
		}
		return jContentPane;
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
			nextButton.setText("OK");
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
			TitledBorder border = BorderFactory.createTitledBorder("Use data points");
			pane4.setBorder(border);
			pane4.add(getStartSet(), gridBagConstraints);
			pane4.add(jLabel, gridBagConstraints1);
			pane4.add(getEndSet(), gridBagConstraints2);
			pane4.add(jLabel1, gridBagConstraints4);
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
			Common_one one_ = Core.getOne_();
			int[] nc3 = one_.getNc3();
			System.out.println("Start: "+nc3[(set-1)*10]+" 1 "+points);
			startSetModel=new SpinnerNumberModel(nc3[(set-1)*10],1,points,1);
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
			Common_one one_ = Core.getOne_();
			int[] nc4 = one_.getNc4();
			System.out.println("End: "+nc4[(set-1)*10]+" 1 "+points);
			endSetModel=new SpinnerNumberModel(nc4[(set-1)*10],1,points,1);
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
			this.dispose();
		}
		else if(name.equals("next")){
			int start=startSetModel.getNumber().intValue();
			int end=endSetModel.getNumber().intValue();
			for(int i=1;i<11;i++){
				Core.range((set-1)*10+i,start,end);
			}
			
			// record the limits so that they can be re-applied if the dataset is re-activated
			DataList.getDataSet(set-1).setLimits(start, end);
			
			// refresh any open plot windows
			fish.plot.PlotFrame.refreshAll(false);
			
			this.dispose();
		}
	}

	
}  //  @jve:decl-index=0:visual-constraint="10,10"
