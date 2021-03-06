package fish.plot;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.UIManager;
import javax.swing.border.EmptyBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.TableColumn;

public class SeriesList extends JDialog implements ActionListener, ListSelectionListener {

	private JPanel jContentPane = null;
	private JButton addButton = null;
	private JButton editButton = null;
	private JButton deleteButton = null;
	private JScrollPane seriesScrollPane = null;
	JTable seriesTable = null;
	private JButton closeButton = null;
	PlotData plotData = null;
	private PlotFrame plotFrame = null;

	SeriesList(PlotFrame parent,PlotData plotData) {
		// create a modal JDialog (i.e. blocks other input to the program)
		super(parent,true);
		this.plotData=plotData;
		this.plotFrame=parent;
		initialize();
	}
	
	private void initialize() {
		this.setSize(new Dimension(452, 245));
		this.setTitle("Plot Series");
		this.setContentPane(getJContentPane());
		setColumnWidth(seriesTable, 0, 30, 40, 100);
		setColumnWidth(seriesTable, 1, 50, 100, 200);		
	}
	
	/**
	 * This method initializes jContentPane	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getJContentPane() {
		if (jContentPane == null) {
			GridBagConstraints gridBagConstraints6 = new GridBagConstraints();
			gridBagConstraints6.gridx = 3;
			gridBagConstraints6.insets = new Insets(8, 0, 0, 0);
			gridBagConstraints6.anchor = GridBagConstraints.SOUTHEAST;
			gridBagConstraints6.gridy = 1;
			GridBagConstraints gridBagConstraints5 = new GridBagConstraints();
			gridBagConstraints5.fill = GridBagConstraints.BOTH;
			gridBagConstraints5.gridy = 0;
			gridBagConstraints5.weightx = 1.0;
			gridBagConstraints5.weighty = 1.0;
			gridBagConstraints5.gridwidth = 4;
			gridBagConstraints5.gridx = 0;
			GridBagConstraints gridBagConstraints11 = new GridBagConstraints();
			gridBagConstraints11.gridx = 2;
			gridBagConstraints11.anchor = GridBagConstraints.SOUTHWEST;
			gridBagConstraints11.ipady = 0;
			gridBagConstraints11.insets = new Insets(8, 0, 0, 0);
			gridBagConstraints11.gridy = 1;
			GridBagConstraints gridBagConstraints1 = new GridBagConstraints();
			gridBagConstraints1.gridx = 1;
			gridBagConstraints1.anchor = GridBagConstraints.SOUTHWEST;
			gridBagConstraints1.insets = new Insets(8, 0, 0, 0);
			gridBagConstraints1.gridy = 1;
			GridBagConstraints gridBagConstraints = new GridBagConstraints();
			gridBagConstraints.insets = new Insets(8, 0, 0, 0);
			gridBagConstraints.gridy = 1;
			gridBagConstraints.anchor = GridBagConstraints.SOUTHWEST;
			gridBagConstraints.gridx = 0;
			jContentPane = new JPanel();
			jContentPane.setLayout(new GridBagLayout());
			jContentPane.setBorder(new EmptyBorder(8,8,8,8));
			jContentPane.add(getAddButton(), gridBagConstraints);
			jContentPane.add(getEditButton(), gridBagConstraints1);
			jContentPane.add(getDeleteButton(), gridBagConstraints11);
			jContentPane.add(getSeriesScrollPane(), gridBagConstraints5);
			jContentPane.add(getCloseButton(), gridBagConstraints6);
		}
		return jContentPane;
	}

	/**
	 * This method initializes addButton	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getAddButton() {
		if (addButton == null) {
			addButton = new JButton();
			addButton.setText("Add");
			addButton.setName("add");
			addButton.addActionListener(this);
		}
		return addButton;
	}

	/**
	 * This method initializes editButton	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getEditButton() {
		if (editButton == null) {
			editButton = new JButton();
			editButton.setText("Edit");
			editButton.setName("edit");
			editButton.setEnabled(false);
			editButton.addActionListener(this);
		}
		return editButton;
	}

	/**
	 * This method initializes deleteButton	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getDeleteButton() {
		if (deleteButton == null) {
			deleteButton = new JButton();
			deleteButton.setText("Delete");
			deleteButton.setName("delete");
			deleteButton.setEnabled(false);
			deleteButton.addActionListener(this);
		}
		return deleteButton;
	}

	/**
	 * This method initializes seriesScrollPane	
	 * 	
	 * @return javax.swing.JScrollPane	
	 */
	private JScrollPane getSeriesScrollPane() {
		if (seriesScrollPane == null) {
			seriesScrollPane = new JScrollPane();
			seriesScrollPane.setViewportView(getSeriesTable());
			seriesScrollPane.setBackground(UIManager.getColor("Table.background"));
		}
		return seriesScrollPane;
	}

	/**
	 * This method initializes seriesTable	
	 * 	
	 * @return javax.swing.JTable	
	 */
	private JTable getSeriesTable() {
		if (seriesTable == null) {
			seriesTable = new JTable();
			seriesTable.setModel(plotData);
			seriesTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
			seriesTable.getSelectionModel().addListSelectionListener(this);
			seriesTable.setOpaque(false);
		}
		return seriesTable;
	}

	/**
	 * This method initializes closeButton	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getCloseButton() {
		if (closeButton == null) {
			closeButton = new JButton();
			closeButton.setText("Close");
			closeButton.setName("close");
			closeButton.addActionListener(this);
		}
		return closeButton;
	}

	private void setColumnWidth(JTable table,int col,int min,int pref,int max){
		TableColumn column=table.getColumnModel().getColumn(col);
	    column.setPreferredWidth(pref);
	    column.setMinWidth(min);
	    column.setMaxWidth(max);	  		
	}

	public void actionPerformed(ActionEvent e) {
		String name=((Component)e.getSource()).getName();
		if(name.equals("edit")) {
			int sel=seriesTable.getSelectedRow();
			if(sel>-1){
				SeriesEdit seriesEdit=new SeriesEdit(this,plotData.getSeries(sel),false);
				seriesEdit.setLocationRelativeTo(this);
				seriesEdit.pack();
				seriesEdit.setVisible(true);
			}
		}
		else if(name.equals("add")){
			plotData.addSeries();
			SeriesEdit seriesEdit=new SeriesEdit(this,plotData.getSeries(seriesTable.getRowCount()-1),true);
			seriesEdit.setLocationRelativeTo(this);
			seriesEdit.pack();
			seriesEdit.setVisible(true);
		}
		else if(name.equals("delete")){
			int sel=seriesTable.getSelectedRow();
			if(sel>-1){
				plotData.deleteSeries(sel);
				plotData.resetSeriesStyles();
				updatePlot();
			}
		}
		else if(name.equals("close")) {
			setVisible(false);
		}
	}

	public void valueChanged(ListSelectionEvent e) {
		if(seriesTable.getSelectedRow()==-1){
			editButton.setEnabled(false);
			deleteButton.setEnabled(false);				
		}
		else{
			editButton.setEnabled(true);
			deleteButton.setEnabled(true);							
		}
	}

	/**
	 * Update both the table listing all the the series and the plot window that it controls
	 *
	 */
	void updatePlot() {
		plotFrame.refresh();
		seriesTable.revalidate();		
	}
}
