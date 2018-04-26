package fish;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.InputMismatchException;
import java.util.NoSuchElementException;
import java.util.Vector;
import java.util.prefs.Preferences;

import javax.swing.DefaultCellEditor;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JTabbedPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.JTextPane;
import javax.swing.SpinnerNumberModel;
import javax.swing.UIManager;
import javax.swing.border.EmptyBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

import fish.plot.PlotFrame;

public class FishFrame extends JFrame implements ActionListener, ItemListener {

	private int dispCycle=0;
	private Vector<Cycle> cycles=null;  //  @jve:decl-index=0:
	
	private static final long serialVersionUID = 1L;
	private JPanel jContentPane = null;
	private JMenuBar menubar = null;
	private JMenu fileMenu = null;
	private JMenu fitMenu = null;
	private JMenu plotMenu = null;
	private JMenu prefMenu = null;
	private JMenu helpMenu = null;
	private JPopupMenu modelPopupMenu = null;
	private JPopupMenu dataPopupMenu = null;
	private JTabbedPane tabs = null;
	private JPanel dataTab = null;
	private JPanel modelTab = null;
	private JPanel fitTab = null;
	private JScrollPane modelListBoxScroll = null;
	private JList modelListBox = null;
	private JButton runButton = null;
	private ModelList modelList;  //  @jve:decl-index=0:
	private JScrollPane fitTableScroll = null;
	private JTable fitTable = null;
	private FittingTable fit = new FittingTable(this);
	private TableColumn[] tableCols;
	private JButton addFileButton = null;
	private JScrollPane dataScrollPane = null;
	private JTable dataTable = null;
	private DataList dataList = null;
	private JFileChooser modelFileChooser;
	private JFileChooser dataFileChooser;
	private JFileChooser modelSaveChooser;
	private JFileChooser dataSaveChooser;
	private Constants constants;
	private SaveData saveDataDialog;
	private JSpinner cycleSpinner = null;
	private SpinnerNumberModel cycleSpinnerModel;
	private JTextPane outputPane = null;
	private JButton ffButton = null;
	
	private Preferences prefs;
	private boolean paramSaved=false;
	private boolean dataSaved=false;
	private int popupX;
	private int popupY;
	private boolean indicateModel=false;
	int modelListHighlight=-1;
	private boolean showXdweSse;
	private JButton addModelButton = null;
	private boolean[] saveData;
	private int saveStart;
	private int saveEnd;
	private boolean noErrs;
	private Cursor normalCursor;  //  @jve:decl-index=0:
	private Cursor busyCursor;  //  @jve:decl-index=0:
	private int dataPopupMenuSet;
	/**
	 * This is the default constructor
	 */
	public FishFrame() {
		super();
		initialize();
	}

	/**
	 * This method initializes this
	 * 
	 * @return void
	 */
	private void initialize() {
		dataList = new DataList(this);
		modelList = new ModelList();
		prefs = Preferences.userNodeForPackage(this.getClass());
		showXdweSse = prefs.getBoolean("ShowXdweSse",false);		
		this.setSize(552, 365);
		this.setJMenuBar(getMenubar());
		this.setContentPane(getJContentPane());
		this.setTitle("Fish");
		setName("FishPanel");
		this.setIconImage(Toolkit.getDefaultToolkit().getImage("bin/fish/fish.png"));
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		this.setLocation(20,40);
		new PlotFrame(this);
		setDataList(dataList);
		setModelList(modelList);

		constants = new Constants(this);	
		saveDataDialog = new SaveData(this);
		saveData = new boolean[10];
		saveData[1]=true;
		saveStart=1;
		saveEnd=8;
		
		noErrs=false;
		
		normalCursor = new Cursor(Cursor.DEFAULT_CURSOR);
		busyCursor = new Cursor(Cursor.WAIT_CURSOR);
		
		String modelFile = prefs.get("DefaultModelFile","");
		try {
			modelList.addlsq(modelFile);
		}
		catch(Exception ex) {
			System.err.println("Could not read default model file");
		}
	}

	/**
	 * This method initializes jContentPane
	 * 
	 * @return javax.swing.JPanel
	 */
	private JPanel getJContentPane() {
		if (jContentPane == null) {
			BorderLayout borderLayout = new BorderLayout();
			borderLayout.setVgap(0);
			jContentPane = new JPanel();
			jContentPane.setLayout(borderLayout);
			jContentPane.add(getTabs(), BorderLayout.CENTER);
		}
		return jContentPane;
	}

	/**
	 * This method initializes menubar	
	 * 	
	 * @return javax.swing.JMenuBar	
	 */
	private JMenuBar getMenubar() {
		if (menubar == null) {
			menubar = new JMenuBar();
			menubar.add(getFileMenu());
			menubar.add(getFitMenu());
			menubar.add(getPlotMenu());
			menubar.add(getPrefMenu());
			menubar.add(getHelpMenu());
		}
		return menubar;
	}

	/**
	 * This method initializes fileMenu	
	 * 	
	 * @return javax.swing.JMenu	
	 */
	private JMenu getFileMenu() {
		if (fileMenu == null) {
			fileMenu = new JMenu();
			fileMenu.setText("File");
			JMenuItem item;

			item=new JMenuItem("Save Data");
			item.setName("saveData");
			item.addActionListener(this);
			fileMenu.add(item);			
			
			item=new JMenuItem("Save Data As...");
			item.setName("saveDataAs");
			item.addActionListener(this);
			fileMenu.add(item);

			item=new JMenuItem("Save Parameters");
			item.setName("saveParam");
			item.addActionListener(this);
			fileMenu.add(item);
			
			item=new JMenuItem("Save Parameters As...");
			item.setName("saveParamAs");
			item.addActionListener(this);
			fileMenu.add(item);
			
			fileMenu.addSeparator();
			
			item=new JMenuItem("Exit");
			item.setName("exit");
			item.addActionListener(this);
			fileMenu.add(item);
		
		}
		return fileMenu;
	}

	private JMenu getFitMenu() {
		if (fitMenu == null) {
			fitMenu = new JMenu();
			fitMenu.setText("Fit");
			JMenuItem item;

			item=new JMenuItem("Constants and Flags...");
			item.setName("consts");
			item.addActionListener(this);
			fitMenu.add(item);			
		}
		return fitMenu;
	}
	
	private JMenu getPlotMenu(){
		if (plotMenu == null) {
			plotMenu = new JMenu();
			plotMenu.setText("Plot");
			JMenuItem item;

			item=new JMenuItem("New Window");
			item.setName("newWindow");
			item.addActionListener(this);
			plotMenu.add(item);
			
			item=new JMenuItem("Bring All Plots to Top");
			item.setName("plotsTop");
			item.addActionListener(this);
			plotMenu.add(item);			
		}
		return plotMenu;
	}

	private JMenu getPrefMenu(){
		if (prefMenu == null) {
			prefMenu = new JMenu();
			prefMenu.setText("Preferences");
			JMenuItem item;
			JCheckBoxMenuItem cbItem;

			item=new JMenuItem("Preferences...");
			item.setName("prefs");
			item.addActionListener(this);
			prefMenu.add(item);	
			
			prefMenu.addSeparator();
			
			cbItem=new JCheckBoxMenuItem("Show Model Numbers");
			cbItem.setName("AdvFit");
			cbItem.setSelected(prefs.getBoolean("AdvancedFitTable",false));
			cbItem.addItemListener(this);
			prefMenu.add(cbItem);
			
			cbItem=new JCheckBoxMenuItem("Show SWSE and XDWE");
			cbItem.setName("SwseXdwe");
			cbItem.setSelected(showXdweSse);
			cbItem.addItemListener(this);
			prefMenu.add(cbItem);		
			
		}
		return prefMenu;
	}

	private JMenu getHelpMenu(){
		if (helpMenu == null) {
			helpMenu = new JMenu();
			helpMenu.setText("Help");
			JMenuItem item;

			item=new JMenuItem("About Fish...");
			item.setName("about");
			item.addActionListener(this);
			helpMenu.add(item);	
		}
		return helpMenu;
	}
	
	private JPopupMenu getModelPopupMenu(){
		if (modelPopupMenu == null) {
			modelPopupMenu = new JPopupMenu();
			JMenuItem item;
			
			item=new JMenuItem("Refresh model file");
			item.setName("refreshModel");
			item.addActionListener(this);
			modelPopupMenu.add(item);
			
			item=new JMenuItem("Remove model");
			item.setName("removeModel");
			item.addActionListener(this);
			modelPopupMenu.add(item);
			
			item=new JMenuItem("Remove model file");
			item.setName("removeModelFile");
			item.addActionListener(this);
			modelPopupMenu.add(item);			
		}
		return modelPopupMenu;
	}
	
	private JPopupMenu getDataPopupMenu(){
		if (dataPopupMenu == null) {
			dataPopupMenu = new JPopupMenu();
			JMenuItem item;
			
			item=new JMenuItem("Set data range...");
			item.setName("dataRange");
			item.addActionListener(this);
			dataPopupMenu.add(item);	
		}
		return dataPopupMenu;
	}	
	
	/**
	 * This method initializes tabs	
	 * 	
	 * @return javax.swing.JTabbedPane	
	 */
	private JTabbedPane getTabs() {
		if (tabs == null) {
			tabs = new JTabbedPane();
			tabs.setBorder(new EmptyBorder(4,4,4,4));
			tabs.addTab("Data", null, getDataTab(), null);
			tabs.addTab("Model", null, getModelTab(), null);
			tabs.addTab("Fitting", null, getFitTab(), null);
		}
		return tabs;
	}

	/**
	 * This method initializes dataTab	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getDataTab() {
		if (dataTab == null) {
			GridBagConstraints gridBagConstraints5 = new GridBagConstraints();
			gridBagConstraints5.fill = GridBagConstraints.BOTH;
			gridBagConstraints5.gridy = 0;
			gridBagConstraints5.weightx = 1.0;
			gridBagConstraints5.weighty = 1.0;
			gridBagConstraints5.anchor = GridBagConstraints.NORTH;
			gridBagConstraints5.gridx = 0;
			GridBagConstraints gridBagConstraints4 = new GridBagConstraints();
			gridBagConstraints4.gridx = 0;
			gridBagConstraints4.anchor = GridBagConstraints.WEST;
			gridBagConstraints4.gridy = 1;
			dataTab = new JPanel();
			dataTab.setBorder(new EmptyBorder(8,8,8,8));
			dataTab.setOpaque(false);
			dataTab.setLayout(new GridBagLayout());
			dataTab.add(getAddFileButton(), gridBagConstraints4);
			dataTab.add(getDataScrollPane(), gridBagConstraints5);
			dataFileChooser=new JFileChooser();
			FishFileFilter allData=new FishFileFilter("All data files");
			dataFileChooser.addChoosableFileFilter(new FishFileFilter("LOQ file (.q)","q",allData));
			FishFileFilter illFilter=new FishFileFilter("RNILS file (.001, .002, etc.)","001",allData);
			illFilter.addExtension("002",allData);
			illFilter.addExtension("003",allData);
			illFilter.addExtension("004",allData);
			illFilter.addExtension("005",allData);
			illFilter.addExtension("006",allData);
			illFilter.addExtension("007",allData);
			illFilter.addExtension("008",allData);
			illFilter.addExtension("009",allData);
			dataFileChooser.addChoosableFileFilter(illFilter);
			dataFileChooser.addChoosableFileFilter(new FishFileFilter("SAS XML file (.xml)","xml",allData));
			FishFileFilter textFilter=new FishFileFilter("Text file (.txt, .csv, .dat)","txt",allData);
			textFilter.addExtension("csv",allData);
			textFilter.addExtension("dat",allData);
			dataFileChooser.addChoosableFileFilter(textFilter);
			dataFileChooser.addChoosableFileFilter(allData);
			Preferences prefs = Preferences.userNodeForPackage(this.getClass());
			String workDir = prefs.get("WorkDirectory", "");
			dataFileChooser.setCurrentDirectory(new File(workDir));
		}
		return dataTab;
	}

	/**
	 * This method initializes modelTab	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getModelTab() {
		if (modelTab == null) {
			GridBagConstraints gridBagConstraints11 = new GridBagConstraints();
			gridBagConstraints11.gridx = 0;
			gridBagConstraints11.anchor = GridBagConstraints.WEST;
			gridBagConstraints11.gridy = 1;
			GridBagConstraints gridBagConstraints = new GridBagConstraints();
			gridBagConstraints.fill = GridBagConstraints.BOTH;
			gridBagConstraints.gridy = 0;
			gridBagConstraints.weightx = 1.0;
			gridBagConstraints.weighty = 1.0;
			gridBagConstraints.gridx = 0;
			modelTab = new JPanel();
			modelTab.setOpaque(false);
			modelTab.setBorder(new EmptyBorder(8,8,8,8));
			modelTab.setLayout(new GridBagLayout());
			modelTab.add(getModelListBoxScroll(), gridBagConstraints);
			modelTab.add(getAddModelButton(), gridBagConstraints11);
			modelFileChooser=new JFileChooser();
			modelFileChooser.addChoosableFileFilter(new FishFileFilter("Data file (.dat)","dat"));
			Preferences prefs = Preferences.userNodeForPackage(this.getClass());
			String workDir = prefs.get("WorkDirectory", "");
			modelFileChooser.setCurrentDirectory(new File(workDir));
		}
		return modelTab;
	}

	/**
	 * This method initializes fitTab	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getFitTab() {
		if (fitTab == null) {
			GridBagConstraints gridBagConstraints8 = new GridBagConstraints();
			gridBagConstraints8.gridx = 1;
			gridBagConstraints8.fill = GridBagConstraints.VERTICAL;
			gridBagConstraints8.insets = new Insets(0, 0, 0, 0);
			gridBagConstraints8.gridwidth = 1;
			gridBagConstraints8.ipadx = 0;
			gridBagConstraints8.ipady = 0;
			gridBagConstraints8.gridy = 2;
			GridBagConstraints gridBagConstraints7 = new GridBagConstraints();
			gridBagConstraints7.fill = GridBagConstraints.BOTH;
			gridBagConstraints7.gridy = 1;
			gridBagConstraints7.weightx = 1.0;
			gridBagConstraints7.weighty = 0.0;
			gridBagConstraints7.gridwidth = 3;
			gridBagConstraints7.gridheight = 1;
			gridBagConstraints7.ipady = 0;
			gridBagConstraints7.insets = new Insets(4, 0, 4, 0);
			gridBagConstraints7.gridx = 0;
			GridBagConstraints gridBagConstraints6 = new GridBagConstraints();
			gridBagConstraints6.gridx = 0;
			gridBagConstraints6.weightx = 0.0;
			gridBagConstraints6.ipadx = 12;
			gridBagConstraints6.ipady = 6;
			gridBagConstraints6.gridy = 2;
			GridBagConstraints gridBagConstraints3 = new GridBagConstraints();
			gridBagConstraints3.fill = GridBagConstraints.BOTH;
			gridBagConstraints3.gridy = 0;
			gridBagConstraints3.weightx = 1.0;
			gridBagConstraints3.weighty = 1.0;
			gridBagConstraints3.gridwidth = 3;
			gridBagConstraints3.gridx = 0;
			GridBagConstraints gridBagConstraints2 = new GridBagConstraints();
			gridBagConstraints2.fill = GridBagConstraints.BOTH;
			gridBagConstraints2.gridy = 0;
			gridBagConstraints2.weightx = 1.0;
			gridBagConstraints2.weighty = 1.0;
			gridBagConstraints2.gridx = 0;
			GridBagConstraints gridBagConstraints1 = new GridBagConstraints();
			gridBagConstraints1.gridx = 2;
			gridBagConstraints1.fill = GridBagConstraints.BOTH;
			gridBagConstraints1.gridy = 2;
			fitTab = new JPanel();
			fitTab.setOpaque(false);
			fitTab.setBorder(new EmptyBorder(8,8,8,8));
			fitTab.setLayout(new GridBagLayout());
			fitTab.add(getRunButton(), gridBagConstraints1);
			fitTab.add(getFitTableScroll(), gridBagConstraints3);
			fitTab.add(getCycleSpinner(), gridBagConstraints6);
			fitTab.add(getOutputPane(), gridBagConstraints7);
			fitTab.add(getFfButton(), gridBagConstraints8);
			modelSaveChooser=new JFileChooser();
			modelSaveChooser.addChoosableFileFilter(new FishFileFilter("LSQ model file (.dat)","dat"));
			modelSaveChooser.setDialogTitle("Save Parameters");
			dataSaveChooser=new JFileChooser();
			dataSaveChooser.addChoosableFileFilter(new FishFileFilter("LOQ file (.q)","q"));
			dataSaveChooser.setDialogTitle("Save Data");
			Preferences prefs = Preferences.userNodeForPackage(this.getClass());
			File workDir = new File(prefs.get("WorkDirectory", ""));
			dataSaveChooser.setCurrentDirectory(workDir);
			modelSaveChooser.setCurrentDirectory(workDir);

			// create vector
			cycles=new Vector<Cycle>();
			
		}
		return fitTab;
	}

	/**
	 * This method initializes modelListBoxScroll	
	 * 	
	 * @return javax.swing.JScrollPane	
	 */
	private JScrollPane getModelListBoxScroll() {
		if (modelListBoxScroll == null) {
			modelListBoxScroll = new JScrollPane();
			modelListBoxScroll.setViewportView(getModelListBox());
		}
		return modelListBoxScroll;
	}	
	
	/**
	 * This method initializes modelListBox	
	 * 	
	 * @return javax.swing.JList	
	 */
	private JList getModelListBox() {
		if (modelListBox == null) {
			modelListBox = new JList(){
				// This method is called as the cursor moves within the list
				public String getToolTipText(MouseEvent e) {
					// Get item index
					int index = locationToIndex(e.getPoint());
					// return title as tooltip
					try {
						return ((Model)getModel().getElementAt(index)).getToolTip();
					}
					catch (Exception ex){}
					return null;
				}
			};
			modelListBox.setCellRenderer(new ModelListRenderer(this));
			// listener for changed selection in model list
			modelListBox
			.addListSelectionListener(new ListSelectionListener() {
				public void valueChanged(ListSelectionEvent e) {
					ModelList mList=(ModelList)modelListBox.getModel();
					Model newModel=mList.getModel(modelListBox.getSelectedIndex());
					Model oldModel=mList.getModel();
					
					// if nothing is selected in the list, set indicateModel to false to that
					// the model is cleared correctly if the user cancels the operation
					if(modelListBox.getSelectedIndex()==-1){
						indicateModel=false;
					}
					
					// selected model has changed
					if(oldModel==null || 
							( newModel!=null &&
									(!indicateModel || (!(newModel.equals(oldModel)))) 
							) 
						) {
						Common_jfit jfit_=Core.getJfit_();
						int infit=jfit_.getInfit();
						boolean changeModel=true;
						if(infit!=0 && !(newModel.equals(oldModel))){
							// if model changed during a fit, make sure the user wanted to do that
							Object[] options = { "Continue", "Cancel" };
							JOptionPane optionPane = new JOptionPane(
									"A fit is in progress. Changing model will lose the results\n"+
									"obtained so far. Have you saved the data and/or parameters\n"+
									"as required?\n\n"+
									"Click Cancel if you need to save files or if you wish to\n"+
									"continue the fit with the current model.\n\n"+
									"Click Continue if you wish to start a new fit with a\n"+
									"different model file.",
									JOptionPane.WARNING_MESSAGE,JOptionPane.OK_CANCEL_OPTION,
									null,options,options[1]);
							JDialog message=optionPane.createDialog(modelTab, "Warning");
							message.setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);
							setCancelDefault(message, optionPane);
							//message.setLocationRelativeTo(frame);
							message.setVisible(true);
							Object choice=optionPane.getValue();
							if(choice.equals(options[1])){
								changeModel=false;
								if(indicateModel){
									modelListBox.setSelectedIndex(mList.getSelectedIndex());
								}
								else{
									modelListBox.clearSelection();
								}
							}									
						}
						if(changeModel){
							mList.setSelected(newModel);
							if(newModel!=null){
								newModel.activate();
								fitInit();
								cycles.removeAllElements();
								cycles.add(new Cycle());
								Common_two two_=Core.getTwo_();
								if(noErrs){
									Core.talk("K1=2");
								}
								Core.talk("OFF");
								Core.talk("R");
								Core.talk("ON");
								two_.setNyc(0);
								fitTable.revalidate();
								constants.refresh();
								jfit_.setInfit(0);
								dispCycle=0;
								cycleSpinner.setValue(0);
								cycleSpinnerModel.setMaximum(0);
								indicateModel=true;
								//PlotFrame.refreshAll(true);
								runButton.setEnabled(true);
							}
						}
						else if(newModel==null){
							indicateModel=false;
							modelListBox.clearSelection();
						}

					}
				}
			});
			modelListBox.addMouseListener(new MouseListener() {

				public void mouseClicked(MouseEvent e) {
					maybeShowPopup(e);
				}

				public void mouseEntered(MouseEvent e) {
					maybeShowPopup(e);
				}

				public void mouseExited(MouseEvent e) {
					maybeShowPopup(e);
				}

				public void mousePressed(MouseEvent e) {
					popupX=e.getX();
					popupY=e.getY();
					int sel=modelListBox.locationToIndex(new Point(popupX,popupY));
					modelListHighlight=sel;
					try {
						modelListBox.repaint(modelListBox.getCellBounds(sel, sel));
					}
					catch(NullPointerException ex) {
						// no models to highlight
					}
					maybeShowPopup(e);
				}

				public void mouseReleased(MouseEvent e) {
					popupX=e.getX();
					popupY=e.getY();
					int sel=modelListBox.locationToIndex(new Point(popupX,popupY));
					modelListHighlight=-1;
					try {
						modelListBox.repaint(modelListBox.getCellBounds(sel, sel));
					}
					catch(NullPointerException ex) {
						// no models to unhighlight
					}
					maybeShowPopup(e);			
				}

			});
			}
			modelPopupMenu=getModelPopupMenu();
			return modelListBox;
		}
	
	private void maybeShowPopup(MouseEvent e) {
		if(e.isPopupTrigger()){
			popupX=e.getX();
			popupY=e.getY();
			modelPopupMenu.show(e.getComponent(), popupX, popupY);
		}				
	}

	/**
	 * Sets the ListModel to display in the Model tab
	 * @param listModel
	 */
	void setModelList(ModelList listModel){
		modelList=listModel;
		JList modelListBox=getModelListBox();
		modelListBox.setModel(listModel);
	}

	/**
	 * Returns the current ModelList
	 * @return ModelList
	 */
	public ModelList getModelList(){
		return modelList;
	}
	
//	/**
//	 * Return the index of the model currently selected in the Model tab
//	 * @return
//	 */
//	public int getModelIndex() {
//		return modelListBox.getSelectedIndex();
//	}

	/**
	 * Initializes the fit by choosing internal data arrays, loading the data files,
	 * initializing the arrays, setting the cycle counter to zero and setting up an
	 * empty vector array in which to store all parameters from each cycle (deleting
	 * the old vector if one existed).
	 *
	 */
	void fitInit() {
		// choose set numbers
		int ns=DataList.maxSet();
		System.out.println("Setting "+ns+ " datasets");
		Common_two two_=Core.getTwo_();
		two_.setNs(ns);
		// load the data files
		boolean noErrFlag=false;
		for(int i=0;i<CoreConstants.MF;i++){
			DataList.activate(i,i*10+1);
			if(!DataList.hasErrors(i)){
					noErrFlag=true;
			}
		}
		// any of the data sets in use don't have error values...
		if(noErrFlag){
			// if we haven't already warned the user since the error-less data was chosen,
			// display a warning about it
			if(!noErrs){
				noErrs=true;
				JOptionPane.showMessageDialog(this,
						"Data currently in use do not have errors associated with them.\n\n" +
						"Weights for the fit will be set to 1/data, overriding the\n" +
						"weighting defined in the models you use.", 
						"Fit warning", JOptionPane.WARNING_MESSAGE);
			}
		}
		else{
			// all data sets have errors, so we don't need to override the setting in the model
			// definition
			noErrs=false;
		}
		Core.datset();

		// print sets
		Core.pindex();

		// set cycle display countrer to zero
		dispCycle=0;
		cycleSpinner.setValue(0);
		cycleSpinnerModel.setMaximum(0);

		// set up empty vector
		//cycles.removeAllElements();
		//cycles.add(new Cycle());
		PlotFrame.refreshAll(true);
	}

	
	
	public void itemStateChanged(ItemEvent e) {
		JMenuItem mi = (JMenuItem)(e.getSource());
		boolean selected =(e.getStateChange() == ItemEvent.SELECTED);
		String name=mi.getName();
		if(name.equals("AdvFit")){
			if(tableCols[0]!=null){
				prefs.putBoolean("AdvancedFitTable", selected);
				TableColumnModel tcm = fitTable.getColumnModel();
				if(selected){
					tcm.addColumn(tableCols[0]);
					tcm.moveColumn(5, 0);
					tcm.addColumn(tableCols[1]);
					tcm.moveColumn(6, 1);
				}
				else{
					tcm.removeColumn(tableCols[0]);
					tcm.removeColumn(tableCols[1]);
				}
			}
		}
		if(name.equals("SwseXdwe")){
			showXdweSse = selected;
			prefs.putBoolean("ShowXdweSse", selected);
			updateOutput();
		}
	}

	public void actionPerformed(ActionEvent e) {
		String name=((Component)e.getSource()).getName();
		if(name.equals("run")){
			boolean runOK=true;
			// find out if we're already doing a fit
			Common_jfit jfit_=Core.getJfit_();
			Common_two two_=Core.getTwo_();
			int infit=jfit_.getInfit();
			if(infit==0){
				fitInit();
				Core.talk("OFF");
				Core.talk("R");
				Core.talk("ON");
				cycles.removeAllElements();
				cycles.add(new Cycle());
				two_.setNyc(0);
				dispCycle=0;
				cycleSpinner.setValue(0);
				cycleSpinnerModel.setMaximum(0);
			}
			// check if we are reviewing an earlier cycle
			System.out.print((cycles.size()-1));
			System.out.print("  ");
			//System.out.println(dispCycle);
			if((cycles.size()-1) != dispCycle){
				//Component frame = (Component)e.getSource();
				 Object[] options = { "Continue", "Cancel" };
				JOptionPane optionPane = new JOptionPane(
						"The fit will continue from the currently selected cycle.\n"
						+"No information will be lost. New cycle numbers will be used,\n"
						+"so you can return to the current results at any time.\n\n"
						+"If you wish to continue fitting from the results of the\n"
						+"most recent cycle, you should return to that cycle before\n"
						+"clicking \"Run\". If you wish to do that now, you may cancel\n"
						+"this action.",
						JOptionPane.INFORMATION_MESSAGE,JOptionPane.OK_CANCEL_OPTION,
						null,options,options[0]);
				JDialog message=optionPane.createDialog(this, "Fit information");
				message.setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);
				//message.setLocationRelativeTo(frame);
				message.setVisible(true);
				Object choice=optionPane.getValue();
				if(choice.equals(options[1])){
					runOK=false;
				}
//				System.out.println(choice+" "+choice.toString()+" "+((Integer)choice).intValue());
			}
			// run
			if(runOK){
				this.setCursor(busyCursor);
				Core.talk("R");
				this.setCursor(normalCursor);
				//Core.talk("T");		
				//Core.talk("TT");
				//Core.talk("PP");
				fit.fireTableDataChanged();
				updateOutput();
				constants.refresh();
				int cycle=two_.getNyc();
				cycleSpinnerModel.setMaximum(cycle);
				dispCycle=cycle;
				cycles.add(new Cycle());
				//System.out.println("Just added cycle "+(-1+cycles.size()));
				cycleSpinner.setValue(cycle);
				//Core.talk("PLOT");
				PlotFrame.refreshAll(false);
			}
		} // end runButton
		else if(name.equals("saveData") && dataSaved){
			// first param is set no.; leave others as ,1,1,0
			doSaveData("",1);
		}
		else if(name.equals("saveDataAs") || (name.equals("saveData") && !dataSaved)){
			saveDataDialog.pack();
			saveDataDialog.setLocationRelativeTo(this);
			saveDataDialog.reset(saveStart, saveEnd,
					saveData[0], saveData[1], saveData[5], saveData[6], saveData[7]);
			saveDataDialog.setVisible(true);
		}
		else if(name.equals("saveParam") && paramSaved){
			Core.lsqout("",1);
		}
		else if(name.equals("saveParamAs") || (name.equals("saveParam") && !paramSaved)){
			int fcRtn=modelSaveChooser.showDialog(jContentPane,"Save");
			if (fcRtn == JFileChooser.APPROVE_OPTION) {
	            File file =modelSaveChooser.getSelectedFile();
	            if(!file.exists() || (file.exists() && askOverwrite(file))){
		            String filename=file.getAbsoluteFile().toString();
		            System.out.println("Saving model file: " + filename);
		            Core.lsqout(filename,0);
		            paramSaved=true;
	            }
	        }
		}
		else if(name.equals("exit")){
			System.exit(0);
		}
		else if(name.equals("newWindow")){
			PlotFrame newFrame=new PlotFrame(this);
			newFrame.refresh();
			newFrame.setVisible(true);
		}
		else if(name.equals("plotsTop")){
			PlotFrame.allToTop();
		}
		else if(name.equals("prefs")){
			FishPref fishPref = new FishPref(this);
			fishPref.pack();
			fishPref.setLocationRelativeTo(this);
			fishPref.setVisible(true);
		}
		else if(name.equals("consts")){
			if(constants.isVisible()){
				constants.setState(NORMAL); // un-minimizes window
				constants.toFront();
			} else {
				constants.setup();
				constants.pack();
				constants.setLocationRelativeTo(this);
				constants.setVisible(true);
			}
		}
		else if(name.equals("about")){
			// Fish.version is set at the top of Fish.java
			URL img = getClass().getResource("fish.png");
			JOptionPane optionPane = new JOptionPane(
					"<html><style>body { font-family: verdana, arial, sans-serif }</style>" +
					"<table><tr valign='top' cellspacing='0' cellpadding='0' cellborder='0'>" +
					"<td><img src=\""+img+"\"></td>" +
					"<td><font size='+2' color='#00008B'><b>Fish</b></font></td></tr></table>" +
					"version "+Fish.version+"<br>&nbsp;<br>" +
					"Fitting code by <b>R.K.Heenan</b>, STFC ISIS facility, &copy; 1989-2007<br>" +
					"Java front end by <b>Jonathan Rawle</b>, Diamond Light Source/ISIS<br>" +
					"Send comments and bug reports to jonathan.rawle@diamond.ac.uk<br>&nbsp;<br>" +
					"<i>Please</i> acknowledge use of this program!<br>&nbsp;<br>" +
					"For more software of interest to the small angle scattering community, vist:<br>" +
					"http://www.small-angle.ac.uk/<br>&nbsp;<br>" +
					"</html>"
					,JOptionPane.PLAIN_MESSAGE);
			JDialog box=optionPane.createDialog(this, "About Fish");
			box.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			box.setVisible(true);
		}
		// pop-up menu
		else if(name.equals("refreshModel")){
			try {
				modelList.getModel(modelListBox.locationToIndex(new Point(popupX,popupY))).refresh(modelList);
			}
			catch(ArrayIndexOutOfBoundsException ex) {}
			catch(NullPointerException ex) {}
			catch(IOException ex) {
				errorMessage("Could not refresh model file: model file could not be read");
			}
			catch(InputMismatchException ex) {
				errorMessage("Could not refresh model file: model file contains an error");
			}
			catch(NoSuchElementException ex) {
				errorMessage("Could not refresh model file: model file contains an error");
			}
			catch(IllegalStateException ex) {
				errorMessage("Could not refresh model file: model file contains an error");
			}
			//modelListBox.clearSelection();
			modelListBox.revalidate();
			modelListBox.repaint();
		}
		else if(name.equals("removeModel")){
			try {
				modelList.remove(modelListBox.locationToIndex(new Point(popupX,popupY)));
			}
			catch(ArrayIndexOutOfBoundsException ex) {}
			catch(NullPointerException ex) {}
			modelListBox.revalidate();
			modelListBox.repaint();
		}
		else if(name.equals("removeModelFile")){
			try {
				modelList.getModel(modelListBox.locationToIndex(new Point(popupX,popupY))).removeFile(modelList);
			}
			catch(ArrayIndexOutOfBoundsException ex) {}
			catch(NullPointerException ex) {}
			modelListBox.revalidate();
			modelListBox.repaint();
		}
		else if(name.equals("dataRange")){
			Range range = new Range(this,dataPopupMenuSet);
		}
	}	

	/**
	 * Shows the SaveDataAs file chooser dialog then saves the data required
	 * Called by SaveData dialog when Next> button clicked
	 * @param start lowest number set to save
	 * @param end highest number set to save
	 * @param o save OBS data
	 * @param c save CALC data
	 * @param p save P(Q)
	 * @param s save S(Q)
	 * @param b save beta(Q)
	 */
	void saveFileChooser(int start, int end, boolean o, boolean c, boolean p, boolean s, boolean b) {
		int fcRtn=dataSaveChooser.showDialog(jContentPane,"Save");
		if (fcRtn == JFileChooser.APPROVE_OPTION) {
            File file =dataSaveChooser.getSelectedFile();
            if(!file.exists() || (file.exists() && askOverwrite(file))){
	            String filename=file.getAbsoluteFile().toString();
	            System.out.println("Saving data file: " + filename);
	            //Common_one one_=Core.getOne_();
	            //int [] nch=one_.getNch();
	            //for(int i=0;i<8;i++){System.out.println(i+" "+nch[i]);}
	            saveData[0]=o;
	            saveData[1]=c;
	            saveData[5]=p;
	            saveData[6]=s;
	            saveData[7]=b;
	            saveStart=start;
	            saveEnd=end;
	    		doSaveData(filename,0);
	            dataSaved=true;
            }
        } 		
	}

	/**
	 * Save a datafile (following either Save or SaveAs actions)
	 * @param filename the filename to open, or blank if using currently open file
	 * @param noOverwrite 0 to overwrite the file or 1 to append
	 */
	private void doSaveData(String filename,int noOverwrite) {
		int flag = noOverwrite;
		DateFormat dateFormat = new SimpleDateFormat("dd-MMM-yy HH:mm:ss");
		String date=dateFormat.format(new Date());
		for(int i=(saveStart-1);i<saveEnd;i++){
			if(DataList.isSetUsed(i)){ // only save data sets that are in use
				for(int j=0;j<8;j++){
					if(saveData[j]){ // save only data types selected in dialog
						System.out.println("Saving dataset "+(i+1)+"."+(j+1));
						if(j>0){ // don't change titles if we are outputting OBS data
							String lab1="Set "+(i+1)+" ";
							if(j==1){
								lab1+="CALC";
							} else if(j==5){
								lab1+="P(Q)";
							} else if(j==6){
								lab1+="S(Q)";
							} else if(j==7){
								lab1+="beta(Q)";
							}

							lab1+="  "+date+"  Model: "+modelList.getModel().toString();
							Core.setlab2(1,1+j+i*10,lab1,lab1.length());
							Core.setlab2(2,1+j+i*10,DataList.getDataSet(i).getLabel(),DataList.getDataSet(i).getLabel().length());
						}
						Core.outp(1+j+i*10,1,1,0,filename,flag);
						filename="";
						flag=1;
					}
				}
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
	
	/**
	 * Pops up a dialog asking whether a file should be overwritten
	 * @param file the file in question
	 * @return boolean whether or not file should be overwritten
	 */
	private boolean askOverwrite(File file) {
	 Object[] options = { "Overwrite", "Cancel" };
		JOptionPane optionPane = new JOptionPane(
				"The file "+file.getName()+" already exists.\nDo you want to overwrite this file?\n" +
						"(The current contents of the file will be erased.)"
				,JOptionPane.QUESTION_MESSAGE,JOptionPane.OK_CANCEL_OPTION,
				null,options,options[1]);
		JDialog message=optionPane.createDialog(this, "File exists");
		setCancelDefault(message, optionPane);
		message.setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);
		message.setLocationRelativeTo(fitTab);
		message.setVisible(true);
		Object choice=optionPane.getValue();
		if(choice.equals(options[0])){
			return true;
		}
		return false;
	}
	
	/**
	 * This method initializes runButton	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getRunButton() {
		if (runButton == null) {
			runButton = new JButton();
			runButton.setText("Run");
			runButton.setName("run");
			runButton.addActionListener(this);		
			runButton.setEnabled(false);
		}
		return runButton;
	}

	/**
	 * Update the status pane at the bottom of the fitting window
	 *
	 */
	void updateOutput(){
		Common_three three_=Core.getThree_();
		Common_two two_=Core.getTwo_();
		String text = "";
				
		if(showXdweSse){
			int sets = DataList.countSets();
			if(sets > 1) {
				float[] fit = three_.getFit();
				for(int i=0;i<DataList.countSets();i++) {
					text += ("Set "+(i+1)+":  SSE="+String.format("%.4e",fit[i])+"\n");
				}
			}
			
			text += "SWSE="+String.format("%.4e",three_.getSse())
				+"        XDWE="+String.format("%.4e",three_.getXdwe());
		}
		else{
			text += "GOF="+String.format("%.4e",three_.getXdwe()/three_.getSse());
		}
		text+="        \u03bb="+String.format("%.1e",two_.getCon()[0])
			+"        var="+String.format("%.4e",three_.getVar());
		outputPane.setText(text);
	}
	
	/**
	 * This method initializes fitTableScroll	
	 * 	
	 * @return javax.swing.JScrollPane	
	 */
	private JScrollPane getFitTableScroll() {
		if (fitTableScroll == null) {
			fitTableScroll = new JScrollPane();
			fitTableScroll.setViewportView(getFitTable());
			fitTableScroll.setOpaque(false);
			fitTableScroll.getViewport().setOpaque(false);
			
		}
		return fitTableScroll;
	}

	/**
	 * This method initializes fitTable	
	 * 	
	 * @return javax.swing.JTable	
	 */
	private JTable getFitTable() {
		if (fitTable == null) {
			fit=new FittingTable(this);
			fitTable = new JTable(fit);
			fitTable.setBackground(Color.white);
			Class colClass=fitTable.getColumnClass(2);
			TableCellRenderer renderer=new FitTableCellRenderer();
			fitTable.setDefaultRenderer(colClass, renderer);
			
			// checkbox editor for "Fitting" column
		    JCheckBox checkBox = new JCheckBox();
		    checkBox.setBackground(Color.white);
		    TableCellEditor checkBoxEditor = new DefaultCellEditor(checkBox);
		    //checkBoxEditor.addCellEditorListener(new javax.swing.event.CellEditorListener(){
		    //	public void editingStopped(ChangeEvent e){
		    //		//System.out.println(((DefaultCellEditor)e.getSource()).getCellEditorValue());
		    //	}
		    //	public void editingCanceled(ChangeEvent e){}
		    //});
		    fitTable.getColumnModel().getColumn(5).setCellEditor(checkBoxEditor);
				        
			String h[]={"Model","Parameter no.","Label","Value","Std deviation","Fit","Calc shift"};
			for(int i=0;i<7;i++){
				fitTable.getColumnModel().getColumn(i).setHeaderValue(h[i]);
			}
			//TableColumn lcol=fitTable.getColumnModel().getColumn(0);
			//fitTable.getColumnModel().removeColumn(lcol);
			//fitTable.getColumnModel().addColumn(lcol);
			setColumnWidth(fitTable,0,20,25,100);
			setColumnWidth(fitTable,1,20,25,100);
			tableCols=new TableColumn[7];
			for(int i=0;i<7;i++){
				tableCols[i]=fitTable.getColumnModel().getColumn(i);
			}
			
			// shall we show the advanced fitting table (i.e. Heenan-esque with model number columns)?
			boolean advanced=prefs.getBoolean("AdvancedFitTable", false);
			if(!advanced){
				fitTable.getColumnModel().removeColumn(tableCols[0]);
				fitTable.getColumnModel().removeColumn(tableCols[1]);
			}
		}
		return fitTable;
	}

	/**
	 * This method initializes addFileButton	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getAddFileButton() {
		if (addFileButton == null) {
			addFileButton = new JButton();
			addFileButton.setText("Add data file...");
			addFileButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					//System.out.println("Add file button");
					dataFileChooser.setDialogTitle("Add data file");
					int fcRtn=dataFileChooser.showDialog(jContentPane,"Add");
					if (fcRtn == JFileChooser.APPROVE_OPTION) {
			            File file = dataFileChooser.getSelectedFile();
			            System.out.println("Opening data file: " + file.getAbsoluteFile());
			            dataList.addDataFile(file.getAbsoluteFile());
			            dataList.fireTableDataChanged();
			        }
				}
			});	
		}
		return addFileButton;
	}
	
	/**
	 * This method initializes dataScrollPane	
	 * 	
	 * @return javax.swing.JScrollPane	
	 */
	private JScrollPane getDataScrollPane() {
		if (dataScrollPane == null) {
			dataScrollPane = new JScrollPane();
			dataScrollPane.setViewportView(getDataTable());
			dataScrollPane.setOpaque(false);
			dataScrollPane.getViewport().setOpaque(false);
		}
		return dataScrollPane;
	}

	/**
	 * This method initializes dataTable	
	 * 	
	 * @return javax.swing.JTable	
	 */
	private JTable getDataTable() {
		if (dataTable == null) {
			dataTable = new JTable(){    
			    //Implement table cell tool tips.
			    public String getToolTipText(MouseEvent e) {
			        String tip = null;
			        java.awt.Point p = e.getPoint();
			        int row = rowAtPoint(p);
			        int col = columnAtPoint(p);
			        int modelcol = convertColumnIndexToModel(col);
			        if(modelcol==2 || modelcol==3){
			        	tip=dataList.getToolTipAt(row, modelcol).toString();
			        }
			        return tip;
			    }
			};
		}
		dataTable.addMouseListener(new MouseAdapter() {
			public void mouseClicked(MouseEvent e) {
				maybeShowDataPopup(e);
			}

			public void mouseEntered(MouseEvent e) {
				maybeShowDataPopup(e);
			}

			public void mouseExited(MouseEvent e) {
				maybeShowDataPopup(e);
			}
	        public void mousePressed(MouseEvent e) {
				maybeShowDataPopup(e);
	        }
	        public void mouseReleased(MouseEvent e) {
				maybeShowDataPopup(e);
	        }
	    });
		dataPopupMenu=getDataPopupMenu();
		return dataTable;
	}

	/**
	 * Show the data popup menu if appropriate
	 * @param e mouse event
	 */
	private void maybeShowDataPopup(MouseEvent e) {
		if(e.isPopupTrigger()){
			popupX=e.getX();
			popupY=e.getY();
			if(dataList.getValueAt(dataTable.rowAtPoint(new Point(popupX, popupY)),1)!=""){
				dataPopupMenuSet=Integer.parseInt((dataList.getValueAt(dataTable.rowAtPoint(new Point(popupX, popupY)),1)).toString());
				dataPopupMenu.show(e.getComponent(), popupX, popupY);
			}
		}				
	}
	
	/**
	 * Associates the table of data files with the DataList object and sets column headings, etc.
	 * @param dList the DataList to be displayed in the table
	 */
	void setDataList(DataList dList){
		dataList=dList;
		dataTable.setModel(dataList);
//		dataList.setPanel(dataTab);
		String h[]={"Use","Set no.","File","Label"};
		for(int i=0;i<4;i++){
			dataTable.getColumnModel().getColumn(i).setHeaderValue(h[i]);
		}
	    JCheckBox checkBox = new JCheckBox();
	    checkBox.setBackground(UIManager.getColor("Table.selectionBackground"));
		checkBox.setHorizontalAlignment(JCheckBox.CENTER);
	    TableCellEditor checkBoxEditor = new DataListEditor(checkBox,dataTab);
	    TableColumn column=dataTable.getColumnModel().getColumn(0);
	    column.setCellEditor(checkBoxEditor);
	    JTextField textField = new JTextField();
	    textField.setBorder(new EmptyBorder(0,0,0,0));
	    TableCellEditor textFieldEditor = new DataListEditor(textField,dataTab);
	    column=dataTable.getColumnModel().getColumn(1);
	    column.setCellEditor(textFieldEditor);	    
	    setColumnWidth(dataTable,0,20,40,40);
	    setColumnWidth(dataTable,1,20,60,100);
	    setColumnWidth(dataTable,2,50,150,99999);
	}

	/**
	 * Sets the width of table columns
	 * 
	 * @param table JTable to operate on
	 * @param col column to adjust
	 * @param min minimum width
	 * @param pref preferred width
	 * @param max maximum width
	 */
	private void setColumnWidth(JTable table,int col,int min,int pref,int max){
		TableColumn column=table.getColumnModel().getColumn(col);
	    column.setPreferredWidth(pref);
	    column.setMinWidth(min);
	    column.setMaxWidth(max);	  		
	}

//	public void refreshPlots(){
//		plotFrame.refresh();
//		if(!plotFrame.isVisible()){					
//			plotFrame.setVisible(true);
//		}
//	}

	/**
	 * This method initializes cycleSpinner	
	 * 	
	 * @return javax.swing.JSpinner	
	 */
	private JSpinner getCycleSpinner() {
		if (cycleSpinner == null) {
			cycleSpinnerModel=new SpinnerNumberModel(0,0,0,1);
			cycleSpinner = new JSpinner(cycleSpinnerModel);
			cycleSpinner.addChangeListener(new ChangeListener(){
				public void stateChanged(ChangeEvent e) {
	                  //System.out.println("Source: " + e.getSource());
	                  
	                  // check if the spinner value is different to the cycle we're displaying
	                  // if it is, we need to do something
	                  if(cycleSpinnerModel.getNumber().intValue() != dispCycle){
	                	  dispCycle=cycleSpinnerModel.getNumber().intValue();
	                	  //System.out.println("Value changed");
	                	  cycles.get(dispCycle).revert();
	                	  fit.fireTableDataChanged();
	                	  Common_two two_=Core.getTwo_();
		                  int cycle=two_.getNyc();
	                	  Core.talk("OFF");
	                	  Core.talk("R");
	                	  Core.talk("ON");
	                	  // decrement cycle number
	                	  two_.setNyc(cycle);
	                	  updateOutput();
	                	  PlotFrame.refreshAll(false);
	                  }
	               }
				});
			}
		return cycleSpinner;
	}

	/**
	 * This method initializes outputPane	
	 * 	
	 * @return javax.swing.JTextPane	
	 */
	private JTextPane getOutputPane() {
		if (outputPane == null) {
			outputPane = new JTextPane();
			outputPane.setOpaque(false);
			outputPane.setEnabled(false);
			outputPane.setDisabledTextColor(Color.black);
		}
		return outputPane;
	}

	/**
	 * This method initializes ffButton	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getFfButton() {
		if (ffButton == null) {
			ffButton = new JButton();
			ffButton.setText(">>");
			ffButton.setToolTipText("Jump to the most recent cycle or undo any changes");
			ffButton.addActionListener(new ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					// jump back to the most recent cycle
					// will also undo any edits made in the fitting window
					Common_jfit jfit_=Core.getJfit_();
					int inFit=jfit_.getInfit();
					if(inFit==1 || indicateModel==true){
						dispCycle=cycles.size()-1;
						cycles.get(dispCycle).revert();
						fit.fireTableDataChanged();
						Common_two two_=Core.getTwo_();
						Core.talk("OFF");
						Core.talk("R");
						Core.talk("ON");
						// decrement cycle number
						two_.setNyc(dispCycle);
						cycleSpinnerModel.setValue(dispCycle);
						updateOutput();
						PlotFrame.refreshAll(false);
					}
				}
			});
		}
		return ffButton;
	}

	/**
	 * Make the "Cancel" button in an optionPane the default button, i.e. the one highlighted 
	 * and activated if Enter is pressed 
	 * @param dialog the dialog containing the optionPane
	 * @param optionPane the optionPane
	 */
	static void setCancelDefault(JDialog dialog, JOptionPane optionPane){
		// All this to set the default button, thanks to a bug with certain look and feels.
		int cnt=optionPane.getComponentCount();
		JButton button=null;
		for(int i=0;i<cnt;i++){
			Component c = optionPane.getComponent(i);
			if (c.getClass().getName().equals("javax.swing.JPanel")){
				try {
					JButton b= (JButton)((JPanel)c).getComponent(1);
					if(b.getText().equals("Cancel")){
						button=b;
					}
				}
				catch(Exception e) {}
			}
		}
		if(button!=null){
			dialog.getRootPane().setDefaultButton(button);
			button.requestFocus();			
		}
	}

	/**
	 * This method initializes addModelButton	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getAddModelButton() {
		if (addModelButton == null) {
			addModelButton = new JButton();
			addModelButton.setText("Add model file...");
			addModelButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					//System.out.println("Add file button");
					modelFileChooser.setDialogTitle("Add model file");
					int fcRtn=modelFileChooser.showDialog(jContentPane,"Add");
					if (fcRtn == JFileChooser.APPROVE_OPTION) {
			            File file = modelFileChooser.getSelectedFile();
			            System.out.println("Opening model file: " + file.getAbsoluteFile());
			            try {
			            	modelList.addlsq(file.getAbsoluteFile().toString());
			            	//modelListBox.revalidate();
			            }
			            catch(IOException ex) {
							errorMessage("File could not be read");
						}
						catch(Exception ex) {
							errorMessage("File contains an error or is not a valid model file");
						}
			        }
//					modelList.addTest();


				}
			});	
		}
		return addModelButton;
	}

	/**
	 * Returns the fitting table displayed in this window
	 * @return FittingTable
	 */
	public FittingTable getFittingTable() {
		return fit;
	}
	
}  //  @jve:decl-index=0:visual-constraint="10,10"
