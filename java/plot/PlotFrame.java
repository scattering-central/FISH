package fish.plot;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Formatter;
import java.util.Vector;
import java.util.prefs.Preferences;

import javax.swing.ButtonGroup;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JRadioButtonMenuItem;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.title.LegendTitle;
import org.jfree.chart.title.TextTitle;
import org.jfree.data.general.DatasetChangeEvent;
import org.jfree.ui.HorizontalAlignment;
import org.jfree.ui.RectangleEdge;
import org.jfree.ui.VerticalAlignment;

import fish.Common_three;
import fish.Common_two;
import fish.Core;
import fish.DataList;
import fish.DataSet;
import fish.FishFrame;
import fish.FittingTable;
import fish.Model;
import fish.ModelList;

public class PlotFrame extends JFrame implements ItemListener, ActionListener, WindowListener {

	private FishFrame fishFrame = null;
	private ChartPanel plotPane = null;
	private JMenuBar menuBar;
	private JMenu axisMenu, seriesMenu, windowMenu;
	private JMenu plotMenu;
	private JFreeChart chart=null;  //  @jve:decl-index=0:
	private PlotData plotData=null;  //  @jve:decl-index=0:
	private SeriesList seriesList;
	private int xAxisType;
	private int yAxisType;
	private int showTable;
	private TextTable chartTable;  //  @jve:decl-index=0:
	private Font axisFont;  //  @jve:decl-index=0:
	private Font font;  //  @jve:decl-index=0:
	private Font font2;  //  @jve:decl-index=0:
	private LegendTitle legend;
	private Boolean showLegend;
	private TextTitle date;
	private SimpleDateFormat dateFormat;
	private Boolean showDate;
	
	private static Vector<PlotFrame> frameList=new Vector<PlotFrame>();
	private static int nextSet=2; // data set to plot by default in next new window
//	private NumberAxis xAxis,yAxis;
//	private LogarithmicAxis logXAxis,logYAxis;

	public PlotFrame(FishFrame fishFrame) {
		super();
		this.fishFrame=fishFrame;
		initialize();
		frameList.add(this);
		if(frameList.size()==1){
			this.setLocation(592,60);
		}
		else {
			this.setLocationByPlatform(true);
		}
		this.setVisible(true);
	}

	/**
	 * This method initializes this
	 * 
	 */
	private void initialize() {
        this.setSize(new Dimension(548, 420));
		this.setTitle("Fish plot window");
		this.setIconImage(Toolkit.getDefaultToolkit().getImage("bin/fish/fish.png"));
		// axis types: 0=linear, 1=log
		Preferences prefs = Preferences.userNodeForPackage(this.getClass());
		xAxisType = prefs.getInt("XAxisType",0);
		yAxisType = prefs.getInt("YAxisType",0);
		showTable = prefs.getInt("ShowTable", 0);
		showDate = prefs.getBoolean("ShowDate", false);
		showLegend = prefs.getBoolean("ShowLegend", true);		
		this.setContentPane(getPlotPane());
		this.setJMenuBar(getMenubar());
		this.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
		this.addWindowListener(this);
		seriesList=new SeriesList(this,plotData);
	}

	/**
	 * This method initializes plotPane	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getPlotPane() {
		if (plotPane == null) {
			int maxSet=fish.DataList.maxSet();
			if(maxSet<1){
				maxSet=1;
			}
			if(nextSet<maxSet){
				maxSet=nextSet;
				nextSet++;
			}
			plotData = new PlotData(this,maxSet);
			chart = ChartFactory.createXYLineChart("","","",
					plotData,PlotOrientation.VERTICAL,true,false,false);
			XYErrorSeriesRenderer renderer = new XYErrorSeriesRenderer();
			chart.getXYPlot().setRenderer(renderer);
//			renderer.setSeriesLinesVisible(0, true);
			plotData.setChart(chart);
			plotData.setDefaultSeriesStyles(renderer);
			//chart.getXYPlot().getRenderer().setSeriesVisible(3, false);
			//xAxis = new NumberAxis("Q (\u212B\u207B\u00b9)");
			//logXAxis=new LogarithmicAxis("log(Q (\u212B\u207B\u00b9))");
			//yAxis = new NumberAxis("I(Q) (cm\u207B\u00b9)");
			//logYAxis=new LogarithmicAxis("log(I(Q) (cm\u207B\u00b9))");
			//logYAxis.setStrictValuesFlag(false);
			//logYAxis.setAllowNegativesFlag(false);
			//XYPlot plot=(XYPlot)chart.getPlot();
			//plot.setDomainAxis(xAxis);		
			//plot.setRangeAxis(yAxis);

			axisFont = new Font("courier",Font.PLAIN,12);
			chart.getXYPlot().getRangeAxis().setLabelFont(axisFont);
			chart.getXYPlot().getDomainAxis().setLabelFont(axisFont);
			
			plotPane = new ChartPanel(chart);
			this.setBackground(Color.white);
			plotPane.setBackground(Color.white);
			chart.setBackgroundPaint(Color.white);
			setAxisLabels();
//			chart.getXYPlot().getRenderer().setSeriesVisibleInLegend(3,false);
//			plotPane.setLayout(new BorderLayout());

			legend = chart.getLegend();
			
			chartTable = new TextTable("");
			font = new Font("monospaced",Font.PLAIN,7);
			font2 = new Font("monospaced",Font.PLAIN,5);			
			
			dateFormat = new SimpleDateFormat("EEE dd-MMM-yy HH:mm:ss");
			date = new TextTitle(dateFormat.format(new Date()),font);
			date.setHorizontalAlignment(HorizontalAlignment.RIGHT);
			
			if(!showLegend) {
				chart.removeLegend();
			}
			if(showTable > 0) {
				showTable();
			}
			if(showDate) {
				showDate();
			}
		}
		return plotPane;
	}

	/**
	 * Shows the table of parameters next to the plot
	 *
	 */
	private void showTable() {
		updateTable();
		
		chartTable.setPosition(RectangleEdge.LEFT);
		chartTable.setHorizontalAlignment(HorizontalAlignment.LEFT);
		if(showTable==2){
			chartTable.setFont(font2);			
			chartTable.setTextAlignment(HorizontalAlignment.LEFT);
		}
		else {
			chartTable.setFont(font);			
			chartTable.setTextAlignment(HorizontalAlignment.CENTER);
		}
		chartTable.setVerticalAlignment(VerticalAlignment.TOP);
		boolean show=true;
		for(int i=0; i<chart.getSubtitleCount(); i++){
			if(chart.getSubtitle(i).equals(chartTable)){
				show=false;
			}
		}
		if(show) {
			chart.addSubtitle(chartTable);
		}
	}

	/**
	 * Shows or hides the date and time on the plot window
	 *
	 */
	private void showDate() {
		if(showDate){
			setDate();
			boolean show=true;
			for(int i=0; i<chart.getSubtitleCount(); i++){
				if(chart.getSubtitle(i).equals(date)){
					show=false;
				}
			}
			if(show) {
				chart.addSubtitle(date);
			}			
		}
		else {
			chart.removeSubtitle(date);
		}
	}
	
	/**
	 * Sets the date subtitle to the current date and time
	 *
	 */
	private void setDate() {
		date.setText(dateFormat.format(new Date()));
	}
	
	/**
	 * Removes the table of parameters from the plot
	 *
	 */
	private void hideTable() {
		chart.removeSubtitle(chartTable);
	}

	/** 
	 * Updates the table of parameters on the plot
	 *
	 */
	private void updateTable() {
		ModelList models=fishFrame.getModelList();
		Model model=models.getModel();
		if(model != null){
			FittingTable table = fishFrame.getFittingTable();
			Common_two two_ = Core.getTwo_();
			String text = "";
			
			if(showTable == 1){
				int np = two_.getNp();
				text = model.toString();
				if(text.length()>30){
					text=text.substring(0,29)+"\u2026";
				}
				text+="\n";

				// note: parameter names are ultimately fetched from model.getParameterLabel which
				// supports long labels. The lines below either trim or pad labels to make them
				// 12 characters for display on the plot so as to future-proof the routine in case
				// new model formats allow long labels.
				for(int i=0; i<np; i++) {
					String label=table.getValueAt(i, 2).toString();
					String value=table.getValueAt(i, 3).toString();

					label=stringLen(label,12);
					if(value.length()==12){
						value=" "+value;
					}

					text+=label+": "+value+"\n";
				}
			}
			
			else if(showTable == 2){
				Common_three three_ = Core.getThree_();
				int nt=two_.getNt();
				int np=two_.getNp();
				int ns=two_.getNs();
				int nc=two_.getNc();
				int nn=two_.getNn();
				Formatter f = new Formatter();
				f.format("T%3d P%3d S%3d C%3d N%3d\n", nt,np,ns,nc,nn);
				
				int iw=two_.getIw();
				int ik=two_.getIk();
				int ip=two_.getIp();
				int ms=two_.getMs();
				int iy=two_.getIy();
				int[] ls=two_.getLs();
				int npred=two_.getNpred();
				int ndat=two_.getNdat();
				int nyc=two_.getNyc();
				int npr=two_.getNpr();
			    //  WRITE(IJ,52)(LS(I),I=17,28)
			    //  52 FORMAT(' W',I3,' K',I3,' IP',I2,' MS',I2,' IY',I2,'  ',
			    //    *I3,'  ',I3,' XB',I2,8I5)
				f.format("W%3d K%3d IP%2d MS%2d IY%2d   %3d   %3d  XB%2d %2d %5d %5d %5d\n"
						, iw,ik,ip,ms,iy,ls[0],ls[1],ls[2],npred,ndat,nyc,npr);
				
				text=f.toString();
				String modelname=model.toString();
				if(modelname.length()>80){
					modelname=modelname.substring(0,79)+"\u2026";
				}
				text+=modelname+"\n";
				
				f.close();
				
				int[] lm=two_.getLm();
				int[] ltyp=two_.getLtyp();
				float[] v=two_.getV();
				float[] esd=two_.getEsd();
				float[] ps=two_.getPs();
				float[] dv=two_.getDv();
				
				for(int i=0;i<np;i++){
					f=new Formatter();
					f.format("%3d%3d%3d", i,lm[i],ltyp[i]);
					String label=table.getValueAt(i, 2).toString();
					label=stringLen(label,12);
					text+=f.toString();
					text+=label;
					f.close();
					
					f=new Formatter();
					f.format("%14.6e%13.3e%6.1f%10.2e",v[i],esd[i],ps[i],dv[i]);
					text+=f.toString()+"\n";
					f.close();
				}
				
				int sets = DataList.maxSet();
//				int[] jd=three_.getJd();
				float[] fit=three_.getFit();
				for(int i=0;i<sets;i++){
					if(DataList.isSetUsed(i)){
						/*
				      IF(J.GT.0)WRITE(IJ,301)I,J,(LAB(L,J),L=1,3),JC(I),JB(I),JY(I),FIT(I)
				      320 IF(J.EQ.-1)WRITE(IJ,302)I,J
				      301 FORMAT(1X,2I3,3A4,'  CALC',I2,' BKG',I2,' POL',I2,
						 *'  SSE=',1PE10.3)
				      302 FORMAT(1X,2I3,' skip this set in model')
						 */
							f=new Formatter();
							f.format("%3d %s  SSE=%10.3e\n", 
									i+1, stringLen(DataList.getDataSet(i).getLabel(),20), fit[i]);
							text+=f.toString();
							f.close();
					}
					else {
						f=new Formatter();
						f.format("%3d   skip this set in model\n", i+1);
						text+=f.toString();
						f.close();
					}
				}
				
				text+=String.format(" XDWE= %10.3e  SSE= %10.3e\n",three_.getXdwe(),three_.getSse());
				
				// constraints
				if(nc > 0) {
					for(int i=0;i<nc;i++) {
						String t=Core.conop2(i+1);
						text=text+t+"\n";
					}
				}
				
				// numerical constants
				if(nn > 0) {
					float[] con = two_.getCon();
					for(int i=0;i<nn;i++) {
						text=text+String.format("%10.3e  ", con[i]);
						if((i%4)==3){
							text+="\n";
						}
					}
				}
				
			}
			
			chartTable.setText(text);
		}
		
		setDate();
	}

	/**
	 * Returns a string of a particular lengh by either padding or truncating
	 * the supplied string as necessary.
	 * @param string input string
	 * @param length the required length
	 * @return string of the desired length
	 */
	private String stringLen(String string, int length) {
		if(string.length()>length){
			string=string.substring(0,length);
		}
		int len=string.length();
		if(len<length){
			String blank="                                                                   ";
			string=string+blank.substring(0, length-len);
		}
		return string;
	}
	
	void setAxisLabels(){
		ValueAxis xAxis=chart.getXYPlot().getDomainAxis();
		ValueAxis yAxis=chart.getXYPlot().getRangeAxis();
		
		// label axes with units of the first set in use
		String qUnit=DataSet.defaultQUnit;
		String iUnit=DataSet.defaultIUnit;
		if(DataList.maxSet()>0){
			try {
				DataSet firstSet=DataList.getDataSet(plotData.getSeries(0).getSet()-1);
				qUnit=firstSet.getQUnit();
				iUnit=firstSet.getIUnit();
			}
			catch(NullPointerException ex) {
				System.out.println("Can't set axis labels to first data set units");
			}
		}
		
		switch(xAxisType){
		case 1:
			xAxis.setLabel("log(Q ("+qUnit+"))");
			break;
		default:
			xAxis.setLabel("Q ("+qUnit+")");
			break;
		}
		
		switch(yAxisType){
		case 1:
			yAxis.setLabel("log(I(Q) ("+iUnit+"))");
			break;
		default:
			yAxis.setLabel("I(Q) ("+iUnit+")");
			break;
		}
		
		xAxis.setAutoRange(true);
		yAxis.setAutoRange(true);

	}
	
	
	/**
	 * This method initializes menubar	
	 * 	
	 * @return javax.swing.JMenuBar	
	 */
	private JMenuBar getMenubar() {
		if (menuBar == null) {
			menuBar = new JMenuBar();
			menuBar.add(getAxisMenu());
			menuBar.add(getSeriesMenu());
			menuBar.add(getPlotMenu());
			menuBar.add(getWindowMenu());
		}
		return menuBar;
	}

	/**
	 * This method initializes axisMenu	
	 * 	
	 * @return javax.swing.JMenu	
	 */
	private JMenu getAxisMenu() {
		if (axisMenu == null) {
			axisMenu = new JMenu();
			axisMenu.setText("Axis");
			JMenu menu;
			ButtonGroup group;
			JRadioButtonMenuItem rbItem;
			JMenuItem item;
			
			menu=new JMenu("X-axis");
			group=new ButtonGroup();
			rbItem=new JRadioButtonMenuItem("Linear");
			rbItem.setName("x_lin");
			rbItem.addItemListener(this);
			if(xAxisType==0) { rbItem.setSelected(true); }
			group.add(rbItem);
			menu.add(rbItem);
			rbItem=new JRadioButtonMenuItem("Logarithmic");
			rbItem.setName("x_log");
			rbItem.addItemListener(this);
			if(xAxisType==1) { rbItem.setSelected(true); }
			group.add(rbItem);
			menu.add(rbItem);
			axisMenu.add(menu);
			
			menu=new JMenu("Y-axis");
			group=new ButtonGroup();
			rbItem=new JRadioButtonMenuItem("Linear");
			rbItem.setName("y_lin");
			rbItem.addItemListener(this);
			if(yAxisType==0) { rbItem.setSelected(true); }
			group.add(rbItem);
			menu.add(rbItem);
			rbItem=new JRadioButtonMenuItem("Logarithmic");
			rbItem.setName("y_log");
			rbItem.addItemListener(this);
			if(yAxisType==1) { rbItem.setSelected(true); }
			group.add(rbItem);
			menu.add(rbItem);
			axisMenu.add(menu);
			
			axisMenu.addSeparator();
			
			item=new JMenuItem("Save As Defaults");
			item.setName("saveDefaultAxis");
			item.addActionListener(this);
			axisMenu.add(item);
		}
		return axisMenu;
	}	
	
	private JMenu getSeriesMenu() {
		if (seriesMenu == null) {
			seriesMenu = new JMenu();
			seriesMenu.setText("Series");
			JMenuItem item;

			item=new JMenuItem("Edit Series...");
			item.setName("editSeries");
			item.addActionListener(this);
			seriesMenu.add(item);			
		}
		return seriesMenu;
	}

	private JMenu getWindowMenu() {
		if (windowMenu == null) {
			windowMenu = new JMenu();
			windowMenu.setText("Window");
			JRadioButtonMenuItem rbitem;
			ButtonGroup group=new ButtonGroup();

			rbitem=new JRadioButtonMenuItem("No Parameter Table");
			rbitem.setName("noTable");
			rbitem.addItemListener(this);
			if(showTable==0) { rbitem.setSelected(true); }
			group.add(rbitem);
			windowMenu.add(rbitem);	
			rbitem=new JRadioButtonMenuItem("Show Parameter Table");
			rbitem.setName("showTable");
			rbitem.addItemListener(this);
			if(showTable==1) { rbitem.setSelected(true); }
			group.add(rbitem);
			windowMenu.add(rbitem);
			rbitem=new JRadioButtonMenuItem("Show Full Model Definition");
			rbitem.setName("showModel");
			rbitem.addItemListener(this);
			if(showTable==2) { rbitem.setSelected(true); }
			group.add(rbitem);
			windowMenu.add(rbitem);
			
			windowMenu.addSeparator();
			
			JCheckBoxMenuItem cbItem = new JCheckBoxMenuItem("Show Date and Time");
			cbItem.setName("showDate");
			cbItem.addActionListener(this);
			if(showDate) { cbItem.setSelected(true); }
			windowMenu.add(cbItem);
			
			cbItem = new JCheckBoxMenuItem("Show Legend");
			cbItem.setName("showLegend");
			cbItem.addActionListener(this);
			if(showLegend) { cbItem.setSelected(true); }
			windowMenu.add(cbItem);	
			
			windowMenu.addSeparator();
			
			JMenuItem item=new JMenuItem("Save As Defaults");
			item.setName("saveDefaultWindow");
			item.addActionListener(this);
			windowMenu.add(item);
		}
		return windowMenu;		
	}

	private JMenu getPlotMenu(){
		// Copy items from the JFreeChart popup menu and add them to the menubar
		// for those people who don't read the instruction to find out how to save/print!
		// For some reason this removes the said items from the popup menu, so leave the
		// zoom options on the popup menu (to save having a 1-pixel popup menu!) 
		if(plotMenu == null){
			JPopupMenu pMenu = plotPane.getPopupMenu();
			Component[] c = pMenu.getComponents();
//			int n = pMenu.getComponentCount();
			plotMenu = new JMenu();
			JMenu nullMenu = new JMenu(); // to hide separators we don't want
			plotMenu.setText("Plot");
			for(int i=0;i<6;i++){
				if(c[i].getClass().getName().equals("javax.swing.JPopupMenu$Separator")){
					nullMenu.add(c[i]);
				} else {
					plotMenu.add(c[i]);
				}
			}
		}
		return plotMenu;
	}
	
	/**
	 * Refreshes the plot window
	 *
	 */
	public void refresh() {
		plotData.resetAllLimits();
		if(showTable > 0) {
			updateTable();
		}
		else if(showDate) {
			setDate();
		}
		chart.getPlot().datasetChanged(new DatasetChangeEvent(this, plotData));
	}

	/**
	 * Refresh all plot windows and open if not already open
	 * @param boolean setAxisLabels whether or not to reset the labels on the axes
	 */
	public static void refreshAll(boolean setAxisLabels) {
		int count=frameList.size();
		if(count>0){
			for(int i=0;i<count;i++)
			{
				PlotFrame frame=frameList.get(i);
				if(setAxisLabels){
					frame.setAxisLabels();
				}
				frame.refresh();
				if(!frame.isVisible()){
					frame.setVisible(true);
				}
			}
		}
	}

	public static void allToTop() {
		int count=frameList.size();
		if(count>0){
			for(int i=0;i<count;i++)
			{
				PlotFrame frame=frameList.get(i);
				if(!frame.isVisible()){
					frame.setVisible(true);
				}
				frame.toFront();
			}
		}
	}
	
	public void itemStateChanged(ItemEvent e) {
		JMenuItem mi = (JMenuItem)(e.getSource());
		boolean selected =(e.getStateChange() == ItemEvent.SELECTED);
		if(selected){
			String name=mi.getName();
			//XYPlot plot=(XYPlot)chart.getPlot();
			if(name.equals("x_log")){
				xAxisType=1;
				setAxisLabels();
				refresh();
			}
			if(name.equals("x_lin")){
				xAxisType=0;
				setAxisLabels();
				refresh();
			}			
			if(name.equals("y_log")){
				yAxisType=1;
				setAxisLabels();
				refresh();
			}
			if(name.equals("y_lin")){
				yAxisType=0;
				setAxisLabels();
				refresh();
			}	
			
			if(name.equals("noTable")){
				showTable=0;
				hideTable();
			}
			if(name.equals("showTable")){
				showTable=1;
				showTable();
			}
			if(name.equals("showModel")){
				showTable=2;
				showTable();
			}
		}
		//System.out.println(mi.getName()+" "+selected);
	}
	
	public void actionPerformed(ActionEvent e) {
		String name=((Component)e.getSource()).getName();
		if(name.equals("editSeries")) {
			seriesList.setLocationRelativeTo(this);
			seriesList.setVisible(true);
		}
		else if(name.equals("saveDefaultAxis")) {
			Preferences prefs = Preferences.userNodeForPackage(this.getClass());
			prefs.putInt("XAxisType", xAxisType);
			prefs.putInt("YAxisType", yAxisType);
		}
		else if(name.equals("saveDefaultWindow")) {
			Preferences prefs = Preferences.userNodeForPackage(this.getClass());
			prefs.putInt("ShowTable", showTable);
			prefs.putBoolean("ShowDate", showDate);
			prefs.putBoolean("ShowLegend", showLegend);		
		}		
		else if(name.equals("showDate")) {
			JCheckBoxMenuItem i = (JCheckBoxMenuItem)(e.getSource());
			if(i.isSelected()){
				showDate=true;				
			}
			else {
				showDate=false;
			}
			showDate();
		}
		else if(name.equals("showLegend")) {
			JCheckBoxMenuItem i = (JCheckBoxMenuItem)(e.getSource());
			if(i.isSelected()){
				showLegend=true;
				if(chart.getLegend()==null){
					chart.addLegend(legend);
				}
			}
			else {
				showLegend=false;
				chart.removeLegend();
			}
		}
//		if(name.equals("showTable")) {
//			JCheckBoxMenuItem i = (JCheckBoxMenuItem)(e.getSource());
//			if(i.isSelected()){
//				showTable=1;				
//				showTable();
//			}
//			else {
//				showTable=0;
//				hideTable();
//			}
//		}
	}

	// window listener methods
	public void windowActivated(WindowEvent arg0) {
	}

	public void windowClosed(WindowEvent e) {
	}

	public void windowClosing(WindowEvent e) {			
		frameList.remove(this);
		seriesList.dispose();
		this.dispose();
	}

	public void windowDeactivated(WindowEvent arg0) {
	}

	public void windowDeiconified(WindowEvent arg0) {
	}

	public void windowIconified(WindowEvent arg0) {
	}

	public void windowOpened(WindowEvent arg0) {
	}
	// end window listener methods
	
	public int getXAxisType(){
		return xAxisType;
	}
	
	public int getYAxisType(){
		//ValueAxis axis;
		//XYPlot plot=(XYPlot)chart.getPlot();
		//axis=plot.getRangeAxis();
		//if(axis.getClass().equals(logYAxis.getClass())){
		//	return 1;
		//}
		//return 0;
		return yAxisType;
	}
}  //  @jve:decl-index=0:visual-constraint="10,10"

