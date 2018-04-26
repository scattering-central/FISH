package fish.plot;

import java.util.Vector;

import javax.swing.event.TableModelListener;
import javax.swing.table.TableModel;

import org.jfree.chart.JFreeChart;
import org.jfree.data.xy.AbstractXYDataset;
import org.jfree.data.xy.IntervalXYDataset;

public class PlotData extends AbstractXYDataset implements IntervalXYDataset, TableModel {

	private Vector<Series> plotSeries;
	private PlotFrame frame=null;
//	private JFreeChart chart=null;
	private XYErrorSeriesRenderer renderer=null;
//	private static final int maxSets=fish.CoreConstants.MF;
//	private static final int maxWorkspace=fish.CoreConstants.MW;
//	private static fish.Common_one one_;
//	private static fish.Common_two two_;
//	private static fish.Common_three three_;
	
	PlotData(PlotFrame frame) {
		this.frame=frame;
		plotSeries=new Vector<Series>();
//		one_=fish.Core.getOne_();
//		two_=fish.Core.getTwo_();
//		three_=fish.Core.getThree_();
	}

	PlotData(PlotFrame frame,int set) {
		this(frame);
		plotSeries.add(new Series(0,set));
		plotSeries.add(new Series(1,set));
		plotSeries.add(new Series(2,set,-5));
		plotSeries.add(new Series(3,set,-5));	
	}

	void setChart(JFreeChart chart){
		// I think this is no longer needed now that renderers are passed directly and stored in each series
		//this.chart=chart; 
	}

	void addSeries(){
		plotSeries.add(new Series(1,fish.DataList.maxSet()));
		int number=plotSeries.size()-1;
		plotSeries.get(number).setDefaultStyle(renderer, number);
	}
	
	/**
	 * Delete a series from the plot
	 * @param series number
	 */
	void deleteSeries(int series){
		plotSeries.remove(series);
	}
	
	/**
	 * Return a particular series
	 * @param series number
	 * @return Series object
	 */
	Series getSeries(int series){
		return plotSeries.get(series);
	}

	/**
	 * Correctly set all series numbers stored within series objects following a re-ordering
	 * (e.g. when a series is deleted)
	 *
	 */
	void resetSeriesStyles(){
		for(int i=0;i<getSeriesCount();i++){
			plotSeries.get(i).setStyle(i);
		}		
	}
	
	/**
	 * Sets all series to their detault styles (whether they have lines, shapes, legend, etc.)
	 *
	 */
	void setDefaultSeriesStyles(XYErrorSeriesRenderer renderer){
		this.renderer=renderer;
		for(int i=0;i<getSeriesCount();i++){
			setDefaultSeriesStyle(i);
		}
	}
	
	/**
	 * Sets a series to its detault styles (whether it has lines, shapes, legend, etc.)
	 * @param series the series to set
	 */
	void setDefaultSeriesStyle(int series){
//		XYErrorSeriesRenderer renderer =(XYErrorSeriesRenderer)ch.getXYPlot().getRenderer();
		renderer.setDrawXError(false);
		renderer.setDrawYError(true);
		renderer.setSeriesVisibleInLegend(series,true);
		plotSeries.get(series).setDefaultStyle(renderer, series);
	}

	
	/**
	 * resets the min and max values for all series
	 *
	 */
	public void resetAllLimits() {
		for(int i=0;i<plotSeries.size();i++){
			plotSeries.get(i).findLimits();
		}
	}
	
	@Override
	public int getSeriesCount() {
		return plotSeries.size();
	}

	@Override
	public Comparable getSeriesKey(int series) {
		return plotSeries.elementAt(series).getKey();
//		String k=ser.getKey();
//		if(fr.getYAxisType()==1 && ser.getType()==2)
//		{
//			return(k.replace(" obs-calc", " |obs-calc|"));
//		}
//		return k;
	}

	public int getItemCount(int series) {
		//System.out.println(plotSeries.elementAt(series).getCount()+" items in series "+series);
		return plotSeries.elementAt(series).getCount();
	}

	public Number getX(int series, int item) {
		boolean log=false;
		if(frame.getXAxisType()==1){
			log=true;
		}		
		return plotSeries.elementAt(series).getX(item,log);
	}

	public Number getY(int series, int item) {
		return getYPoint(series,item,0);
	}
	
	/**
	 * Gets either the y value, or the +/- y error bar
	 * @param series series to plat
	 * @param item item within the series
	 * @param bar 0 for the data point, -1 or 1 for -/+ error
	 * @return Number
	 */
	private Number getYPoint(int series, int item,int bar) {
		boolean log=false;
		if(frame.getYAxisType()==1){
			log=true;
		}
		return plotSeries.elementAt(series).getYPoint(item, bar, log);
	}

	// no error bars for x values
	public Number getStartX(int series, int item) {
		return getX(series,item);
	}

	public double getStartXValue(int series, int item) {
		return getStartX(series,item).doubleValue();
	}
	
	public Number getEndX(int series, int item) {
		return getX(series,item);
	}

	public double getEndXValue(int series, int item) {
		return getEndX(series,item).doubleValue();
	}


	// error bars for y values
	public Number getStartY(int series, int item) {
		return getYPoint(series,item,-1);
		//return null;
	}

	public double getStartYValue(int series, int item) {
		return getStartY(series,item).doubleValue();
		//return getY(series,item).doubleValue();
	}
	
	public Number getEndY(int series, int item) {
		return getYPoint(series,item,1);
		//return null;
	}

	public double getEndYValue(int series, int item) {
		return getEndY(series,item).doubleValue();
		//return getY(series,item).doubleValue();
	}

	
	/* table model methods */
	
	public void addTableModelListener(TableModelListener arg0) {
		// TODO Auto-generated method stub
		
	}

	public Class<?> getColumnClass(int arg0) {
		return String.class;
	}

	public int getColumnCount() {
		return 3;
	}

	public String getColumnName(int col) {
		switch(col){
		case 0:
			return "Set";
		case 1:
			return "Variable";
		case 2:
			return "Label";
		}
		return "";
	}

	public int getRowCount() {
		return plotSeries.size();
	}

	public Object getValueAt(int row, int col) {
		switch(col){
		case 0:
			return plotSeries.get(row).getSet();
		case 1:
			return plotSeries.get(row).getTypeDesc();
		case 2:
			return plotSeries.get(row).getKey();
		}
		return "";
	}

	public boolean isCellEditable(int arg0, int arg1) {
		return false;
	}

	public void removeTableModelListener(TableModelListener arg0) {
		// TODO Auto-generated method stub
		
	}

	public void setValueAt(Object arg0, int arg1, int arg2) {
		// TODO Auto-generated method stub
		
	}

//	/**
//	 * Hide all series of type=3 (y=0 line for obs-calc) if using a log y-axis;
//	 * unhide if using linear axis
//	 */
//	public void hideSeries(){
//		int yAxisType=fr.getYAxisType();
//		for(int i=0;i<plotSeries.size();i++){
//			if(plotSeries.elementAt(i).getType()==3){
//				if(yAxisType==1){
//					ch.getXYPlot().getRenderer().setSeriesVisible(i, false);
//				}
//				else{
//					ch.getXYPlot().getRenderer().setSeriesVisible(i, true);
//				}
//			}
//		}
//	}
	
//	public boolean getItemVisible(int series,int item){
//		if(series==1){
//			return false;
//		}
//		return true;
//	}
	
	
	
}

