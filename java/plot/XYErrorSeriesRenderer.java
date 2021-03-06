package fish.plot;

import java.awt.BasicStroke;
import java.awt.Graphics2D;
import java.awt.Paint;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;

import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.event.RendererChangeEvent;
import org.jfree.chart.plot.CrosshairState;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.PlotRenderingInfo;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYItemRendererState;
import org.jfree.data.xy.IntervalXYDataset;
import org.jfree.data.xy.XYDataset;
import org.jfree.ui.RectangleEdge;
import org.jfree.util.BooleanList;

/**
 * A line and shape renderer that can display error bars for
 * selected series only.
 */
public class XYErrorSeriesRenderer extends XYErrorRenderer {

    /** A flag that controls whether or not all the x-error bars are drawn. 
     *  Overrides the per-series flags
     */	
    private boolean drawXError;
    
    /** A flag that controls whether or not all the y-error bars are drawn.
     *  Overrides the per-series flags
     */
    private boolean drawYError;
	
    /** A list that controls whether or not the x-error bars are drawn. */
    private BooleanList seriesDrawXError;
    
    /** A list that controls whether or not the y-error bars are drawn. */
    private BooleanList seriesDrawYError;
    
    /**
     * Creates a new <code>XYErrorSeriesRenderer</code> instance.
     */
    public XYErrorSeriesRenderer() {
    	super();
    	this.seriesDrawXError=new BooleanList();
    	this.seriesDrawYError=new BooleanList();
    	
    	// prevent the superclass from drawing error bars
    	super.setDrawXError(false);
    	super.setDrawYError(false); 	
    }
    
    /**
     * Returns the flag that controls whether or not the renderer draws all error
     * bars for the x-values.
     * 
     * @return A boolean.
     * 
     * @see #setDrawXError(boolean)
     */
    public boolean getDrawXError() {
        return this.drawXError;
    }
    
    /**
     * Sets the flag that controls whether or not the renderer draws all error
     * bars for the x-values and, if the flag changes, sends a 
     * {@link RendererChangeEvent} to all registered listeners.
     *
     * @param draw  the flag value.
     * 
     * @see #getDrawXError()
     */
    public void setDrawXError(boolean draw) {
        if (this.drawXError != draw) {
            this.drawXError = draw;
            this.notifyListeners(new RendererChangeEvent(this));
        }
    }
    
    /**
     * Returns the flag that controls whether or not the renderer draws all error
     * bars for the y-values.
     * 
     * @return A boolean.
     * 
     * @see #setDrawYError(boolean)
     */
    public boolean getDrawYError() {
        return this.drawYError;
    }
    
    /**
     * Sets the flag that controls whether or not the renderer draws all error
     * bars for the y-values and, if the flag changes, sends a 
     * {@link RendererChangeEvent} to all registered listeners.
     *
     * @param draw  the flag value.
     * 
     * @see #getDrawYError()
     */
    public void setDrawYError(boolean draw) {
        if (this.drawYError != draw) {
            this.drawYError = draw;
            notifyListeners(new RendererChangeEvent(this));
        }
    } 
    
    /**
     * Returns the flag that controls whether or not the renderer draws error
     * bars for the x-values for the given series.
     * @param series
     * @return A boolean.
     */
    public boolean getSeriesDrawXError(int series) {
    	Boolean result=this.seriesDrawXError.getBoolean(series);
    	if(result != null){
    		return result.booleanValue();
    	}
        return false;
    }
    
    /**
     * Sets the flag that controls whether or not the renderer draws error
     * bars for the x-values in this series, and, if the flag changes, sends a 
     * {@link RendererChangeEvent} to all registered listeners.
     *
	 * @param series series to change
     * @param draw  the flag value.
     */
    public void setSeriesDrawXError(int series,boolean draw) {
    	Boolean old=this.seriesDrawXError.getBoolean(series);
        if (old==null || old != draw) {
            this.seriesDrawXError.setBoolean(series,draw);
            this.notifyListeners(new RendererChangeEvent(this));
        }
    }

    /**
     * Returns the flag that controls whether or not the renderer draws error
     * bars for the y-values for the given series.
     * @param series
     * @return A boolean.
     */
    public boolean getSeriesDrawYError(int series) {
    	Boolean result=this.seriesDrawYError.getBoolean(series);
    	if(result != null){
    		return result.booleanValue();
    	}
        return false;
    }
    
    /**
     * Sets the flag that controls whether or not the renderer draws error
     * bars for the y-values in this series, and, if the flag changes, sends a 
     * {@link RendererChangeEvent} to all registered listeners.
     *
	 * @param series series to change
     * @param draw  the flag value.
     */
    public void setSeriesDrawYError(int series,boolean draw) {
    	Boolean old=this.seriesDrawYError.getBoolean(series);
        if (old==null || old != draw) {
            this.seriesDrawYError.setBoolean(series,draw);
            this.notifyListeners(new RendererChangeEvent(this));
            //System.out.println(series+" "+draw);
        }
    }
 
    /**
     * Draws the visual representation for one data item.
     * 
     * @param g2  the graphics output target.
     * @param state  the renderer state.
     * @param dataArea  the data area.
     * @param info  the plot rendering info.
     * @param plot  the plot.
     * @param domainAxis  the domain axis.
     * @param rangeAxis  the range axis.
     * @param dataset  the dataset.
     * @param series  the series index.
     * @param item  the item index.
     * @param crosshairState  the crosshair state.
     * @param pass  the pass index.
     */
    public void drawItem(Graphics2D g2, XYItemRendererState state, 
            Rectangle2D dataArea, PlotRenderingInfo info, XYPlot plot, 
            ValueAxis domainAxis, ValueAxis rangeAxis, XYDataset dataset, 
            int series, int item, CrosshairState crosshairState, int pass) {

        if (pass == 0 && dataset instanceof IntervalXYDataset 
                && getItemVisible(series, item)) {
            IntervalXYDataset ixyd = (IntervalXYDataset) dataset;
            PlotOrientation orientation = plot.getOrientation();
            Paint errorPaint=super.getErrorPaint();
            double capLength=super.getCapLength();
            if (this.drawXError && this.getSeriesDrawXError(series)) {
                // draw the error bar for the x-interval
                double x0 = ixyd.getStartXValue(series, item);
                double x1 = ixyd.getEndXValue(series, item);
                double y = ixyd.getYValue(series, item);
                RectangleEdge edge = plot.getDomainAxisEdge();
                double xx0 = domainAxis.valueToJava2D(x0, dataArea, edge);
                double xx1 = domainAxis.valueToJava2D(x1, dataArea, edge);
                double yy = rangeAxis.valueToJava2D(y, dataArea, 
                        plot.getRangeAxisEdge());
                Line2D line;
                Line2D cap1 = null;
                Line2D cap2 = null;
                double adj = capLength / 2.0;
                if (orientation == PlotOrientation.VERTICAL) {
                    line = new Line2D.Double(xx0, yy, xx1, yy);
                    cap1 = new Line2D.Double(xx0, yy - adj, xx0, yy + adj);
                    cap2 = new Line2D.Double(xx1, yy - adj, xx1, yy + adj);
                }
                else {  // PlotOrientation.HORIZONTAL
                    line = new Line2D.Double(yy, xx0, yy, xx1);
                    cap1 = new Line2D.Double(yy - adj, xx0, yy + adj, xx0);
                    cap2 = new Line2D.Double(yy - adj, xx1, yy + adj, xx1);
                }
                g2.setStroke(new BasicStroke(1.0f));
                if (errorPaint != null) {
                    g2.setPaint(errorPaint);    
                }
                else {
                    g2.setPaint(getItemPaint(series, item));
                }
                g2.draw(line);
                g2.draw(cap1);
                g2.draw(cap2);
            }
            if (this.drawYError && this.getSeriesDrawYError(series)) {
                // draw the error bar for the y-interval
                double y0 = ixyd.getStartYValue(series, item);
                double y1 = ixyd.getEndYValue(series, item);
                double x = ixyd.getXValue(series, item);
                RectangleEdge edge = plot.getRangeAxisEdge();
                double yy0 = rangeAxis.valueToJava2D(y0, dataArea, edge);
                double yy1 = rangeAxis.valueToJava2D(y1, dataArea, edge);
                double xx = domainAxis.valueToJava2D(x, dataArea, 
                        plot.getDomainAxisEdge());
                Line2D line;
                Line2D cap1 = null;
                Line2D cap2 = null;
                double adj = capLength / 2.0;
                if (orientation == PlotOrientation.VERTICAL) {
                    line = new Line2D.Double(xx, yy0, xx, yy1);
                    cap1 = new Line2D.Double(xx - adj, yy0, xx + adj, yy0);
                    cap2 = new Line2D.Double(xx - adj, yy1, xx + adj, yy1);
                }
                else {  // PlotOrientation.HORIZONTAL
                    line = new Line2D.Double(yy0, xx, yy1, xx);
                    cap1 = new Line2D.Double(yy0, xx - adj, yy0, xx + adj);
                    cap2 = new Line2D.Double(yy1, xx - adj, yy1, xx + adj);
                }
                g2.setStroke(new BasicStroke(1.0f));
                if (errorPaint != null) {
                    g2.setPaint(errorPaint);    
                }
                else {
                    g2.setPaint(getItemPaint(series, item));
                }
                g2.draw(line);                    
                g2.draw(cap1);                    
                g2.draw(cap2);                    
            }
        }
        super.drawItem(g2, state, dataArea, info, plot, domainAxis, rangeAxis, 
                dataset, series, item, crosshairState, pass);
    }
    
}