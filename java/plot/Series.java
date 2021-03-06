package fish.plot;

import java.awt.geom.Ellipse2D;

import fish.Core;

public class Series {
	private int type;
	private int set;
	private String key;
	private int min;
	private int max;
	private float yshift;
	
	boolean yerror;
	boolean lines;
	boolean shapes;
	boolean autoLabel;
	
	private XYErrorSeriesRenderer renderer;
	private int seriesNumber;
	private static final int maxWorkspace=fish.CoreConstants.MW;
	private static final fish.Common_one one_=fish.Core.getOne_();
	private static final fish.Common_three three_=fish.Core.getThree_();
	private static final fish.SWIGTYPE_p_a_1024__float q=one_.getQ();
	private static final fish.SWIGTYPE_p_a_1024__float c=one_.getC();
	private static final fish.SWIGTYPE_p_a_1024__float e=one_.getE();
	
	/**
	 * Create a new series of observed or calculated data
	 * @param type 0=obs, 1=calc, 2=obs-calc, 3=line at 0, 4=P(Q), 5=S(Q), 6=beta(Q)
	 * @param set set number
	 * @param yshift shift this series up by a constant
	 */
	Series(int type,int set,float yshift) {
		this.type=type;
		this.set=set;
		this.yshift=yshift;
		this.autoLabel=true;
		this.key=getAutoLabel();
	}

	Series(int type,int set) {
		this(type,set,0);
	}

	String getAutoLabel() {
		String key="";
		if(type==0){
			key="Set "+set+" obs";
		}
		if(type==1){
			key="Set "+set+" calc";
		}
		if(type==2){
			key="Set "+set+" obs-calc";
		}	
		if(type==3){
			key="";
		}
		if(type==4){
			key="Set "+set+" P(Q)";
		}	
		if(type==5){
			key="Set "+set+" S(Q)";
		}
		if(type==6){
			key="Set "+set+" beta(Q)";
		}			
		return key;
	}
	
	/**
	 * Return the key
	 * @return key
	 */
	public String getKey(){
		return key;
	}

	public void setKey(String key){
		this.key=key;
		if(renderer!=null){
			if(key.length()==0) {
				renderer.setSeriesVisibleInLegend(seriesNumber,false);
			}
			else {
				renderer.setSeriesVisibleInLegend(seriesNumber,true);
			}
		}
		System.out.println(key.length());
	}
	
	/**
	 * Return the number of the dataset used for this series
	 * @return set
	 */
	public int getSet(){
		return set;
	}
	
	/**
	 * Set the number of the dataset used for this series
	 * @param set
	 */
	public void setSet(int set){
		this.set=set;
	}
	
	/**
	 * Return the type of the series
	 * @return 0=obs, 1=calc, 2=obs-calc, 3=line at 0, 4=P(Q), 5=S(Q), 6=beta(Q)
	 */
	public int getType(){
		return type;
	}

	/**
	 * Set the type of the series
	 * @param type
	 */
	public void setType(int type){
		this.type=type;
	}
	
	/**
	 * Return a description for the current series
	 * @return descriptive text
	 */
	public String getTypeDesc(){
		return getTypeDesc(type);
	}

	/**
	 * Return a description for a certain plot type
	 * @param type
	 * @return descriptive text
	 */
	public static String getTypeDesc(int type){
		switch(type){
		case 0:
			return "OBS";
		case 1:
			return "CALC";
		case 2:
			return "OBS-CALC";
		case 3:
			return "Constant";
		case 4:
			return "P(Q)";
		case 5:
			return "S(Q) rescaled";
		case 6:
			return "beta(Q) rescaled";
		}
		return "";		
	}

	/**
	 * Return the number of different series types
	 * @return count
	 */
	public static int countTypes(){
		return 7;
	}
	
	/**
	 * Find the minimum and maximim indices for use in plotting the arrays
	 */
	void findLimits() {
		int[] jd=three_.getJd();
		int[] nc1=one_.getNc1();
		int[] nc2=one_.getNc2();
		int[] nc3=one_.getNc3();
		int[] nc4=one_.getNc4();
		int kd=jd[set-1];
		if(kd<1 || kd>maxWorkspace){
			//System.out.println("findLimits: workspace number out of bounds");
			min=0;
			max=0;
		}
		else {
			max=nc4[kd-1];
			min=nc3[kd-1];
			// what's all this about? Assume we have +ve q now
//			if(nc1[kd-1]*nc2[kd-1]<0){
//				min=nc3[kd-1];
//			}
//			else {
//				min=nc1[kd-1];
//			}
		}
	}

	/**
	 * Return the maximum useful array index
	 * @return max
	 */
	public int getMax(){
		return max;
	}
	
	/**
	 * Return the minimum useful array index
	 * @return min
	 */	
	public int getMin(){
		return min;
	}
	
	/**
	 * Return the number of points in the series
	 * @return count = max-min
	 */
	public int getCount(){
		findLimits();
		return max-min;
	}
	
	/**
	 * Return the yshift for this series
	 * @return yshift
	 */
	public float getYshift(){
		return yshift;
	}

	/**
	 * Set the yshift for this series
	 * @param yshift
	 */	
	public void setYshift(int yshift){
		this.yshift=yshift;
	}

	/**
	 * Set the renderer to use the default style for this series
	 * @param renderer an XYErrorSeriesRenderer used to render this series
	 * @param seriesNumber the index of the series within the plot
	 */
	public void setDefaultStyle(XYErrorSeriesRenderer renderer, int seriesNumber){
		this.renderer=renderer;
		this.seriesNumber=seriesNumber;
		if(type==0){
			lines=false;
			shapes=true;
			yerror=true;
		}
		else {
			lines=true;
			shapes=false;
			yerror=false;
		}
		setStyle();
	}

	/**
	 * Set the series number for use with the renderer, then set render to use the correct
	 * styles for this series
	 * @param seriesNumber
	 */	
	public void setStyle(int seriesNumber){
		this.seriesNumber=seriesNumber;
		setStyle();
	}
	
	/**
	 * Set the style for this series
	 */
	public void setStyle(){
			renderer.setSeriesShape(seriesNumber, new Ellipse2D.Double(-2.0, -2.0, 4.0, 4.0));
			renderer.setSeriesLinesVisible(seriesNumber, lines);
			renderer.setSeriesShapesVisible(seriesNumber, shapes);
			renderer.setSeriesDrawYError(seriesNumber, yerror);
		if(key.length()==0){
			renderer.setSeriesVisibleInLegend(seriesNumber,false);
		}
		else{
			renderer.setSeriesVisibleInLegend(seriesNumber,true);			
		}
	}	
	
	/**
	 * Return a particular x value from this series
	 * @param item the item from the series to return
	 * @param log whether or not to return the log of the value
	 * @return the x value
	 */
	Number getX(int item, boolean log){
		int[] jd=three_.getJd();
		int kd=jd[set-1];
		float qq=Core.q_get(q,kd-1,min+item);

		// find log values if required
		if(log){
			qq=(float)Math.log10(qq);
		}

		//System.out.println("Item requested: "+item+" set "+set+" array "+kd+" x_value "+qq);
		return qq;
	}

	/**
	 * Return a either a y value from this series or else the - or + error
	 * @param item the item from the series to return
	 * @param bar 0 for the data point, -1 or 1 for -/+ error
	 * @param log whether or not to return the log of the value
	 * @return the requested value
	 */
	Number getYPoint(int item,int bar,boolean log) {
		int[] jd=null;
		int[] jc=null;		

//		if(type==0){
			jd=three_.getJd();	
//		}
//		else if(type==1){
			jc=three_.getJc();	
//		}
		if(type>=0 && type<=6)
		{
			float cc;
			int kd=jd[set-1];
			int kc=jc[set-1];
			float obs=Core.c_get(c,kd-1,min+item);
			float calc=Core.c_get(c,kc-1,min+item);
			switch(type){
			case 0:
				cc=obs+bar*Core.e_get(e,kd-1,min+item);
				break;
			case 1:
				cc=calc;
				break;
			case 2:
				if(log){
					cc=(float)Math.log10(obs)-(float)Math.log10(calc);
				}
				else{
					cc=obs-calc;
				}
				break;
			case 3:
				// if we're using log axis, set value equal to that in a different serires to avoid log 0
				// (this series will be hidden anyway, but it affects axis limits etc.)
				//if(fr.getYAxisType()==1 && series>0){
				//	cc=getY(series-1,item).floatValue();
				//}
				//else{
				//	cc=0;
				//}

				cc=0;
			
				break;
			case 4:
				int[] jpq=three_.getJpq();
				cc=Core.c_get(c, jpq[set-1]-1, min+item);
				break;
			case 5:
				float scale;
				int[] jsq=three_.getJsq();
				cc=Core.c_get(c, jsq[set-1]-1, min+item);
				// find scale factor for S(Q)
				scale=Core.sqscl(set);
				cc*=scale;
				break;
			case 6:
				int[] jbt=three_.getJbt();
				cc=Core.c_get(c, jbt[set-1]-1, min+item);
				// find scale factor, use same as for S(Q)
				scale=Core.sqscl(set);
				cc*=scale;
				break;
			default:
				cc=0;
				break;		
			}

			// find log values if required
			if(log && (type<=1 || type>=4)){
				//if(type==2){
				//	cc=(float)Math.log10(Math.abs(cc));
				//}
				//else{
					cc=(float)Math.log10(cc);
				//}
			}
			
			// add the yShift
			cc+=yshift;	
			
			//System.out.println("Item requested: "+item+" set "+set+" array "+kd+" y_value "+cc);
			return cc;
		}
		return null;
	}
}
