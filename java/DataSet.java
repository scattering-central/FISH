package fish;

public interface DataSet {

	public static final String defaultQUnit="\u212B\u207B\u00b9";
	public static final String defaultIUnit="cm\u207B\u00b9";
	
	/**
	 * Returns true if the dataset is in use by the fit
	 * @return true or false
	 */
	boolean isUsed();
	
	/**
	 * Returns the "set number" if dataset is in use
	 * @return number (1,2,3...) or -1 if not in use
	 */
	int getSetNumber();
	
	/**
	 * Sets the "set number"
	 * @param n required set number, or -1 if dataset is no longer required
	 */
	void setSetNumber(int number);
	
	/**
	 * Gets the filename associated with the dataset (may not be unique) 
	 * @return filename
	 */
	public String getFilename();
	
	/**
	 * Gets the label associated with the dataset
	 * @return label
	 */
	public String getLabel();
	
	/**
	 * Sets the label associated with the dataset
	 * @param label
	 */
	public void setLabel(String label);

	/**
	 * Get the tooltip associated with the dataset
	 * @return tooltip
	 */
	public String getToolTip();
	
	/**
	 * Load the data into memory ready for use in the fitting routine
	 * @param dataSet the internal set number into which to load the data
	 */
	public void activate(int dataSet);
	
	/**
	 * Returns true if the dataset has error values
	 * @return true or false
	 */
	public boolean hasErrors();

	public String getQUnit();
	
	public String getIUnit();
	
	/**
	 * Returns the first datapoint to be used,
	 * or -1 to use that defined in the datafile or the first point if datafile doesn't define limits
	 * @return minimum
	 */
	public int getMin();
	
	/**
	 * Returns the last datapoint to be used,
	 * or -1 to use that defined in the datafile or the last point if datafile doesn't define limits
	 * @return maximum
	 */
	public int getMax();
	
	/**
	 * Set the range of points to use during the fit. These are stored so that they can be
	 * re-applied whenever the dataset is re-activated
	 * @param min
	 * @param max
	 */
	public void setLimits(int min,int max);
}
