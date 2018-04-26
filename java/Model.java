package fish;

import java.io.IOException;
import java.util.InputMismatchException;
import java.util.NoSuchElementException;

public interface Model {

	/**
	 * Returns the descriptive title of the model as a string
	 * 
	 * @return the title string
	 */
	public String toString();

	/**
	 * "Activate" the model, i.e. select it for use in the fitting routine
	 *
	 */
	public void activate();

	/**
	 * Returns the label for a particular parameter in the model, only required if the
	 * model is the currently activated model
	 * 
	 * @param par the index of the parameter of which the label is required, 0..n
	 * @return the label as a string
	 */
	public String getParameterLabel(int par);

	/**
	 * Reloads the model, e.g. after the source file has been edited.
	 * Also updates any other models in the same file
	 * @param modelList the list containing details of all the models (this is used to find other models from the
	 * same file, etc.)
	 */
	public void refresh(ModelList modelList)
		throws IOException, InputMismatchException, NoSuchElementException, IllegalStateException;

	/**
	 * Removes the model, and in the case of multi-model files, removes all other models from the same file
	 * @param modelList the list from which models in the same file should be removed
	 */
	public void removeFile(ModelList modelList);
	
	/**
	 * Returns the descriptive tooltip for the model for use in the model selection list
	 * @return tooltip
	 */
	public String getToolTip();
}
