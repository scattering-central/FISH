package fish;

import java.io.File;
import java.util.Vector;

import javax.swing.filechooser.FileFilter;

public class FishFileFilter extends FileFilter {

	private Vector<String> extensions=new Vector<String>();
	private String description;
		
	/** Creates an empty filter
	 * 
	 * @param text Description of this filter
	 */
	FishFileFilter(String text) {
		super();
		description=text;
	}

/**
 * Creates a filter for files of a particular type
 * @param text Description of this filter
 * @param extension Required extension
 */
	FishFileFilter(String text, String extension) {
		this(text);
		addExtension(extension);
	}

	/**
	 * Creates a filter for a single extension and also adds the extension to another filter
	 * @param text Description of this filter
	 * @param extension Required extension
	 * @param filter Filter to which to add the extension
	 */
	FishFileFilter(String text, String extension,FishFileFilter filter) {
		this(text,extension);
		filter.addExtension(extension);
	}	

	/**
	 * Adds an extension to this filter
	 * @param extension Extension to add
	 */
	public void addExtension(String extension){
		extensions.add(extension);
	}

	/**
	 * Adds an extension to this filter and also adds the extension to another filter
	 * @param extension Extension to add
	 */
	public void addExtension(String extension,FishFileFilter filter){
		extensions.add(extension);
		filter.addExtension(extension);
	}	
	
	@Override
	public boolean accept(File f) {
		if (f.isDirectory()) {
			return true;
		}
		String extension = getExtension(f);
		int count=extensions.size();
		if (extension != null && count>0) {
			for(int i=0;i<count;i++){
				if(extension.equals(extensions.elementAt(i))){
					return true;
				}
			}
		}
		return false;
	}

	@Override
	public String getDescription() {
		return description;
	}

	private static String getExtension(File f) {
        String ext = null;
        String s = f.getName();
        int i = s.lastIndexOf('.');

        if (i > 0 &&  i < s.length() - 1) {
            ext = s.substring(i+1).toLowerCase();
        }
        return ext;
    }
	
}
