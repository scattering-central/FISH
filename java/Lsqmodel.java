package fish;

import java.io.IOException;
import java.util.InputMismatchException;
import java.util.NoSuchElementException;

public class Lsqmodel implements Model {
	private String f;
	private String t;
	private String tooltip;
	private int idx;
	private int p;
	
	Lsqmodel(String filename, int index, int pointer, String title,String tooltip){
		f=filename;
		idx=index;
		p=pointer;
		t=title;
		this.tooltip=tooltip;
	}

	public String toString(){
		return t;
	}
	
	public void activate(){
		// open the required model file
		Common_ch ch_=Core.getCh_();
		int il=ch_.getIl();
		Core.closefile(il);
		System.out.println("Model file: "+f);
		Core.openshare(il,f);
		
		// select the required model from within the file
		Core.acmod(idx);
		Common_two two_=Core.getTwo_();
		int np=two_.getNp();
		System.out.println("Model activated with "+np+" parameters\n");
	}

	public String getParameterLabel(int n){
		String s = Core.getLpar(n).substring(0,12);
		return s;
	}

	/**
	 * Returns the name of the file that contains this model
	 * @return filename
	 */
	public String getFilename(){
		return f;
	}
	
	// to refresh a model from an lsqfile, we must find and remove all the other models from the same file
	// then reload the entire file
	public void refresh(ModelList modelList)
			throws IOException, InputMismatchException, NoSuchElementException, IllegalStateException {
		removeFile(modelList);
		modelList.addlsq(f);
	}

	public void removeFile(ModelList modelList) {
		int n=modelList.getSize();
		for(int i=0;i<n;i++){
			Model m=modelList.getModel(i);
			if(m.getClass().equals(Lsqmodel.class)){
				if(((Lsqmodel)m).getFilename()==f){
					modelList.remove(i);
					n--;
					i--;
				}
			}
		}
	}
	
	public String getToolTip(){
		return tooltip;
	}
}

