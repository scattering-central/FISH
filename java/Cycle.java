package fish;

public class Cycle {

	private float[] v;
	private float[] esd;
	private float[] dv;
	private float swse;
	private float xdwe;
	private float var;
	private float lambda;
	
	/**
	 * creates a new set of saved parameters from the curret cycle's parameters
	 * 
	 */
	Cycle() {
		Common_two two_=Core.getTwo_();
		Common_three three_=Core.getThree_();
		
		v=two_.getV().clone();
		esd=two_.getEsd().clone();
		dv=two_.getDv().clone();
		
		swse=three_.getSse();
		xdwe=three_.getXdwe();
		var=three_.getVar();
		lambda=two_.getCon()[0];
		
	}
	
	/**
	 * Reverts to the values from this stored cycle
	 *
	 */
	public void revert() {
		Common_two two_=Core.getTwo_();
		Common_three three_=Core.getThree_();
		
		two_.setV(v);
		two_.setEsd(esd);
		two_.setDv(dv);
		
		three_.setSse(swse);
		three_.setXdwe(xdwe);
		three_.setVar(var);
		float[] con=two_.getCon();
		con[0]=lambda;
		two_.setCon(con);
	}
}
