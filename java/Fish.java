package fish;

import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.File;
import java.util.prefs.Preferences;

import javax.swing.SwingUtilities;
import javax.swing.UIManager;

public class Fish implements WindowListener {

	static final String version="5.01 21/03/2017";
	
	static final String javaDir = System.getProperty("user.dir");
	static int toOpen=0;

	Fish() {
		Preferences prefs = Preferences.userNodeForPackage(this.getClass());
		
		// uncomment these lines and the related import line above to clear preferences
		/*
		try {
			prefs.clear();
		}
		catch (BackingStoreException ex) {
			System.err.println("Backing store exception");
			System.exit(1);
		}
		*/
		
		String workDir = prefs.get("WorkDirectory", "");
		String modelFile = prefs.get("DefaultModelFile","");

		if(workDir=="" || modelFile==""){
			FishPref fishPref = new FishPref();
			fishPref.pack();
			fishPref.setLocationRelativeTo(null);
			fishPref.addWindowListener(this);
			fishPref.setVisible(true);
		}
		else {
			initialize();
		}
	}

	private void initialize() {
		Common_pref pref_ = Core.getPref_();
		Preferences prefs = Preferences.userNodeForPackage(this.getClass());
		String workDir = prefs.get("WorkDirectory", "");
		String modelFile = prefs.get("DefaultModelFile","");
		pref_.setFishlsin(modelFile);
		pref_.setLflsp(pref_.getFishlsin().length());
		pref_.setFishplot(workDir);
		pref_.setLfplt(pref_.getFishplot().length());		
		pref_.setFishsrce(javaDir+File.separator+"lib"+File.separator);
		pref_.setLfsrc(pref_.getFishsrce().length());
		
		// move an existing logfile to fishlog.old.txt (assuming it is > 0 bytes)
		try {
			File logFile = new File(workDir+File.separator+"fishlog.txt");
			String oldFile = workDir+File.separator+"fishlog.old.txt";
			if(logFile.exists() && logFile.length() > 0){
				if(new File(oldFile).exists()){
					new File(oldFile).delete();
				}
				logFile.renameTo(new File(oldFile));
			}
		}
		catch(Exception e) {
			System.err.println("Could not move old log file");
		}
		
		Core.init();
		
		Common_one one_ = Core.getOne_();
		int maxRows = CoreConstants.MN;
		int maxSets = CoreConstants.MW;
		fish.SWIGTYPE_p_a_1024__float q=one_.getQ();
		fish.SWIGTYPE_p_a_1024__float c=one_.getC();
		fish.SWIGTYPE_p_a_1024__float e=one_.getE();
		for(int i=0;i<maxSets;i++){
			for(int j=0;j<maxRows;j++){
				Core.q_set(q, i, j, 0);
				Core.c_set(c, i, j, 0);
				Core.e_set(e, i, j, 0);
			}
		}
		
		FishFrame fishFrame = new FishFrame();
		fishFrame.setVisible(true);
	}

	public void windowActivated(WindowEvent e) {}

	public void windowClosed(WindowEvent e) {
		if(toOpen>0){
			toOpen--;
			initialize();
		}
	}

	public void windowClosing(WindowEvent e) {}

	public void windowDeactivated(WindowEvent e) {}

	public void windowDeiconified(WindowEvent e) {}

	public void windowIconified(WindowEvent e) {}

	public void windowOpened(WindowEvent e) {}

	public static void main(String[] args) {
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				try {
					UIManager.setLookAndFeel(UIManager.
							getSystemLookAndFeelClassName());
				} catch (Exception e) {
					e.printStackTrace();
				}

				new Fish();

			}
		});
	}

	static {
		System.loadLibrary("fish");
	}

}
