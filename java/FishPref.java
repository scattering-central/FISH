package fish;

import java.awt.Component;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.util.prefs.Preferences;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.JTextPane;

public class FishPref extends JDialog implements ActionListener {

	private static final long serialVersionUID = 1L;

	private JPanel jContentPane = null;

	private JLabel workLabel = null;

	private JTextField workField = null;

	private JButton workButton = null;

	private JLabel modelLabel = null;

	private JTextField modelField = null;

	private JButton modelButton = null;

	private JButton cancelButton = null;

	private JButton okButton = null;

	private JTextPane introTextPane = null;

	private JFileChooser fileChooser = null;
	
	private boolean welcome;
	private String workDir;  //  @jve:decl-index=0:
	private String modelFile;  //  @jve:decl-index=0:
//	private JFrame owner;

	/**
	 * Creates a preferences dialog to be shown on startup when no saved preferences could be found 
	 */
	public FishPref() {
		this.welcome=true;
		initialize();
	}
	
	/**
	 * Creates a preferences dialog from within a Fish session
	 * @param owner the window that owns the dialog
	 */
	public FishPref(JFrame owner) {
		super(owner);
		this.welcome=false;
		initialize();
	}

	/**
	 * This method initializes this
	 * 
	 * @return void
	 */
	private void initialize() {
		this.setSize(359, 337);
		this.setContentPane(getJContentPane());
		fileChooser=getFileChooser();
		setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
		
		if(welcome) {
			workDir=Fish.javaDir+File.separator+"work"+File.separator;
			modelFile=Fish.javaDir+File.separator+"work"+File.separator+"lsinp.dat";
			this.setTitle("Welcome to Fish!");
			introTextPane.setText("Please choose defaults for the file locations listed below.\n" +
								  "Your choices will be saved for future Fish sessions, so you\n" +
								  "will not see this message again when starting Fish. You can\n" +
								  "change your preferences at any time via the menu.");
			//setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);
			setAlwaysOnTop(true);
			//cancelButton.setEnabled(false);
		}
		else {
			readPrefs();
			this.setTitle("Fish Preferences");
			introTextPane.setText("Some options will only take effect next time you start Fish.");
			//setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			setAlwaysOnTop(false);
			//cancelButton.setEnabled(true);
		}
		workField.setText(workDir);
		modelField.setText(modelFile);
	}

	/**
	 * Read the preferences saved in the permanent backing store (i.e. Windows registry,
	 * Unix .java directory)
	 *
	 */
	private void readPrefs() {
		Preferences prefs = Preferences.userNodeForPackage(this.getClass());
		workDir = prefs.get("WorkDirectory", "");
		modelFile = prefs.get("DefaultModelFile","");
	}
	
	/**
	 * This method initializes jContentPane
	 * 
	 * @return javax.swing.JPanel
	 */
	private JPanel getJContentPane() {
		if (jContentPane == null) {
			GridBagConstraints gridBagConstraints8 = new GridBagConstraints();
			gridBagConstraints8.fill = GridBagConstraints.HORIZONTAL;
			gridBagConstraints8.gridy = 0;
			gridBagConstraints8.weightx = 1.0;
			gridBagConstraints8.weighty = 0.0;
			gridBagConstraints8.gridwidth = 2;
			gridBagConstraints8.insets = new Insets(8, 8, 8, 8);
			gridBagConstraints8.gridx = 0;
			GridBagConstraints gridBagConstraints6 = new GridBagConstraints();
			gridBagConstraints6.gridx = 1;
			gridBagConstraints6.fill = GridBagConstraints.HORIZONTAL;
			gridBagConstraints6.insets = new Insets(24, 0, 8, 8);
			gridBagConstraints6.weighty = 0.0;
			gridBagConstraints6.anchor = GridBagConstraints.SOUTHEAST;
			gridBagConstraints6.gridy = 5;
			GridBagConstraints gridBagConstraints5 = new GridBagConstraints();
			gridBagConstraints5.gridx = 0;
			gridBagConstraints5.anchor = GridBagConstraints.SOUTHEAST;
			gridBagConstraints5.insets = new Insets(24, 0, 8, 8);
			gridBagConstraints5.weighty = 0.0;
			gridBagConstraints5.gridy = 5;
			GridBagConstraints gridBagConstraints4 = new GridBagConstraints();
			gridBagConstraints4.gridx = 1;
			gridBagConstraints4.insets = new Insets(4, 0, 8, 8);
			gridBagConstraints4.gridy = 4;
			GridBagConstraints gridBagConstraints3 = new GridBagConstraints();
			gridBagConstraints3.fill = GridBagConstraints.HORIZONTAL;
			gridBagConstraints3.gridy = 4;
			gridBagConstraints3.weightx = 1.0;
			gridBagConstraints3.insets = new Insets(4, 8, 8, 8);
			gridBagConstraints3.ipady = 4;
			gridBagConstraints3.gridx = 0;
			GridBagConstraints gridBagConstraints2 = new GridBagConstraints();
			gridBagConstraints2.gridx = 0;
			gridBagConstraints2.gridwidth = 2;
			gridBagConstraints2.anchor = GridBagConstraints.NORTHWEST;
			gridBagConstraints2.insets = new Insets(20, 8, 8, 8);
			gridBagConstraints2.gridy = 3;
			modelLabel = new JLabel();
			modelLabel.setText("Initial model file");
			GridBagConstraints gridBagConstraints11 = new GridBagConstraints();
			gridBagConstraints11.gridx = 1;
			gridBagConstraints11.insets = new Insets(4, 0, 8, 8);
			gridBagConstraints11.gridy = 2;
			GridBagConstraints gridBagConstraints1 = new GridBagConstraints();
			gridBagConstraints1.fill = GridBagConstraints.HORIZONTAL;
			gridBagConstraints1.gridy = 2;
			gridBagConstraints1.weightx = 1.0;
			gridBagConstraints1.anchor = GridBagConstraints.WEST;
			gridBagConstraints1.insets = new Insets(4, 8, 8, 8);
			gridBagConstraints1.weighty = 0.0;
			gridBagConstraints1.ipady = 4;
			gridBagConstraints1.gridx = 0;
			GridBagConstraints gridBagConstraints = new GridBagConstraints();
			gridBagConstraints.insets = new Insets(8, 8, 8, 8);
			gridBagConstraints.gridy = 1;
			gridBagConstraints.ipadx = 0;
			gridBagConstraints.anchor = GridBagConstraints.NORTHWEST;
			gridBagConstraints.fill = GridBagConstraints.NONE;
			gridBagConstraints.gridwidth = 2;
			gridBagConstraints.gridheight = 1;
			gridBagConstraints.weighty = 0.0;
			gridBagConstraints.gridx = 0;
			workLabel = new JLabel();
			workLabel.setText("Working directory for output, log files, etc.");
			jContentPane = new JPanel();
			jContentPane.setLayout(new GridBagLayout());
			jContentPane.add(workLabel, gridBagConstraints);
			jContentPane.add(getWorkField(), gridBagConstraints1);
			jContentPane.add(getWorkButton(), gridBagConstraints11);
			jContentPane.add(modelLabel, gridBagConstraints2);
			jContentPane.add(getModelField(), gridBagConstraints3);
			jContentPane.add(getModelButton(), gridBagConstraints4);
			jContentPane.add(getCancelButton(), gridBagConstraints5);
			jContentPane.add(getOkButton(), gridBagConstraints6);
			jContentPane.add(getIntroTextPane(), gridBagConstraints8);
		}
		return jContentPane;
	}

	/**
	 * This method initializes workField	
	 * 	
	 * @return javax.swing.JTextField	
	 */
	private JTextField getWorkField() {
		if (workField == null) {
			workField = new JTextField();
		}
		return workField;
	}

	/**
	 * This method initializes workButton	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getWorkButton() {
		if (workButton == null) {
			workButton = new JButton();
			workButton.setText("Browse");
			workButton.setName("work");
			workButton.addActionListener(this);
		}
		return workButton;
	}

	/**
	 * This method initializes modelField	
	 * 	
	 * @return javax.swing.JTextField	
	 */
	private JTextField getModelField() {
		if (modelField == null) {
			modelField = new JTextField();
		}
		return modelField;
	}

	/**
	 * This method initializes modelButton	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getModelButton() {
		if (modelButton == null) {
			modelButton = new JButton();
			modelButton.setText("Browse");
			modelButton.setName("model");
			modelButton.addActionListener(this);
		}
		return modelButton;
	}

	/**
	 * This method initializes cancelButton	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getCancelButton() {
		if (cancelButton == null) {
			cancelButton = new JButton();
			cancelButton.setText("Cancel");
			cancelButton.setName("cancel");
			cancelButton.addActionListener(this);
		}
		return cancelButton;
	}

	/**
	 * This method initializes okButton	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getOkButton() {
		if (okButton == null) {
			okButton = new JButton();
			okButton.setText("OK");
			okButton.setName("ok");
			okButton.addActionListener(this);
		}
		return okButton;
	}

	/**
	 * This method initializes introTextPane	
	 * 	
	 * @return javax.swing.JTextPane	
	 */
	private JTextPane getIntroTextPane() {
		if (introTextPane == null) {
			introTextPane = new JTextPane();
			introTextPane.setOpaque(false);
			introTextPane.setEditable(false);
		}
		return introTextPane;
	}

	private JFileChooser getFileChooser() {
		JFileChooser fc = new JFileChooser();
		return fc;
	}
	
	public void actionPerformed(ActionEvent e) {
		String name=((Component)e.getSource()).getName();
		if(name.equals("ok")){
			String dir=workField.getText();
			String filename=modelField.getText();
			File f = new File(dir);
			if(f.isDirectory())
			{
				f = new File(filename);
				try {
					if(f.exists() && f.canRead()){
						ModelList tmpList = new ModelList();
						tmpList.addlsq(filename);

						Preferences prefs = Preferences.userNodeForPackage(this.getClass());
						prefs.put("WorkDirectory", dir);
						prefs.put("DefaultModelFile",filename);
						if(welcome){
							Fish.toOpen++;
						}
						else {
							Common_pref pref_ = Core.getPref_();
							pref_.setFishplot(dir);
							pref_.setLfplt(pref_.getFishplot().length());	
						}
						dispose();
					}
					else {
						throw (new IOException());
					}
				}
				catch (IOException ioex) {
					message(filename+" does not exist or is not readable");
				}
				catch (Exception ex) {
					message(filename+" is not a valid model file");
				}
			}
			else {
				message(dir+" is not a directory");
			}
		}
		else if(name.equals("work")){
			fileChooser.setDialogTitle("Choose working directory");
			fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
			fileChooser.setSelectedFile(new File(workDir));
			int fcRtn=fileChooser.showDialog(this, "Select");
			if (fcRtn == JFileChooser.APPROVE_OPTION) {
	            File file = fileChooser.getSelectedFile();
	            workField.setText(file.getAbsolutePath());
			}
		}
		else if(name.equals("model")){
			fileChooser.setDialogTitle("Choose model file");
			fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
			fileChooser.setSelectedFile(new File(modelFile));
			int fcRtn=fileChooser.showDialog(this, "Select");
			if (fcRtn == JFileChooser.APPROVE_OPTION) {
	            File file = fileChooser.getSelectedFile();
	            modelField.setText(file.getAbsolutePath());
			}
		}
		else if(name.equals("cancel")){
			dispose();
		}
			
	}

	/**
	 * Displays an error message dialog
	 * @param message error message
	 */
	private void message(String message) {
		System.err.println(message);
		JOptionPane optionPane = new JOptionPane(message,JOptionPane.ERROR_MESSAGE);
		JDialog box=optionPane.createDialog(this, "Error");
		box.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
		box.setVisible(true);
	}
	
}  //  @jve:decl-index=0:visual-constraint="10,10"
