package fish;

import java.awt.Color;
import java.awt.Component;

import javax.swing.DefaultListCellRenderer;
import javax.swing.JList;
import javax.swing.UIManager;

/**
 * Renderer for the list of models. This allows the cells to be highlighted when they are clicked
 * independently of the current selection.
 * @author dqr75132
 *
 */
public class ModelListRenderer extends DefaultListCellRenderer {

	private FishFrame frame;
	private static final Color bg = UIManager.getColor("Table.selectionBackground");
	
	ModelListRenderer(FishFrame frame) {
		this.frame=frame;
	}
	
	public Component getListCellRendererComponent(JList list, Object value,
			int index, boolean isSelected, boolean hasFocus) {
	         setText(value.toString());
	         setBackground(isSelected ? bg : Color.white);
	         setForeground(isSelected ? Color.white : Color.black);
	         if(index==frame.modelListHighlight){
	        	 if(!isSelected) {
	        		 setBackground(Color.lightGray);
	        	 }
	        	 setForeground(Color.yellow);
	         }
	         return this;
	}

}
