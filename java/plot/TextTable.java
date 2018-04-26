/*
 * Based on org.jfree.chart.title.TextTitle
 */

package fish.plot;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.Paint;
import java.awt.geom.Rectangle2D;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

import org.jfree.chart.block.BlockResult;
import org.jfree.chart.block.EntityBlockParams;
import org.jfree.chart.block.LengthConstraintType;
import org.jfree.chart.block.RectangleConstraint;
import org.jfree.chart.entity.ChartEntity;
import org.jfree.chart.entity.EntityCollection;
import org.jfree.chart.entity.StandardEntityCollection;
import org.jfree.chart.event.TitleChangeEvent;
import org.jfree.chart.title.Title;
import org.jfree.data.Range;
import org.jfree.io.SerialUtilities;
import org.jfree.text.G2TextMeasurer;
import org.jfree.text.TextBlock;
import org.jfree.text.TextBlockAnchor;
import org.jfree.text.TextUtilities;
import org.jfree.ui.HorizontalAlignment;
import org.jfree.ui.RectangleEdge;
import org.jfree.ui.RectangleInsets;
import org.jfree.ui.Size2D;
import org.jfree.ui.VerticalAlignment;
import org.jfree.util.ObjectUtilities;
import org.jfree.util.PaintUtilities;
import org.jfree.util.PublicCloneable;

/**
 * A chart title that displays a text string with automatic wrapping as 
 * required.
 */
public class TextTable extends Title 
                       implements Serializable, Cloneable, PublicCloneable {

    /** For serialization. */
    private static final long serialVersionUID = 8372008692127477443L;
    
    /** The default font. */
    public static final Font DEFAULT_FONT 
        = new Font("SansSerif", Font.BOLD, 12);

    /** The default text color. */
    public static final Paint DEFAULT_TEXT_PAINT = Color.black;

    /** The title text. */
    private String text;

    /** The font used to display the title. */
    private Font font;
    
    /** The text alignment. */
    private HorizontalAlignment textAlignment;

    /** The paint used to display the title text. */
    private transient Paint paint;

    /** The background paint. */
    private transient Paint backgroundPaint;

    /** The tool tip text (can be <code>null</code>). */
    private String toolTipText;
    
    /** The URL text (can be <code>null</code>). */
    private String urlText;
    
    /** The content. */
    private TextBlock content;
    
    /** 
     * A flag that controls whether the title expands to fit the available
     * space..
     */
    private boolean expandToFitSpace = false;
    
    /**
     * Creates a new title, using default attributes where necessary.
     */
    public TextTable() {
        this("");
    }

    /**
     * Creates a new title, using default attributes where necessary.
     *
     * @param text  the title text (<code>null</code> not permitted).
     */
    public TextTable(String text) {
        this(text, TextTable.DEFAULT_FONT, TextTable.DEFAULT_TEXT_PAINT,
                Title.DEFAULT_POSITION, Title.DEFAULT_HORIZONTAL_ALIGNMENT,
                Title.DEFAULT_VERTICAL_ALIGNMENT, Title.DEFAULT_PADDING);
    }

    /**
     * Creates a new title, using default attributes where necessary.
     *
     * @param text  the title text (<code>null</code> not permitted).
     * @param font  the title font (<code>null</code> not permitted).
     */
    public TextTable(String text, Font font) {
        this(text, font, TextTable.DEFAULT_TEXT_PAINT, Title.DEFAULT_POSITION,
                Title.DEFAULT_HORIZONTAL_ALIGNMENT, 
                Title.DEFAULT_VERTICAL_ALIGNMENT, Title.DEFAULT_PADDING);
    }

    /**
     * Creates a new title.
     *
     * @param text  the text for the title (<code>null</code> not permitted).
     * @param font  the title font (<code>null</code> not permitted).
     * @param paint  the title paint (<code>null</code> not permitted).
     * @param position  the title position (<code>null</code> not permitted).
     * @param horizontalAlignment  the horizontal alignment (<code>null</code> 
     *                             not permitted).
     * @param verticalAlignment  the vertical alignment (<code>null</code> not 
     *                           permitted).
     * @param padding  the space to leave around the outside of the title.
     */
    public TextTable(String text, Font font, Paint paint, 
                     RectangleEdge position, 
                     HorizontalAlignment horizontalAlignment, 
                     VerticalAlignment verticalAlignment,
                     RectangleInsets padding) {

        super(position, horizontalAlignment, verticalAlignment, padding);
        
        if (text == null) {
            throw new NullPointerException("Null 'text' argument.");
        }
        if (font == null) {
            throw new NullPointerException("Null 'font' argument.");
        }
        if (paint == null) {
            throw new NullPointerException("Null 'paint' argument.");
        }
        this.text = text;
        this.font = font;
        this.paint = paint;
        // the textAlignment and the horizontalAlignment are separate things,
        // but it makes sense for the default textAlignment to match the
        // title's horizontal alignment...
        this.textAlignment = horizontalAlignment;
        this.backgroundPaint = null;
        this.content = null;
        this.toolTipText = null;
        this.urlText = null;
        
    }

    /**
     * Returns the title text.
     *
     * @return The text (never <code>null</code>).
     */
    public String getText() {
        return this.text;
    }

    /**
     * Sets the title to the specified text and sends a 
     * {@link TitleChangeEvent} to all registered listeners.
     *
     * @param text  the text (<code>null</code> not permitted).
     */
    public void setText(String text) {
        if (text == null) {
            throw new NullPointerException("Null 'text' argument.");
        }
        if (!this.text.equals(text)) {
            this.text = text;
            notifyListeners(new TitleChangeEvent(this));
        }
    }

    /**
     * Returns the text alignment.  This controls how the text is aligned 
     * within the title's bounds, whereas the title's horizontal alignment
     * controls how the title's bounding rectangle is aligned within the 
     * drawing space.
     * 
     * @return The text alignment.
     */
    public HorizontalAlignment getTextAlignment() {
        return this.textAlignment;
    }
    
    /**
     * Sets the text alignment.
     * 
     * @param alignment  the alignment (<code>null</code> not permitted).
     */
    public void setTextAlignment(HorizontalAlignment alignment) {
        if (alignment == null) {
            throw new IllegalArgumentException("Null 'alignment' argument.");
        }
        this.textAlignment = alignment;
        notifyListeners(new TitleChangeEvent(this));
    }
    
    /**
     * Returns the font used to display the title string.
     *
     * @return The font (never <code>null</code>).
     */
    public Font getFont() {
        return this.font;
    }

    /**
     * Sets the font used to display the title string.  Registered listeners 
     * are notified that the title has been modified.
     *
     * @param font  the new font (<code>null</code> not permitted).
     */
    public void setFont(Font font) {
        if (font == null) {
            throw new IllegalArgumentException("Null 'font' argument.");
        }
        if (!this.font.equals(font)) {
            this.font = font;
            notifyListeners(new TitleChangeEvent(this));
        }
    }

    /**
     * Returns the paint used to display the title string.
     *
     * @return The paint (never <code>null</code>).
     */
    public Paint getPaint() {
        return this.paint;
    }

    /**
     * Sets the paint used to display the title string.  Registered listeners 
     * are notified that the title has been modified.
     *
     * @param paint  the new paint (<code>null</code> not permitted).
     */
    public void setPaint(Paint paint) {
        if (paint == null) {
            throw new IllegalArgumentException("Null 'paint' argument.");
        }
        if (!this.paint.equals(paint)) {
            this.paint = paint;
            notifyListeners(new TitleChangeEvent(this));
        }
    }

    /**
     * Returns the background paint.
     *
     * @return The paint (possibly <code>null</code>).
     */
    public Paint getBackgroundPaint() {
        return this.backgroundPaint;
    }

    /**
     * Sets the background paint and sends a {@link TitleChangeEvent} to all 
     * registered listeners.  If you set this attribute to <code>null</code>, 
     * no background is painted (which makes the title background transparent).
     *
     * @param paint  the background paint (<code>null</code> permitted).
     */
    public void setBackgroundPaint(Paint paint) {
        this.backgroundPaint = paint;
        notifyListeners(new TitleChangeEvent(this));
    }
    
    /**
     * Returns the tool tip text.
     *
     * @return The tool tip text (possibly <code>null</code>).
     */
    public String getToolTipText() {
        return this.toolTipText;
    }

    /**
     * Sets the tool tip text to the specified text and sends a 
     * {@link TitleChangeEvent} to all registered listeners.
     *
     * @param text  the text (<code>null</code> permitted).
     */
    public void setToolTipText(String text) {
        this.toolTipText = text;
        notifyListeners(new TitleChangeEvent(this));
    }

    /**
     * Returns the URL text.
     *
     * @return The URL text (possibly <code>null</code>).
     */
    public String getURLText() {
        return this.urlText;
    }

    /**
     * Sets the URL text to the specified text and sends a 
     * {@link TitleChangeEvent} to all registered listeners.
     *
     * @param text  the text (<code>null</code> permitted).
     */
    public void setURLText(String text) {
        this.urlText = text;
        notifyListeners(new TitleChangeEvent(this));
    }
    
    /**
     * Returns the flag that controls whether or not the title expands to fit
     * the available space.
     * 
     * @return The flag.
     */
    public boolean getExpandToFitSpace() {
        return this.expandToFitSpace;   
    }
    
    /**
     * Sets the flag that controls whether the title expands to fit the 
     * available space, and sends a {@link TitleChangeEvent} to all registered
     * listeners.
     * 
     * @param expand  the flag.
     */
    public void setExpandToFitSpace(boolean expand) {
        this.expandToFitSpace = expand;
        notifyListeners(new TitleChangeEvent(this));        
    }

    /**
     * Arranges the contents of the block, within the given constraints, and 
     * returns the block size.
     * 
     * @param g2  the graphics device.
     * @param constraint  the constraint (<code>null</code> not permitted).
     * 
     * @return The block size (in Java2D units, never <code>null</code>).
     */
    public Size2D arrange(Graphics2D g2, RectangleConstraint constraint) {
        RectangleConstraint cc = toContentConstraint(constraint);
        LengthConstraintType w = cc.getWidthConstraintType();
        LengthConstraintType h = cc.getHeightConstraintType();
        Size2D contentSize = null;
        if (w == LengthConstraintType.NONE) {
            if (h == LengthConstraintType.NONE) {
                throw new RuntimeException("Not yet implemented."); 
            }
            else if (h == LengthConstraintType.RANGE) {
                throw new RuntimeException("Not yet implemented."); 
            }
            else if (h == LengthConstraintType.FIXED) {
                throw new RuntimeException("Not yet implemented.");                 
            }            
        }
        else if (w == LengthConstraintType.RANGE) {
            if (h == LengthConstraintType.NONE) {
                throw new RuntimeException("Not yet implemented."); 
            }
            else if (h == LengthConstraintType.RANGE) {
                contentSize = arrangeRR(g2, cc.getWidthRange(), 
                        cc.getHeightRange()); 
            }
            else if (h == LengthConstraintType.FIXED) {
                throw new RuntimeException("Not yet implemented.");                 
            }
        }
        else if (w == LengthConstraintType.FIXED) {
            if (h == LengthConstraintType.NONE) {
                throw new RuntimeException("Not yet implemented."); 
            }
            else if (h == LengthConstraintType.RANGE) {
                throw new RuntimeException("Not yet implemented."); 
            }
            else if (h == LengthConstraintType.FIXED) {
                throw new RuntimeException("Not yet implemented.");                 
            }
        }
        return new Size2D(calculateTotalWidth(contentSize.getWidth()),
                calculateTotalHeight(contentSize.getHeight()));
    }
    
    /**
     * Returns the content size for the title.  This will reflect the fact that
     * a text title positioned on the left or right of a chart will be rotated
     * 90 degrees.
     * 
     * @param g2  the graphics device.
     * @param widthRange  the width range.
     * @param heightRange  the height range.
     * 
     * @return The content size.
     */
    protected Size2D arrangeRR(Graphics2D g2, Range widthRange, 
            Range heightRange) {
        RectangleEdge position = getPosition();
        if (position == RectangleEdge.TOP || position == RectangleEdge.BOTTOM ||
        		position == RectangleEdge.LEFT || position == RectangleEdge.RIGHT) {
            float maxWidth = (float) widthRange.getUpperBound();
            g2.setFont(this.font);
            this.content = TextUtilities.createTextBlock(this.text, this.font, 
                    this.paint, maxWidth, new G2TextMeasurer(g2));
            this.content.setLineAlignment(this.textAlignment);
            Size2D contentSize = this.content.calculateDimensions(g2);
            if (this.expandToFitSpace) {
                return new Size2D(maxWidth, contentSize.getHeight());
            }
            else {
                return contentSize;
            }
        }
        else {
            throw new RuntimeException("Unrecognised exception.");
        }
    }
    
    /**
     * Draws the title on a Java 2D graphics device (such as the screen or a 
     * printer).
     *
     * @param g2  the graphics device.
     * @param area  the area allocated for the title.
     */
    public void draw(Graphics2D g2, Rectangle2D area) {
        draw(g2, area, null);
    }
    
    /**
     * Draws the block within the specified area.
     * 
     * @param g2  the graphics device.
     * @param area  the area.
     * @param params  if this is an instance of {@link EntityBlockParams} it
     *                is used to determine whether or not an 
     *                {@link EntityCollection} is returned by this method.
     * 
     * @return An {@link EntityCollection} containing a chart entity for the
     *         title, or <code>null</code>.
     */
    public Object draw(Graphics2D g2, Rectangle2D area, Object params) {
        if (this.content == null) {
            return null;   
        }
        area = trimMargin(area);
        drawBorder(g2, area);
        if (this.text.equals("")) {
            return null;
        }
        ChartEntity entity = null;
        if (params instanceof EntityBlockParams) {
            EntityBlockParams p = (EntityBlockParams) params;
            if (p.getGenerateEntities()) {
                entity = new ChartEntity(area, this.toolTipText, this.urlText);    
            }
        }
        area = trimBorder(area);
        if (this.backgroundPaint != null) {
            g2.setPaint(this.backgroundPaint);
            g2.fill(area);
        }
        area = trimPadding(area);
        RectangleEdge position = getPosition();
        if (position == RectangleEdge.TOP || position == RectangleEdge.BOTTOM) {
            drawHorizontal(g2, area);
        }
        else if (position == RectangleEdge.LEFT 
                 || position == RectangleEdge.RIGHT) {
            drawVertical(g2, area);
        }
        BlockResult result = new BlockResult();
        if (entity != null) {
            StandardEntityCollection sec = new StandardEntityCollection();
            sec.add(entity);
            result.setEntityCollection(sec);
        }
        return result;
    }

    /**
     * Draws a the title horizontally within the specified area.  This method 
     * will be called from the {@link #draw(Graphics2D, Rectangle2D) draw} 
     * method.
     * 
     * @param g2  the graphics device.
     * @param area  the area for the title.
     */
    protected void drawHorizontal(Graphics2D g2, Rectangle2D area) {
        Rectangle2D titleArea = (Rectangle2D) area.clone();
        g2.setFont(this.font);
        g2.setPaint(this.paint);
        TextBlockAnchor anchor = null;
        float x = 0.0f;
        HorizontalAlignment horizontalAlignment = getHorizontalAlignment();
        if (horizontalAlignment == HorizontalAlignment.LEFT) {
            x = (float) titleArea.getX();
            anchor = TextBlockAnchor.TOP_LEFT;
        }
        else if (horizontalAlignment == HorizontalAlignment.RIGHT) {
            x = (float) titleArea.getMaxX();
            anchor = TextBlockAnchor.TOP_RIGHT;
        }
        else if (horizontalAlignment == HorizontalAlignment.CENTER) {
            x = (float) titleArea.getCenterX();
            anchor = TextBlockAnchor.TOP_CENTER;
        }
        float y = 0.0f;
        RectangleEdge position = getPosition();
        if (position == RectangleEdge.TOP) {
            y = (float) titleArea.getY();
        }
        else if (position == RectangleEdge.BOTTOM) {
            y = (float) titleArea.getMaxY();
            if (horizontalAlignment == HorizontalAlignment.LEFT) {
                anchor = TextBlockAnchor.BOTTOM_LEFT;
            }
            else if (horizontalAlignment == HorizontalAlignment.CENTER) {
                anchor = TextBlockAnchor.BOTTOM_CENTER;
            }
            else if (horizontalAlignment == HorizontalAlignment.RIGHT) {
                anchor = TextBlockAnchor.BOTTOM_RIGHT;
            }
        }
        this.content.draw(g2, x, y, anchor);
    }
    
    /**
     * Draws a the title vertically within the specified area.  This method 
     * will be called from the {@link #draw(Graphics2D, Rectangle2D) draw} 
     * method.
     * 
     * @param g2  the graphics device.
     * @param area  the area for the title.
     */
    protected void drawVertical(Graphics2D g2, Rectangle2D area) {
        Rectangle2D titleArea = (Rectangle2D) area.clone();
        g2.setFont(this.font);
        g2.setPaint(this.paint);
        TextBlockAnchor anchor = null;
        float y = 0.0f;
        VerticalAlignment verticalAlignment = getVerticalAlignment();
        if (verticalAlignment == VerticalAlignment.TOP) {
            y = (float) titleArea.getY();
            anchor = TextBlockAnchor.TOP_LEFT;
        }
        else if (verticalAlignment == VerticalAlignment.BOTTOM) {
            y = (float) titleArea.getMaxY();
            anchor = TextBlockAnchor.BOTTOM_LEFT;
        }
        else if (verticalAlignment == VerticalAlignment.CENTER) {
            y = (float) titleArea.getCenterY();
            anchor = TextBlockAnchor.CENTER_LEFT;
        }
        float x = 0.0f;

        x = (float) titleArea.getX();
            
        this.content.draw(g2, x, y, anchor);
    }

    /**
     * Tests this title for equality with another object.
     *
     * @param obj  the object (<code>null</code> permitted).
     *
     * @return <code>true</code> or <code>false</code>.
     */
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        if (!(obj instanceof TextTable)) {
            return false;
        }
        if (!super.equals(obj)) {
            return false;
        }
        TextTable that = (TextTable) obj;
        if (!ObjectUtilities.equal(this.text, that.text)) {
            return false;
        }
        if (!ObjectUtilities.equal(this.font, that.font)) {
            return false;
        }
        if (!PaintUtilities.equal(this.paint, that.paint)) {
            return false;
        }
        if (this.textAlignment != that.textAlignment) {
            return false;
        }
        if (!PaintUtilities.equal(this.backgroundPaint, that.backgroundPaint)) {
            return false;
        }
        return true;
    }

    /**
     * Returns a hash code.
     * 
     * @return A hash code.
     */
    public int hashCode() {
        int result = super.hashCode();
        result = 29 * result + (this.text != null ? this.text.hashCode() : 0);
        result = 29 * result + (this.font != null ? this.font.hashCode() : 0);
        result = 29 * result + (this.paint != null ? this.paint.hashCode() : 0);
        result = 29 * result + (this.backgroundPaint != null 
                ? this.backgroundPaint.hashCode() : 0);
        return result;
    }

    /**
     * Returns a clone of this object.
     * 
     * @return A clone.
     * 
     * @throws CloneNotSupportedException never.
     */
    public Object clone() throws CloneNotSupportedException {
        return super.clone();
    }
    
    /**
     * Provides serialization support.
     *
     * @param stream  the output stream.
     *
     * @throws IOException  if there is an I/O error.
     */
    private void writeObject(ObjectOutputStream stream) throws IOException {
        stream.defaultWriteObject();
        SerialUtilities.writePaint(this.paint, stream);
        SerialUtilities.writePaint(this.backgroundPaint, stream);
    }

    /**
     * Provides serialization support.
     *
     * @param stream  the input stream.
     *
     * @throws IOException  if there is an I/O error.
     * @throws ClassNotFoundException  if there is a classpath problem.
     */
    private void readObject(ObjectInputStream stream) 
        throws IOException, ClassNotFoundException 
    {
        stream.defaultReadObject();
        this.paint = SerialUtilities.readPaint(stream);
        this.backgroundPaint = SerialUtilities.readPaint(stream);
    }

}
