/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package fish;

public class Common_binpar {
  private transient long swigCPtr;
  protected transient boolean swigCMemOwn;

  protected Common_binpar(long cPtr, boolean cMemoryOwn) {
    swigCMemOwn = cMemoryOwn;
    swigCPtr = cPtr;
  }

  protected static long getCPtr(Common_binpar obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  protected void finalize() {
    delete();
  }

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CoreJNI.delete_Common_binpar(swigCPtr);
      }
      swigCPtr = 0;
    }
  }

  public void setNbin(int[] value) {
    CoreJNI.Common_binpar_nbin_set(swigCPtr, this, value);
  }

  public int[] getNbin() {
    return CoreJNI.Common_binpar_nbin_get(swigCPtr, this);
  }

  public void setRbin(float[] value) {
    CoreJNI.Common_binpar_rbin_set(swigCPtr, this, value);
  }

  public float[] getRbin() {
    return CoreJNI.Common_binpar_rbin_get(swigCPtr, this);
  }

  public Common_binpar() {
    this(CoreJNI.new_Common_binpar(), true);
  }

}