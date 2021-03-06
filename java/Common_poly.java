/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package fish;

public class Common_poly {
  private transient long swigCPtr;
  protected transient boolean swigCMemOwn;

  protected Common_poly(long cPtr, boolean cMemoryOwn) {
    swigCMemOwn = cMemoryOwn;
    swigCPtr = cPtr;
  }

  protected static long getCPtr(Common_poly obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  protected void finalize() {
    delete();
  }

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CoreJNI.delete_Common_poly(swigCPtr);
      }
      swigCPtr = 0;
    }
  }

  public void setPr1(float value) {
    CoreJNI.Common_poly_pr1_set(swigCPtr, this, value);
  }

  public float getPr1() {
    return CoreJNI.Common_poly_pr1_get(swigCPtr, this);
  }

  public void setPr2(float value) {
    CoreJNI.Common_poly_pr2_set(swigCPtr, this, value);
  }

  public float getPr2() {
    return CoreJNI.Common_poly_pr2_get(swigCPtr, this);
  }

  public void setNr1(int value) {
    CoreJNI.Common_poly_nr1_set(swigCPtr, this, value);
  }

  public int getNr1() {
    return CoreJNI.Common_poly_nr1_get(swigCPtr, this);
  }

  public void setNr2(int value) {
    CoreJNI.Common_poly_nr2_set(swigCPtr, this, value);
  }

  public int getNr2() {
    return CoreJNI.Common_poly_nr2_get(swigCPtr, this);
  }

  public void setAbc(float[] value) {
    CoreJNI.Common_poly_abc_set(swigCPtr, this, value);
  }

  public float[] getAbc() {
    return CoreJNI.Common_poly_abc_get(swigCPtr, this);
  }

  public void setRb(float value) {
    CoreJNI.Common_poly_rb_set(swigCPtr, this, value);
  }

  public float getRb() {
    return CoreJNI.Common_poly_rb_get(swigCPtr, this);
  }

  public void setPa(float value) {
    CoreJNI.Common_poly_pa_set(swigCPtr, this, value);
  }

  public float getPa() {
    return CoreJNI.Common_poly_pa_get(swigCPtr, this);
  }

  public void setPb(float value) {
    CoreJNI.Common_poly_pb_set(swigCPtr, this, value);
  }

  public float getPb() {
    return CoreJNI.Common_poly_pb_get(swigCPtr, this);
  }

  public void setPcc(float value) {
    CoreJNI.Common_poly_pcc_set(swigCPtr, this, value);
  }

  public float getPcc() {
    return CoreJNI.Common_poly_pcc_get(swigCPtr, this);
  }

  public void setPd(float value) {
    CoreJNI.Common_poly_pd_set(swigCPtr, this, value);
  }

  public float getPd() {
    return CoreJNI.Common_poly_pd_get(swigCPtr, this);
  }

  public void setIpd(int value) {
    CoreJNI.Common_poly_ipd_set(swigCPtr, this, value);
  }

  public int getIpd() {
    return CoreJNI.Common_poly_ipd_get(swigCPtr, this);
  }

  public void setJpd(int value) {
    CoreJNI.Common_poly_jpd_set(swigCPtr, this, value);
  }

  public int getJpd() {
    return CoreJNI.Common_poly_jpd_get(swigCPtr, this);
  }

  public void setNpp(int value) {
    CoreJNI.Common_poly_npp_set(swigCPtr, this, value);
  }

  public int getNpp() {
    return CoreJNI.Common_poly_npp_get(swigCPtr, this);
  }

  public void setNpp1(int value) {
    CoreJNI.Common_poly_npp1_set(swigCPtr, this, value);
  }

  public int getNpp1() {
    return CoreJNI.Common_poly_npp1_get(swigCPtr, this);
  }

  public void setNpp2(int value) {
    CoreJNI.Common_poly_npp2_set(swigCPtr, this, value);
  }

  public int getNpp2() {
    return CoreJNI.Common_poly_npp2_get(swigCPtr, this);
  }

  public void setNpp3(int value) {
    CoreJNI.Common_poly_npp3_set(swigCPtr, this, value);
  }

  public int getNpp3() {
    return CoreJNI.Common_poly_npp3_get(swigCPtr, this);
  }

  public void setNpm(int value) {
    CoreJNI.Common_poly_npm_set(swigCPtr, this, value);
  }

  public int getNpm() {
    return CoreJNI.Common_poly_npm_get(swigCPtr, this);
  }

  public void setNsimp(int value) {
    CoreJNI.Common_poly_nsimp_set(swigCPtr, this, value);
  }

  public int getNsimp() {
    return CoreJNI.Common_poly_nsimp_get(swigCPtr, this);
  }

  public Common_poly() {
    this(CoreJNI.new_Common_poly(), true);
  }

}
