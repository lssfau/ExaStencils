package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public class MultiPwAff {
    public static class Ptr extends PointerType { public Ptr() { super(); } }
    protected Context.Ptr ctx;
    protected PointerType ptr;
    MultiPwAff() {}
    MultiPwAff(Context.Ptr ctx, PointerType ptr) {
        assert !ptr.getPointer().equals(Pointer.NULL);
        this.ctx = ctx;
        this.ptr = ptr;
    }
    MultiPwAff.Ptr getPtr() { return (MultiPwAff.Ptr)this.ptr; }
    MultiPwAff.Ptr makePtr0() { 
        MultiPwAff.Ptr p = (MultiPwAff.Ptr)this.ptr;
        this.ptr = new MultiPwAff.Ptr();
        return p;
    }
    protected void finalize() {
        synchronized(this.ctx) {
            Impl.isl.isl_multi_pw_aff_free(getPtr());
        }
    }
    public String toString() {
        Printer p = Printer.toStr();
        p = p.printMultiPwAff(this);
        return p.getStr();
    }
    MultiPwAff asMultiPwAff() {
        Class clazz = this.getClass();
        if (clazz.equals(MultiPwAff.class))
            return this;
        try {
            Constructor<MultiPwAff> c = MultiPwAff.class.getConstructor(clazz);
            return c.newInstance(this);
        } catch (Exception e) {
           throw new IllegalStateException("Cannot construct MultiPwAff from " +
               clazz.getName() + " ?", e);
        }
    }

}
