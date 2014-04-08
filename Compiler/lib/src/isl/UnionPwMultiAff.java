package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public class UnionPwMultiAff {
    public static class Ptr extends PointerType { public Ptr() { super(); } }
    protected Context.Ptr ctx;
    protected PointerType ptr;
    UnionPwMultiAff() {}
    UnionPwMultiAff(Context.Ptr ctx, PointerType ptr) {
        assert !ptr.getPointer().equals(Pointer.NULL);
        this.ctx = ctx;
        this.ptr = ptr;
    }
    UnionPwMultiAff.Ptr getPtr() { return (UnionPwMultiAff.Ptr)this.ptr; }
    UnionPwMultiAff.Ptr makePtr0() { 
        UnionPwMultiAff.Ptr p = (UnionPwMultiAff.Ptr)this.ptr;
        this.ptr = new UnionPwMultiAff.Ptr();
        return p;
    }
    protected void finalize() {
        synchronized(this.ctx) {
            Impl.isl.isl_union_pw_multi_aff_free(getPtr());
        }
    }
    public String toString() {
        Printer p = Printer.toStr();
        p = p.printUnionPwMultiAff(this);
        return p.getStr();
    }
    UnionPwMultiAff asUnionPwMultiAff() {
        Class clazz = this.getClass();
        if (clazz.equals(UnionPwMultiAff.class))
            return this;
        try {
            Constructor<UnionPwMultiAff> c = UnionPwMultiAff.class.getConstructor(clazz);
            return c.newInstance(this);
        } catch (Exception e) {
           throw new IllegalStateException("Cannot construct UnionPwMultiAff from " +
               clazz.getName() + " ?", e);
        }
    }

}
