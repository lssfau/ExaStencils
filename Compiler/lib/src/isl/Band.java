package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public class Band {
    public static class Ptr extends PointerType { public Ptr() { super(); } }
    protected Context.Ptr ctx;
    protected PointerType ptr;
    Band() {}
    Band(Context.Ptr ctx, PointerType ptr) {
        assert !ptr.getPointer().equals(Pointer.NULL);
        this.ctx = ctx;
        this.ptr = ptr;
    }
    Band.Ptr getPtr() { return (Band.Ptr)this.ptr; }
    Band.Ptr makePtr0() { 
        Band.Ptr p = (Band.Ptr)this.ptr;
        this.ptr = new Band.Ptr();
        return p;
    }
    protected void finalize() {
        synchronized(this.ctx) {
            Impl.isl.isl_band_free(getPtr());
        }
    }
    public String toString() {
        Printer p = Printer.toStr();
        p = p.printBand(this);
        return p.getStr();
    }
    Band asBand() {
        Class clazz = this.getClass();
        if (clazz.equals(Band.class))
            return this;
        try {
            Constructor<Band> c = Band.class.getConstructor(clazz);
            return c.newInstance(this);
        } catch (Exception e) {
           throw new IllegalStateException("Cannot construct Band from " +
               clazz.getName() + " ?", e);
        }
    }
    public UnionMap getPrefixSchedule() {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            Band self = this.asBand();
            res = Impl.isl.isl_band_get_prefix_schedule(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap getPartialSchedule() {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            Band self = this.asBand();
            res = Impl.isl.isl_band_get_partial_schedule(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap getSuffixSchedule() {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            Band self = this.asBand();
            res = Impl.isl.isl_band_get_suffix_schedule(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }

}
