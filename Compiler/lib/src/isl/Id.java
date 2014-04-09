package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public class Id {
    public static class Ptr extends PointerType { public Ptr() { super(); } }
    protected Context.Ptr ctx;
    protected PointerType ptr;
    Id() {}
    Id(Context.Ptr ctx, PointerType ptr) {
        assert !ptr.getPointer().equals(Pointer.NULL);
        this.ctx = ctx;
        this.ptr = ptr;
    }
    Id.Ptr getPtr() { return (Id.Ptr)this.ptr; }
    Id.Ptr makePtr0() { 
        Id.Ptr p = (Id.Ptr)this.ptr;
        this.ptr = new Id.Ptr();
        return p;
    }
    public static Id alloc(String name, Pointer user) {
        Id that = new Id();
        that.ctx = Context.getDefaultInstance();
        synchronized(that.ctx) {
            that.ptr = Impl.isl.isl_id_alloc(that.ctx, name, user);
            Context.checkError(that.ctx);
        }
        return that;
    }
    protected void finalize() {
        synchronized(this.ctx) {
            Impl.isl.isl_id_free(getPtr());
        }
    }
    public String toString() {
        Printer p = Printer.toStr();
        p = p.printId(this);
        return p.getStr();
    }
    Id asId() {
        Class clazz = this.getClass();
        if (clazz.equals(Id.class))
            return this;
        try {
            Constructor<Id> c = Id.class.getConstructor(clazz);
            return c.newInstance(this);
        } catch (Exception e) {
           throw new IllegalStateException("Cannot construct Id from " +
               clazz.getName() + " ?", e);
        }
    }
    public Pointer getUser() {
        Pointer res;
        synchronized(this.ctx) {
            Id self = this.asId();
            res = Impl.isl.isl_id_get_user(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }

}
