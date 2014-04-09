package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public class Vec {
    public static class Ptr extends PointerType { public Ptr() { super(); } }
    protected Context.Ptr ctx;
    protected PointerType ptr;
    Vec() {}
    Vec(Context.Ptr ctx, PointerType ptr) {
        assert !ptr.getPointer().equals(Pointer.NULL);
        this.ctx = ctx;
        this.ptr = ptr;
    }
    Vec.Ptr getPtr() { return (Vec.Ptr)this.ptr; }
    Vec.Ptr makePtr0() { 
        Vec.Ptr p = (Vec.Ptr)this.ptr;
        this.ptr = new Vec.Ptr();
        return p;
    }
    public static Vec alloc(int size) {
        Vec that = new Vec();
        that.ctx = Context.getDefaultInstance();
        synchronized(that.ctx) {
            assert size >= 0;
            that.ptr = Impl.isl.isl_vec_alloc(that.ctx, size);
            Context.checkError(that.ctx);
        }
        return that;
    }
    protected void finalize() {
        synchronized(this.ctx) {
            Impl.isl.isl_vec_free(getPtr());
        }
    }
    public String toString() {
        Printer p = Printer.toStr();
        p = p.printVec(this);
        return p.getStr();
    }
    Vec asVec() {
        Class clazz = this.getClass();
        if (clazz.equals(Vec.class))
            return this;
        try {
            Constructor<Vec> c = Vec.class.getConstructor(clazz);
            return c.newInstance(this);
        } catch (Exception e) {
           throw new IllegalStateException("Cannot construct Vec from " +
               clazz.getName() + " ?", e);
        }
    }
    public int size() {
        int res;
        synchronized(this.ctx) {
            Vec self = this.asVec();
            res = Impl.isl.isl_vec_size(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Val getElementVal(int pos) {
        Val.Ptr res;
        synchronized(this.ctx) {
            Vec self = this.asVec();
            res = Impl.isl.isl_vec_get_element_val(self.getPtr(), pos);
            Context.checkError(this.ctx);
        }
        return new Val(this.ctx, res);
    }
    public Vec setElementVal(int pos, Val v) {
        Vec.Ptr res;
        synchronized(this.ctx) {
            Vec self = this.asVec();
            v = v.asVal();
            res = Impl.isl.isl_vec_set_element_val(Impl.isl.isl_vec_copy(self.getPtr()), pos, Impl.isl.isl_val_copy(v.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Vec(this.ctx, res);
    }
    public boolean isEqual(Vec vec2) {
        boolean res;
        synchronized(this.ctx) {
            Vec self = this.asVec();
            vec2 = vec2.asVec();
            res = Impl.isl.isl_vec_is_equal(self.getPtr(), vec2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public int cmpElement(Vec vec2, int pos) {
        int res;
        synchronized(this.ctx) {
            Vec self = this.asVec();
            vec2 = vec2.asVec();
            res = Impl.isl.isl_vec_cmp_element(self.getPtr(), vec2.getPtr(), pos);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Vec setVal(Val v) {
        Vec.Ptr res;
        synchronized(this.ctx) {
            Vec self = this.asVec();
            v = v.asVal();
            res = Impl.isl.isl_vec_set_val(Impl.isl.isl_vec_copy(self.getPtr()), Impl.isl.isl_val_copy(v.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Vec(this.ctx, res);
    }
    public Vec clr() {
        Vec.Ptr res;
        synchronized(this.ctx) {
            Vec self = this.asVec();
            res = Impl.isl.isl_vec_clr(Impl.isl.isl_vec_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Vec(this.ctx, res);
    }
    public Vec neg() {
        Vec.Ptr res;
        synchronized(this.ctx) {
            Vec self = this.asVec();
            res = Impl.isl.isl_vec_neg(Impl.isl.isl_vec_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Vec(this.ctx, res);
    }
    public Vec add(Vec vec2) {
        Vec.Ptr res;
        synchronized(this.ctx) {
            Vec self = this.asVec();
            vec2 = vec2.asVec();
            res = Impl.isl.isl_vec_add(Impl.isl.isl_vec_copy(self.getPtr()), Impl.isl.isl_vec_copy(vec2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Vec(this.ctx, res);
    }
    public Vec extend(int size) {
        Vec.Ptr res;
        synchronized(this.ctx) {
            Vec self = this.asVec();
            assert size >= 0;
            res = Impl.isl.isl_vec_extend(Impl.isl.isl_vec_copy(self.getPtr()), size);
            Context.checkError(this.ctx);
        }
        return new Vec(this.ctx, res);
    }
    public Vec zeroExtend(int size) {
        Vec.Ptr res;
        synchronized(this.ctx) {
            Vec self = this.asVec();
            assert size >= 0;
            res = Impl.isl.isl_vec_zero_extend(Impl.isl.isl_vec_copy(self.getPtr()), size);
            Context.checkError(this.ctx);
        }
        return new Vec(this.ctx, res);
    }
    public Vec concat(Vec vec2) {
        Vec.Ptr res;
        synchronized(this.ctx) {
            Vec self = this.asVec();
            vec2 = vec2.asVec();
            res = Impl.isl.isl_vec_concat(Impl.isl.isl_vec_copy(self.getPtr()), Impl.isl.isl_vec_copy(vec2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Vec(this.ctx, res);
    }
    public Vec sort() {
        Vec.Ptr res;
        synchronized(this.ctx) {
            Vec self = this.asVec();
            res = Impl.isl.isl_vec_sort(Impl.isl.isl_vec_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Vec(this.ctx, res);
    }
    public Vec dropEls(int pos, int n) {
        Vec.Ptr res;
        synchronized(this.ctx) {
            Vec self = this.asVec();
            assert pos >= 0;
            assert n >= 0;
            res = Impl.isl.isl_vec_drop_els(Impl.isl.isl_vec_copy(self.getPtr()), pos, n);
            Context.checkError(this.ctx);
        }
        return new Vec(this.ctx, res);
    }
    public Vec insertEls(int pos, int n) {
        Vec.Ptr res;
        synchronized(this.ctx) {
            Vec self = this.asVec();
            assert pos >= 0;
            assert n >= 0;
            res = Impl.isl.isl_vec_insert_els(Impl.isl.isl_vec_copy(self.getPtr()), pos, n);
            Context.checkError(this.ctx);
        }
        return new Vec(this.ctx, res);
    }
    public Vec insertZeroEls(int pos, int n) {
        Vec.Ptr res;
        synchronized(this.ctx) {
            Vec self = this.asVec();
            assert pos >= 0;
            assert n >= 0;
            res = Impl.isl.isl_vec_insert_zero_els(Impl.isl.isl_vec_copy(self.getPtr()), pos, n);
            Context.checkError(this.ctx);
        }
        return new Vec(this.ctx, res);
    }
    public Vec moveEls(int dst_col, int src_col, int n) {
        Vec.Ptr res;
        synchronized(this.ctx) {
            Vec self = this.asVec();
            assert dst_col >= 0;
            assert src_col >= 0;
            assert n >= 0;
            res = Impl.isl.isl_vec_move_els(Impl.isl.isl_vec_copy(self.getPtr()), dst_col, src_col, n);
            Context.checkError(this.ctx);
        }
        return new Vec(this.ctx, res);
    }

}
