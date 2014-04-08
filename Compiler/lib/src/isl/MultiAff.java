package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public class MultiAff {
    public static class Ptr extends PointerType { public Ptr() { super(); } }
    protected Context.Ptr ctx;
    protected PointerType ptr;
    MultiAff() {}
    MultiAff(Context.Ptr ctx, PointerType ptr) {
        assert !ptr.getPointer().equals(Pointer.NULL);
        this.ctx = ctx;
        this.ptr = ptr;
    }
    MultiAff.Ptr getPtr() { return (MultiAff.Ptr)this.ptr; }
    MultiAff.Ptr makePtr0() { 
        MultiAff.Ptr p = (MultiAff.Ptr)this.ptr;
        this.ptr = new MultiAff.Ptr();
        return p;
    }
    // isl_multi_aff_from_aff
    public MultiAff(Aff aff) {
        this.ctx = aff.ctx;
        synchronized(this.ctx) {
            aff = aff.asAff();
            this.ptr = Impl.isl.isl_multi_aff_from_aff(Impl.isl.isl_aff_copy(aff.getPtr()));
            Context.checkError(this.ctx);
        }
    }
    protected void finalize() {
        synchronized(this.ctx) {
            Impl.isl.isl_multi_aff_free(getPtr());
        }
    }
    public String toString() {
        Printer p = Printer.toStr();
        p = p.printMultiAff(this);
        return p.getStr();
    }
    MultiAff asMultiAff() {
        Class clazz = this.getClass();
        if (clazz.equals(MultiAff.class))
            return this;
        try {
            Constructor<MultiAff> c = MultiAff.class.getConstructor(clazz);
            return c.newInstance(this);
        } catch (Exception e) {
           throw new IllegalStateException("Cannot construct MultiAff from " +
               clazz.getName() + " ?", e);
        }
    }
    public MultiAff add(MultiAff maff2) {
        MultiAff.Ptr res;
        synchronized(this.ctx) {
            MultiAff self = this.asMultiAff();
            maff2 = maff2.asMultiAff();
            res = Impl.isl.isl_multi_aff_add(Impl.isl.isl_multi_aff_copy(self.getPtr()), Impl.isl.isl_multi_aff_copy(maff2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new MultiAff(this.ctx, res);
    }
    public MultiAff sub(MultiAff ma2) {
        MultiAff.Ptr res;
        synchronized(this.ctx) {
            MultiAff self = this.asMultiAff();
            ma2 = ma2.asMultiAff();
            res = Impl.isl.isl_multi_aff_sub(Impl.isl.isl_multi_aff_copy(self.getPtr()), Impl.isl.isl_multi_aff_copy(ma2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new MultiAff(this.ctx, res);
    }
    public MultiAff product(MultiAff ma2) {
        MultiAff.Ptr res;
        synchronized(this.ctx) {
            MultiAff self = this.asMultiAff();
            ma2 = ma2.asMultiAff();
            res = Impl.isl.isl_multi_aff_product(Impl.isl.isl_multi_aff_copy(self.getPtr()), Impl.isl.isl_multi_aff_copy(ma2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new MultiAff(this.ctx, res);
    }
    public MultiAff gistParams(Set context) {
        MultiAff.Ptr res;
        synchronized(this.ctx) {
            MultiAff self = this.asMultiAff();
            context = context.asSet();
            res = Impl.isl.isl_multi_aff_gist_params(Impl.isl.isl_multi_aff_copy(self.getPtr()), Impl.isl.isl_set_copy(context.getPtr()));
            Context.checkError(this.ctx);
        }
        return new MultiAff(this.ctx, res);
    }
    public MultiAff gist(Set context) {
        MultiAff.Ptr res;
        synchronized(this.ctx) {
            MultiAff self = this.asMultiAff();
            context = context.asSet();
            res = Impl.isl.isl_multi_aff_gist(Impl.isl.isl_multi_aff_copy(self.getPtr()), Impl.isl.isl_set_copy(context.getPtr()));
            Context.checkError(this.ctx);
        }
        return new MultiAff(this.ctx, res);
    }
    public MultiAff lift(LocalSpace[] ls) {
        MultiAff.Ptr res;
        synchronized(this.ctx) {
            MultiAff self = this.asMultiAff();
            assert ls == null || ls.length == 1;
        LocalSpace.Ptr[] _ls = ls != null ? new LocalSpace.Ptr[1] : null;
            res = Impl.isl.isl_multi_aff_lift(Impl.isl.isl_multi_aff_copy(self.getPtr()), _ls);
        if (ls != null)
            ls[0] = new LocalSpace(this.ctx, _ls[0]);
            Context.checkError(this.ctx);
        }
        return new MultiAff(this.ctx, res);
    }
    public MultiAff pullbackMultiAff(MultiAff ma2) {
        MultiAff.Ptr res;
        synchronized(this.ctx) {
            MultiAff self = this.asMultiAff();
            ma2 = ma2.asMultiAff();
            res = Impl.isl.isl_multi_aff_pullback_multi_aff(Impl.isl.isl_multi_aff_copy(self.getPtr()), Impl.isl.isl_multi_aff_copy(ma2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new MultiAff(this.ctx, res);
    }
    public Set lexLeSet(MultiAff ma2) {
        Set.Ptr res;
        synchronized(this.ctx) {
            MultiAff self = this.asMultiAff();
            ma2 = ma2.asMultiAff();
            res = Impl.isl.isl_multi_aff_lex_le_set(Impl.isl.isl_multi_aff_copy(self.getPtr()), Impl.isl.isl_multi_aff_copy(ma2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set lexGeSet(MultiAff ma2) {
        Set.Ptr res;
        synchronized(this.ctx) {
            MultiAff self = this.asMultiAff();
            ma2 = ma2.asMultiAff();
            res = Impl.isl.isl_multi_aff_lex_ge_set(Impl.isl.isl_multi_aff_copy(self.getPtr()), Impl.isl.isl_multi_aff_copy(ma2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }

}
