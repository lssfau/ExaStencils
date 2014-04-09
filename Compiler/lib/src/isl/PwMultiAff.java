package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public class PwMultiAff {
    public static class Ptr extends PointerType { public Ptr() { super(); } }
    protected Context.Ptr ctx;
    protected PointerType ptr;
    PwMultiAff() {}
    PwMultiAff(Context.Ptr ctx, PointerType ptr) {
        assert !ptr.getPointer().equals(Pointer.NULL);
        this.ctx = ctx;
        this.ptr = ptr;
    }
    PwMultiAff.Ptr getPtr() { return (PwMultiAff.Ptr)this.ptr; }
    PwMultiAff.Ptr makePtr0() { 
        PwMultiAff.Ptr p = (PwMultiAff.Ptr)this.ptr;
        this.ptr = new PwMultiAff.Ptr();
        return p;
    }
    // isl_pw_multi_aff_from_multi_aff
    public PwMultiAff(MultiAff ma) {
        this.ctx = ma.ctx;
        synchronized(this.ctx) {
            ma = ma.asMultiAff();
            this.ptr = Impl.isl.isl_pw_multi_aff_from_multi_aff(Impl.isl.isl_multi_aff_copy(ma.getPtr()));
            Context.checkError(this.ctx);
        }
    }
    // isl_pw_multi_aff_from_pw_aff
    public PwMultiAff(PwAff pa) {
        this.ctx = pa.ctx;
        synchronized(this.ctx) {
            pa = pa.asPwAff();
            this.ptr = Impl.isl.isl_pw_multi_aff_from_pw_aff(Impl.isl.isl_pw_aff_copy(pa.getPtr()));
            Context.checkError(this.ctx);
        }
    }
    public static PwMultiAff alloc(Set set, MultiAff maff) {
        PwMultiAff that = new PwMultiAff();
        that.ctx = maff.ctx;
        synchronized(that.ctx) {
            set = set.asSet();
            maff = maff.asMultiAff();
            that.ptr = Impl.isl.isl_pw_multi_aff_alloc(Impl.isl.isl_set_copy(set.getPtr()), Impl.isl.isl_multi_aff_copy(maff.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    protected void finalize() {
        synchronized(this.ctx) {
            Impl.isl.isl_pw_multi_aff_free(getPtr());
        }
    }
    public String toString() {
        Printer p = Printer.toStr();
        p = p.printPwMultiAff(this);
        return p.getStr();
    }
    PwMultiAff asPwMultiAff() {
        Class clazz = this.getClass();
        if (clazz.equals(PwMultiAff.class))
            return this;
        try {
            Constructor<PwMultiAff> c = PwMultiAff.class.getConstructor(clazz);
            return c.newInstance(this);
        } catch (Exception e) {
           throw new IllegalStateException("Cannot construct PwMultiAff from " +
               clazz.getName() + " ?", e);
        }
    }
    public PwMultiAff coalesce() {
        PwMultiAff.Ptr res;
        synchronized(this.ctx) {
            PwMultiAff self = this.asPwMultiAff();
            res = Impl.isl.isl_pw_multi_aff_coalesce(Impl.isl.isl_pw_multi_aff_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwMultiAff(this.ctx, res);
    }
    public PwMultiAff gistParams(Set set) {
        PwMultiAff.Ptr res;
        synchronized(this.ctx) {
            PwMultiAff self = this.asPwMultiAff();
            set = set.asSet();
            res = Impl.isl.isl_pw_multi_aff_gist_params(Impl.isl.isl_pw_multi_aff_copy(self.getPtr()), Impl.isl.isl_set_copy(set.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwMultiAff(this.ctx, res);
    }
    public PwMultiAff gist(Set set) {
        PwMultiAff.Ptr res;
        synchronized(this.ctx) {
            PwMultiAff self = this.asPwMultiAff();
            set = set.asSet();
            res = Impl.isl.isl_pw_multi_aff_gist(Impl.isl.isl_pw_multi_aff_copy(self.getPtr()), Impl.isl.isl_set_copy(set.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwMultiAff(this.ctx, res);
    }
    public PwMultiAff pullbackMultiAff(MultiAff ma) {
        PwMultiAff.Ptr res;
        synchronized(this.ctx) {
            PwMultiAff self = this.asPwMultiAff();
            ma = ma.asMultiAff();
            res = Impl.isl.isl_pw_multi_aff_pullback_multi_aff(Impl.isl.isl_pw_multi_aff_copy(self.getPtr()), Impl.isl.isl_multi_aff_copy(ma.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwMultiAff(this.ctx, res);
    }
    public PwMultiAff pullbackPwMultiAff(PwMultiAff pma2) {
        PwMultiAff.Ptr res;
        synchronized(this.ctx) {
            PwMultiAff self = this.asPwMultiAff();
            pma2 = pma2.asPwMultiAff();
            res = Impl.isl.isl_pw_multi_aff_pullback_pw_multi_aff(Impl.isl.isl_pw_multi_aff_copy(self.getPtr()), Impl.isl.isl_pw_multi_aff_copy(pma2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwMultiAff(this.ctx, res);
    }
    public <ExceptionTy extends Exception> void foreachPiece(final XCallback2<Set,MultiAff,ExceptionTy> fn) throws ExceptionTy {
        int res;
        synchronized(this.ctx) {
            PwMultiAff self = this.asPwMultiAff();
            final Context.Ptr _ctx = this.ctx;
            final Throwable[] exc_info = new Throwable[1];
            Callback cb = new Callback() {
                public int callback(Set.Ptr cb_arg0, MultiAff.Ptr cb_arg1, Pointer _user) {
                    try {
                    fn.apply(new Set(_ctx, cb_arg0), new MultiAff(_ctx, cb_arg1));
                    } catch (Throwable e) {
                        exc_info[0] = e;
                        return -1;
                    }
                    return 0;
                }
            };
            res = Impl.isl.isl_pw_multi_aff_foreach_piece(self.getPtr(), cb, Pointer.NULL);
            if (exc_info[0] != null) {
                if (exc_info[0] instanceof Error)
                    throw (Error)exc_info[0];
                else if (exc_info[0] instanceof RuntimeException)
                    throw (RuntimeException)exc_info[0];
                @SuppressWarnings({"unchecked"})
                ExceptionTy e = (ExceptionTy)exc_info[0];
                throw e;
            }
            Context.checkError(this.ctx);
        }
    }
    public int dim(DimType type) {
        int res;
        synchronized(this.ctx) {
            PwMultiAff self = this.asPwMultiAff();
            res = Impl.isl.isl_pw_multi_aff_dim(self.getPtr(), type.value);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public PwAff getPwAff(int pos) {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            PwMultiAff self = this.asPwMultiAff();
            res = Impl.isl.isl_pw_multi_aff_get_pw_aff(self.getPtr(), pos);
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public PwMultiAff setPwAff(int pos, PwAff pa) {
        PwMultiAff.Ptr res;
        synchronized(this.ctx) {
            PwMultiAff self = this.asPwMultiAff();
            assert pos >= 0;
            pa = pa.asPwAff();
            res = Impl.isl.isl_pw_multi_aff_set_pw_aff(Impl.isl.isl_pw_multi_aff_copy(self.getPtr()), pos, Impl.isl.isl_pw_aff_copy(pa.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwMultiAff(this.ctx, res);
    }
    public Space getDomainSpace() {
        Space.Ptr res;
        synchronized(this.ctx) {
            PwMultiAff self = this.asPwMultiAff();
            res = Impl.isl.isl_pw_multi_aff_get_domain_space(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public Space getSpace() {
        Space.Ptr res;
        synchronized(this.ctx) {
            PwMultiAff self = this.asPwMultiAff();
            res = Impl.isl.isl_pw_multi_aff_get_space(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public boolean hasTupleName(DimType type) {
        boolean res;
        synchronized(this.ctx) {
            PwMultiAff self = this.asPwMultiAff();
            res = Impl.isl.isl_pw_multi_aff_has_tuple_name(self.getPtr(), type.value);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public String getTupleName(DimType type) {
        String res;
        synchronized(this.ctx) {
            PwMultiAff self = this.asPwMultiAff();
            res = Impl.isl.isl_pw_multi_aff_get_tuple_name(self.getPtr(), type.value);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Id getTupleId(DimType type) {
        Id.Ptr res;
        synchronized(this.ctx) {
            PwMultiAff self = this.asPwMultiAff();
            res = Impl.isl.isl_pw_multi_aff_get_tuple_id(self.getPtr(), type.value);
            Context.checkError(this.ctx);
        }
        return new Id(this.ctx, res);
    }
    public boolean hasTupleId(DimType type) {
        boolean res;
        synchronized(this.ctx) {
            PwMultiAff self = this.asPwMultiAff();
            res = Impl.isl.isl_pw_multi_aff_has_tuple_id(self.getPtr(), type.value);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public PwMultiAff setTupleId(DimType type, Id id) {
        PwMultiAff.Ptr res;
        synchronized(this.ctx) {
            PwMultiAff self = this.asPwMultiAff();
            id = id.asId();
            res = Impl.isl.isl_pw_multi_aff_set_tuple_id(Impl.isl.isl_pw_multi_aff_copy(self.getPtr()), type.value, Impl.isl.isl_id_copy(id.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwMultiAff(this.ctx, res);
    }
    public PwMultiAff dropDims(DimType type, int first, int n) {
        PwMultiAff.Ptr res;
        synchronized(this.ctx) {
            PwMultiAff self = this.asPwMultiAff();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_pw_multi_aff_drop_dims(Impl.isl.isl_pw_multi_aff_copy(self.getPtr()), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return new PwMultiAff(this.ctx, res);
    }
    public Set domain() {
        Set.Ptr res;
        synchronized(this.ctx) {
            PwMultiAff self = this.asPwMultiAff();
            res = Impl.isl.isl_pw_multi_aff_domain(Impl.isl.isl_pw_multi_aff_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public String getDimName(DimType type, int pos) {
        String res;
        synchronized(this.ctx) {
            PwMultiAff self = this.asPwMultiAff();
            assert pos >= 0;
            res = Impl.isl.isl_pw_multi_aff_get_dim_name(self.getPtr(), type.value, pos);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Id getDimId(DimType type, int pos) {
        Id.Ptr res;
        synchronized(this.ctx) {
            PwMultiAff self = this.asPwMultiAff();
            assert pos >= 0;
            res = Impl.isl.isl_pw_multi_aff_get_dim_id(self.getPtr(), type.value, pos);
            Context.checkError(this.ctx);
        }
        return new Id(this.ctx, res);
    }
    public PwMultiAff setDimId(DimType type, int pos, Id id) {
        PwMultiAff.Ptr res;
        synchronized(this.ctx) {
            PwMultiAff self = this.asPwMultiAff();
            assert pos >= 0;
            id = id.asId();
            res = Impl.isl.isl_pw_multi_aff_set_dim_id(Impl.isl.isl_pw_multi_aff_copy(self.getPtr()), type.value, pos, Impl.isl.isl_id_copy(id.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwMultiAff(this.ctx, res);
    }
    public boolean plainIsEqual(PwMultiAff pma2) {
        boolean res;
        synchronized(this.ctx) {
            PwMultiAff self = this.asPwMultiAff();
            pma2 = pma2.asPwMultiAff();
            res = Impl.isl.isl_pw_multi_aff_plain_is_equal(self.getPtr(), pma2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public PwMultiAff unionAdd(PwMultiAff pma2) {
        PwMultiAff.Ptr res;
        synchronized(this.ctx) {
            PwMultiAff self = this.asPwMultiAff();
            pma2 = pma2.asPwMultiAff();
            res = Impl.isl.isl_pw_multi_aff_union_add(Impl.isl.isl_pw_multi_aff_copy(self.getPtr()), Impl.isl.isl_pw_multi_aff_copy(pma2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwMultiAff(this.ctx, res);
    }
    public PwMultiAff add(PwMultiAff pma2) {
        PwMultiAff.Ptr res;
        synchronized(this.ctx) {
            PwMultiAff self = this.asPwMultiAff();
            pma2 = pma2.asPwMultiAff();
            res = Impl.isl.isl_pw_multi_aff_add(Impl.isl.isl_pw_multi_aff_copy(self.getPtr()), Impl.isl.isl_pw_multi_aff_copy(pma2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwMultiAff(this.ctx, res);
    }
    public PwMultiAff sub(PwMultiAff pma2) {
        PwMultiAff.Ptr res;
        synchronized(this.ctx) {
            PwMultiAff self = this.asPwMultiAff();
            pma2 = pma2.asPwMultiAff();
            res = Impl.isl.isl_pw_multi_aff_sub(Impl.isl.isl_pw_multi_aff_copy(self.getPtr()), Impl.isl.isl_pw_multi_aff_copy(pma2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwMultiAff(this.ctx, res);
    }
    public PwMultiAff scaleVal(Val v) {
        PwMultiAff.Ptr res;
        synchronized(this.ctx) {
            PwMultiAff self = this.asPwMultiAff();
            v = v.asVal();
            res = Impl.isl.isl_pw_multi_aff_scale_val(Impl.isl.isl_pw_multi_aff_copy(self.getPtr()), Impl.isl.isl_val_copy(v.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwMultiAff(this.ctx, res);
    }
    public PwMultiAff unionLexmin(PwMultiAff pma2) {
        PwMultiAff.Ptr res;
        synchronized(this.ctx) {
            PwMultiAff self = this.asPwMultiAff();
            pma2 = pma2.asPwMultiAff();
            res = Impl.isl.isl_pw_multi_aff_union_lexmin(Impl.isl.isl_pw_multi_aff_copy(self.getPtr()), Impl.isl.isl_pw_multi_aff_copy(pma2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwMultiAff(this.ctx, res);
    }
    public PwMultiAff unionLexmax(PwMultiAff pma2) {
        PwMultiAff.Ptr res;
        synchronized(this.ctx) {
            PwMultiAff self = this.asPwMultiAff();
            pma2 = pma2.asPwMultiAff();
            res = Impl.isl.isl_pw_multi_aff_union_lexmax(Impl.isl.isl_pw_multi_aff_copy(self.getPtr()), Impl.isl.isl_pw_multi_aff_copy(pma2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwMultiAff(this.ctx, res);
    }
    public PwMultiAff rangeProduct(PwMultiAff pma2) {
        PwMultiAff.Ptr res;
        synchronized(this.ctx) {
            PwMultiAff self = this.asPwMultiAff();
            pma2 = pma2.asPwMultiAff();
            res = Impl.isl.isl_pw_multi_aff_range_product(Impl.isl.isl_pw_multi_aff_copy(self.getPtr()), Impl.isl.isl_pw_multi_aff_copy(pma2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwMultiAff(this.ctx, res);
    }
    public PwMultiAff flatRangeProduct(PwMultiAff pma2) {
        PwMultiAff.Ptr res;
        synchronized(this.ctx) {
            PwMultiAff self = this.asPwMultiAff();
            pma2 = pma2.asPwMultiAff();
            res = Impl.isl.isl_pw_multi_aff_flat_range_product(Impl.isl.isl_pw_multi_aff_copy(self.getPtr()), Impl.isl.isl_pw_multi_aff_copy(pma2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwMultiAff(this.ctx, res);
    }
    public PwMultiAff product(PwMultiAff pma2) {
        PwMultiAff.Ptr res;
        synchronized(this.ctx) {
            PwMultiAff self = this.asPwMultiAff();
            pma2 = pma2.asPwMultiAff();
            res = Impl.isl.isl_pw_multi_aff_product(Impl.isl.isl_pw_multi_aff_copy(self.getPtr()), Impl.isl.isl_pw_multi_aff_copy(pma2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwMultiAff(this.ctx, res);
    }
    public PwMultiAff intersectParams(Set set) {
        PwMultiAff.Ptr res;
        synchronized(this.ctx) {
            PwMultiAff self = this.asPwMultiAff();
            set = set.asSet();
            res = Impl.isl.isl_pw_multi_aff_intersect_params(Impl.isl.isl_pw_multi_aff_copy(self.getPtr()), Impl.isl.isl_set_copy(set.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwMultiAff(this.ctx, res);
    }
    public PwMultiAff intersectDomain(Set set) {
        PwMultiAff.Ptr res;
        synchronized(this.ctx) {
            PwMultiAff self = this.asPwMultiAff();
            set = set.asSet();
            res = Impl.isl.isl_pw_multi_aff_intersect_domain(Impl.isl.isl_pw_multi_aff_copy(self.getPtr()), Impl.isl.isl_set_copy(set.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwMultiAff(this.ctx, res);
    }
    public PwMultiAff projectDomainOnParams() {
        PwMultiAff.Ptr res;
        synchronized(this.ctx) {
            PwMultiAff self = this.asPwMultiAff();
            res = Impl.isl.isl_pw_multi_aff_project_domain_on_params(Impl.isl.isl_pw_multi_aff_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwMultiAff(this.ctx, res);
    }
    public PwMultiAff alignParams(Space model) {
        PwMultiAff.Ptr res;
        synchronized(this.ctx) {
            PwMultiAff self = this.asPwMultiAff();
            model = model.asSpace();
            res = Impl.isl.isl_pw_multi_aff_align_params(Impl.isl.isl_pw_multi_aff_copy(self.getPtr()), Impl.isl.isl_space_copy(model.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwMultiAff(this.ctx, res);
    }

}
