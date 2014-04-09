package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public class PwAff {
    public static class Ptr extends PointerType { public Ptr() { super(); } }
    protected Context.Ptr ctx;
    protected PointerType ptr;
    PwAff() {}
    PwAff(Context.Ptr ctx, PointerType ptr) {
        assert !ptr.getPointer().equals(Pointer.NULL);
        this.ctx = ctx;
        this.ptr = ptr;
    }
    PwAff.Ptr getPtr() { return (PwAff.Ptr)this.ptr; }
    PwAff.Ptr makePtr0() { 
        PwAff.Ptr p = (PwAff.Ptr)this.ptr;
        this.ptr = new PwAff.Ptr();
        return p;
    }
    // isl_pw_aff_from_aff
    public PwAff(Aff aff) {
        this.ctx = aff.ctx;
        synchronized(this.ctx) {
            aff = aff.asAff();
            this.ptr = Impl.isl.isl_pw_aff_from_aff(Impl.isl.isl_aff_copy(aff.getPtr()));
            Context.checkError(this.ctx);
        }
    }
    public static PwAff empty(Space dim) {
        PwAff that = new PwAff();
        that.ctx = dim.ctx;
        synchronized(that.ctx) {
            dim = dim.asSpace();
            that.ptr = Impl.isl.isl_pw_aff_empty(Impl.isl.isl_space_copy(dim.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static PwAff alloc(Set set, Aff aff) {
        PwAff that = new PwAff();
        that.ctx = aff.ctx;
        synchronized(that.ctx) {
            set = set.asSet();
            aff = aff.asAff();
            that.ptr = Impl.isl.isl_pw_aff_alloc(Impl.isl.isl_set_copy(set.getPtr()), Impl.isl.isl_aff_copy(aff.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static PwAff zeroOnDomain(LocalSpace ls) {
        PwAff that = new PwAff();
        that.ctx = ls.ctx;
        synchronized(that.ctx) {
            ls = ls.asLocalSpace();
            that.ptr = Impl.isl.isl_pw_aff_zero_on_domain(Impl.isl.isl_local_space_copy(ls.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static PwAff varOnDomain(LocalSpace ls, DimType type, int pos) {
        PwAff that = new PwAff();
        that.ctx = ls.ctx;
        synchronized(that.ctx) {
            ls = ls.asLocalSpace();
            assert pos >= 0;
            that.ptr = Impl.isl.isl_pw_aff_var_on_domain(Impl.isl.isl_local_space_copy(ls.getPtr()), type.value, pos);
            Context.checkError(that.ctx);
        }
        return that;
    }
    protected void finalize() {
        synchronized(this.ctx) {
            Impl.isl.isl_pw_aff_free(getPtr());
        }
    }
    public String toString() {
        Printer p = Printer.toStr();
        p = p.printPwAff(this);
        return p.getStr();
    }
    PwAff asPwAff() {
        Class clazz = this.getClass();
        if (clazz.equals(PwAff.class))
            return this;
        try {
            Constructor<PwAff> c = PwAff.class.getConstructor(clazz);
            return c.newInstance(this);
        } catch (Exception e) {
           throw new IllegalStateException("Cannot construct PwAff from " +
               clazz.getName() + " ?", e);
        }
    }
    public Space getDomainSpace() {
        Space.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            res = Impl.isl.isl_pw_aff_get_domain_space(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public Space getSpace() {
        Space.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            res = Impl.isl.isl_pw_aff_get_space(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public String getDimName(DimType type, int pos) {
        String res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            assert pos >= 0;
            res = Impl.isl.isl_pw_aff_get_dim_name(self.getPtr(), type.value, pos);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean hasDimId(DimType type, int pos) {
        boolean res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            assert pos >= 0;
            res = Impl.isl.isl_pw_aff_has_dim_id(self.getPtr(), type.value, pos);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Id getDimId(DimType type, int pos) {
        Id.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            assert pos >= 0;
            res = Impl.isl.isl_pw_aff_get_dim_id(self.getPtr(), type.value, pos);
            Context.checkError(this.ctx);
        }
        return new Id(this.ctx, res);
    }
    public PwAff setDimId(DimType type, int pos, Id id) {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            assert pos >= 0;
            id = id.asId();
            res = Impl.isl.isl_pw_aff_set_dim_id(Impl.isl.isl_pw_aff_copy(self.getPtr()), type.value, pos, Impl.isl.isl_id_copy(id.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public boolean isEmpty() {
        boolean res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            res = Impl.isl.isl_pw_aff_is_empty(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isEqual(PwAff pa2) {
        boolean res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            pa2 = pa2.asPwAff();
            res = Impl.isl.isl_pw_aff_is_equal(self.getPtr(), pa2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public PwAff unionMin(PwAff pwaff2) {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            pwaff2 = pwaff2.asPwAff();
            res = Impl.isl.isl_pw_aff_union_min(Impl.isl.isl_pw_aff_copy(self.getPtr()), Impl.isl.isl_pw_aff_copy(pwaff2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public PwAff unionMax(PwAff pwaff2) {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            pwaff2 = pwaff2.asPwAff();
            res = Impl.isl.isl_pw_aff_union_max(Impl.isl.isl_pw_aff_copy(self.getPtr()), Impl.isl.isl_pw_aff_copy(pwaff2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public PwAff unionAdd(PwAff pwaff2) {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            pwaff2 = pwaff2.asPwAff();
            res = Impl.isl.isl_pw_aff_union_add(Impl.isl.isl_pw_aff_copy(self.getPtr()), Impl.isl.isl_pw_aff_copy(pwaff2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public int dim(DimType type) {
        int res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            res = Impl.isl.isl_pw_aff_dim(self.getPtr(), type.value);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean involvesDims(DimType type, int first, int n) {
        boolean res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_pw_aff_involves_dims(self.getPtr(), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isCst() {
        boolean res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            res = Impl.isl.isl_pw_aff_is_cst(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public PwAff alignParams(Space model) {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            model = model.asSpace();
            res = Impl.isl.isl_pw_aff_align_params(Impl.isl.isl_pw_aff_copy(self.getPtr()), Impl.isl.isl_space_copy(model.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public boolean hasTupleId(DimType type) {
        boolean res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            res = Impl.isl.isl_pw_aff_has_tuple_id(self.getPtr(), type.value);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Id getTupleId(DimType type) {
        Id.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            res = Impl.isl.isl_pw_aff_get_tuple_id(self.getPtr(), type.value);
            Context.checkError(this.ctx);
        }
        return new Id(this.ctx, res);
    }
    public PwAff setTupleId(DimType type, Id id) {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            id = id.asId();
            res = Impl.isl.isl_pw_aff_set_tuple_id(Impl.isl.isl_pw_aff_copy(self.getPtr()), type.value, Impl.isl.isl_id_copy(id.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public Set params() {
        Set.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            res = Impl.isl.isl_pw_aff_params(Impl.isl.isl_pw_aff_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set domain() {
        Set.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            res = Impl.isl.isl_pw_aff_domain(Impl.isl.isl_pw_aff_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public PwAff fromRange() {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            res = Impl.isl.isl_pw_aff_from_range(Impl.isl.isl_pw_aff_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public PwAff min(PwAff pwaff2) {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            pwaff2 = pwaff2.asPwAff();
            res = Impl.isl.isl_pw_aff_min(Impl.isl.isl_pw_aff_copy(self.getPtr()), Impl.isl.isl_pw_aff_copy(pwaff2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public PwAff max(PwAff pwaff2) {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            pwaff2 = pwaff2.asPwAff();
            res = Impl.isl.isl_pw_aff_max(Impl.isl.isl_pw_aff_copy(self.getPtr()), Impl.isl.isl_pw_aff_copy(pwaff2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public PwAff mul(PwAff pwaff2) {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            pwaff2 = pwaff2.asPwAff();
            res = Impl.isl.isl_pw_aff_mul(Impl.isl.isl_pw_aff_copy(self.getPtr()), Impl.isl.isl_pw_aff_copy(pwaff2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public PwAff div(PwAff pa2) {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            pa2 = pa2.asPwAff();
            res = Impl.isl.isl_pw_aff_div(Impl.isl.isl_pw_aff_copy(self.getPtr()), Impl.isl.isl_pw_aff_copy(pa2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public PwAff add(PwAff pwaff2) {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            pwaff2 = pwaff2.asPwAff();
            res = Impl.isl.isl_pw_aff_add(Impl.isl.isl_pw_aff_copy(self.getPtr()), Impl.isl.isl_pw_aff_copy(pwaff2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public PwAff sub(PwAff pwaff2) {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            pwaff2 = pwaff2.asPwAff();
            res = Impl.isl.isl_pw_aff_sub(Impl.isl.isl_pw_aff_copy(self.getPtr()), Impl.isl.isl_pw_aff_copy(pwaff2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public PwAff neg() {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            res = Impl.isl.isl_pw_aff_neg(Impl.isl.isl_pw_aff_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public PwAff ceil() {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            res = Impl.isl.isl_pw_aff_ceil(Impl.isl.isl_pw_aff_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public PwAff floor() {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            res = Impl.isl.isl_pw_aff_floor(Impl.isl.isl_pw_aff_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public PwAff modVal(Val mod) {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            mod = mod.asVal();
            res = Impl.isl.isl_pw_aff_mod_val(Impl.isl.isl_pw_aff_copy(self.getPtr()), Impl.isl.isl_val_copy(mod.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public PwAff tdivQ(PwAff pa2) {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            pa2 = pa2.asPwAff();
            res = Impl.isl.isl_pw_aff_tdiv_q(Impl.isl.isl_pw_aff_copy(self.getPtr()), Impl.isl.isl_pw_aff_copy(pa2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public PwAff tdivR(PwAff pa2) {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            pa2 = pa2.asPwAff();
            res = Impl.isl.isl_pw_aff_tdiv_r(Impl.isl.isl_pw_aff_copy(self.getPtr()), Impl.isl.isl_pw_aff_copy(pa2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public PwAff intersectParams(Set set) {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            set = set.asSet();
            res = Impl.isl.isl_pw_aff_intersect_params(Impl.isl.isl_pw_aff_copy(self.getPtr()), Impl.isl.isl_set_copy(set.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public PwAff intersectDomain(Set set) {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            set = set.asSet();
            res = Impl.isl.isl_pw_aff_intersect_domain(Impl.isl.isl_pw_aff_copy(self.getPtr()), Impl.isl.isl_set_copy(set.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public PwAff cond(PwAff pwaff_true, PwAff pwaff_false) {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            pwaff_true = pwaff_true.asPwAff();
            pwaff_false = pwaff_false.asPwAff();
            res = Impl.isl.isl_pw_aff_cond(Impl.isl.isl_pw_aff_copy(self.getPtr()), Impl.isl.isl_pw_aff_copy(pwaff_true.getPtr()), Impl.isl.isl_pw_aff_copy(pwaff_false.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public PwAff scaleVal(Val v) {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            v = v.asVal();
            res = Impl.isl.isl_pw_aff_scale_val(Impl.isl.isl_pw_aff_copy(self.getPtr()), Impl.isl.isl_val_copy(v.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public PwAff scaleDownVal(Val f) {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            f = f.asVal();
            res = Impl.isl.isl_pw_aff_scale_down_val(Impl.isl.isl_pw_aff_copy(self.getPtr()), Impl.isl.isl_val_copy(f.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public PwAff insertDims(DimType type, int first, int n) {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_pw_aff_insert_dims(Impl.isl.isl_pw_aff_copy(self.getPtr()), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public PwAff addDims(DimType type, int n) {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            assert n >= 0;
            res = Impl.isl.isl_pw_aff_add_dims(Impl.isl.isl_pw_aff_copy(self.getPtr()), type.value, n);
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public PwAff moveDims(DimType dst_type, int dst_pos, DimType src_type, int src_pos, int n) {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            assert dst_pos >= 0;
            assert src_pos >= 0;
            assert n >= 0;
            res = Impl.isl.isl_pw_aff_move_dims(Impl.isl.isl_pw_aff_copy(self.getPtr()), dst_type.value, dst_pos, src_type.value, src_pos, n);
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public PwAff dropDims(DimType type, int first, int n) {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_pw_aff_drop_dims(Impl.isl.isl_pw_aff_copy(self.getPtr()), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public PwAff coalesce() {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            res = Impl.isl.isl_pw_aff_coalesce(Impl.isl.isl_pw_aff_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public PwAff gist(Set context) {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            context = context.asSet();
            res = Impl.isl.isl_pw_aff_gist(Impl.isl.isl_pw_aff_copy(self.getPtr()), Impl.isl.isl_set_copy(context.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public PwAff gistParams(Set context) {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            context = context.asSet();
            res = Impl.isl.isl_pw_aff_gist_params(Impl.isl.isl_pw_aff_copy(self.getPtr()), Impl.isl.isl_set_copy(context.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public PwAff pullbackMultiAff(MultiAff ma) {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            ma = ma.asMultiAff();
            res = Impl.isl.isl_pw_aff_pullback_multi_aff(Impl.isl.isl_pw_aff_copy(self.getPtr()), Impl.isl.isl_multi_aff_copy(ma.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public PwAff pullbackPwMultiAff(PwMultiAff pma) {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            pma = pma.asPwMultiAff();
            res = Impl.isl.isl_pw_aff_pullback_pw_multi_aff(Impl.isl.isl_pw_aff_copy(self.getPtr()), Impl.isl.isl_pw_multi_aff_copy(pma.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public int nPiece() {
        int res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            res = Impl.isl.isl_pw_aff_n_piece(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public <ExceptionTy extends Exception> void foreachPiece(final XCallback2<Set,Aff,ExceptionTy> fn) throws ExceptionTy {
        int res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            final Context.Ptr _ctx = this.ctx;
            final Throwable[] exc_info = new Throwable[1];
            Callback cb = new Callback() {
                public int callback(Set.Ptr cb_arg0, Aff.Ptr cb_arg1, Pointer _user) {
                    try {
                    fn.apply(new Set(_ctx, cb_arg0), new Aff(_ctx, cb_arg1));
                    } catch (Throwable e) {
                        exc_info[0] = e;
                        return -1;
                    }
                    return 0;
                }
            };
            res = Impl.isl.isl_pw_aff_foreach_piece(self.getPtr(), cb, Pointer.NULL);
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
    public Set nonnegSet() {
        Set.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            res = Impl.isl.isl_pw_aff_nonneg_set(Impl.isl.isl_pw_aff_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set zeroSet() {
        Set.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            res = Impl.isl.isl_pw_aff_zero_set(Impl.isl.isl_pw_aff_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set nonZeroSet() {
        Set.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            res = Impl.isl.isl_pw_aff_non_zero_set(Impl.isl.isl_pw_aff_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set eqSet(PwAff pwaff2) {
        Set.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            pwaff2 = pwaff2.asPwAff();
            res = Impl.isl.isl_pw_aff_eq_set(Impl.isl.isl_pw_aff_copy(self.getPtr()), Impl.isl.isl_pw_aff_copy(pwaff2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set neSet(PwAff pwaff2) {
        Set.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            pwaff2 = pwaff2.asPwAff();
            res = Impl.isl.isl_pw_aff_ne_set(Impl.isl.isl_pw_aff_copy(self.getPtr()), Impl.isl.isl_pw_aff_copy(pwaff2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set leSet(PwAff pwaff2) {
        Set.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            pwaff2 = pwaff2.asPwAff();
            res = Impl.isl.isl_pw_aff_le_set(Impl.isl.isl_pw_aff_copy(self.getPtr()), Impl.isl.isl_pw_aff_copy(pwaff2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set ltSet(PwAff pwaff2) {
        Set.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            pwaff2 = pwaff2.asPwAff();
            res = Impl.isl.isl_pw_aff_lt_set(Impl.isl.isl_pw_aff_copy(self.getPtr()), Impl.isl.isl_pw_aff_copy(pwaff2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set geSet(PwAff pwaff2) {
        Set.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            pwaff2 = pwaff2.asPwAff();
            res = Impl.isl.isl_pw_aff_ge_set(Impl.isl.isl_pw_aff_copy(self.getPtr()), Impl.isl.isl_pw_aff_copy(pwaff2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set gtSet(PwAff pwaff2) {
        Set.Ptr res;
        synchronized(this.ctx) {
            PwAff self = this.asPwAff();
            pwaff2 = pwaff2.asPwAff();
            res = Impl.isl.isl_pw_aff_gt_set(Impl.isl.isl_pw_aff_copy(self.getPtr()), Impl.isl.isl_pw_aff_copy(pwaff2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }

}
