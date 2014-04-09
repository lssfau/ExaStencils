package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public class Aff {
    public static class Ptr extends PointerType { public Ptr() { super(); } }
    protected Context.Ptr ctx;
    protected PointerType ptr;
    Aff() {}
    Aff(Context.Ptr ctx, PointerType ptr) {
        assert !ptr.getPointer().equals(Pointer.NULL);
        this.ctx = ctx;
        this.ptr = ptr;
    }
    Aff.Ptr getPtr() { return (Aff.Ptr)this.ptr; }
    Aff.Ptr makePtr0() { 
        Aff.Ptr p = (Aff.Ptr)this.ptr;
        this.ptr = new Aff.Ptr();
        return p;
    }
    public static Aff zeroOnDomain(LocalSpace ls) {
        Aff that = new Aff();
        that.ctx = ls.ctx;
        synchronized(that.ctx) {
            ls = ls.asLocalSpace();
            that.ptr = Impl.isl.isl_aff_zero_on_domain(Impl.isl.isl_local_space_copy(ls.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static Aff valOnDomain(LocalSpace ls, Val val) {
        Aff that = new Aff();
        that.ctx = val.ctx;
        synchronized(that.ctx) {
            ls = ls.asLocalSpace();
            val = val.asVal();
            that.ptr = Impl.isl.isl_aff_val_on_domain(Impl.isl.isl_local_space_copy(ls.getPtr()), Impl.isl.isl_val_copy(val.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static Aff varOnDomain(LocalSpace ls, DimType type, int pos) {
        Aff that = new Aff();
        that.ctx = ls.ctx;
        synchronized(that.ctx) {
            ls = ls.asLocalSpace();
            assert pos >= 0;
            that.ptr = Impl.isl.isl_aff_var_on_domain(Impl.isl.isl_local_space_copy(ls.getPtr()), type.value, pos);
            Context.checkError(that.ctx);
        }
        return that;
    }
    protected void finalize() {
        synchronized(this.ctx) {
            Impl.isl.isl_aff_free(getPtr());
        }
    }
    public String toString() {
        Printer p = Printer.toStr();
        p = p.printAff(this);
        return p.getStr();
    }
    Aff asAff() {
        Class clazz = this.getClass();
        if (clazz.equals(Aff.class))
            return this;
        try {
            Constructor<Aff> c = Aff.class.getConstructor(clazz);
            return c.newInstance(this);
        } catch (Exception e) {
           throw new IllegalStateException("Cannot construct Aff from " +
               clazz.getName() + " ?", e);
        }
    }
    public int dim(DimType type) {
        int res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            res = Impl.isl.isl_aff_dim(self.getPtr(), type.value);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean involvesDims(DimType type, int first, int n) {
        boolean res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_aff_involves_dims(self.getPtr(), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Space getDomainSpace() {
        Space.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            res = Impl.isl.isl_aff_get_domain_space(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public Space getSpace() {
        Space.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            res = Impl.isl.isl_aff_get_space(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public LocalSpace getDomainLocalSpace() {
        LocalSpace.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            res = Impl.isl.isl_aff_get_domain_local_space(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new LocalSpace(this.ctx, res);
    }
    public LocalSpace getLocalSpace() {
        LocalSpace.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            res = Impl.isl.isl_aff_get_local_space(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new LocalSpace(this.ctx, res);
    }
    public String getDimName(DimType type, int pos) {
        String res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            assert pos >= 0;
            res = Impl.isl.isl_aff_get_dim_name(self.getPtr(), type.value, pos);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Val getConstantVal() {
        Val.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            res = Impl.isl.isl_aff_get_constant_val(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new Val(this.ctx, res);
    }
    public Val getCoefficientVal(DimType type, int pos) {
        Val.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            res = Impl.isl.isl_aff_get_coefficient_val(self.getPtr(), type.value, pos);
            Context.checkError(this.ctx);
        }
        return new Val(this.ctx, res);
    }
    public Val getDenominatorVal() {
        Val.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            res = Impl.isl.isl_aff_get_denominator_val(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new Val(this.ctx, res);
    }
    public Aff setConstantSi(int v) {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            res = Impl.isl.isl_aff_set_constant_si(Impl.isl.isl_aff_copy(self.getPtr()), v);
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public Aff setConstantVal(Val v) {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            v = v.asVal();
            res = Impl.isl.isl_aff_set_constant_val(Impl.isl.isl_aff_copy(self.getPtr()), Impl.isl.isl_val_copy(v.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public Aff setCoefficientSi(DimType type, int pos, int v) {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            res = Impl.isl.isl_aff_set_coefficient_si(Impl.isl.isl_aff_copy(self.getPtr()), type.value, pos, v);
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public Aff setCoefficientVal(DimType type, int pos, Val v) {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            v = v.asVal();
            res = Impl.isl.isl_aff_set_coefficient_val(Impl.isl.isl_aff_copy(self.getPtr()), type.value, pos, Impl.isl.isl_val_copy(v.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public Aff addConstantSi(int v) {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            res = Impl.isl.isl_aff_add_constant_si(Impl.isl.isl_aff_copy(self.getPtr()), v);
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public Aff addConstantVal(Val v) {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            v = v.asVal();
            res = Impl.isl.isl_aff_add_constant_val(Impl.isl.isl_aff_copy(self.getPtr()), Impl.isl.isl_val_copy(v.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public Aff addConstantNumSi(int v) {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            res = Impl.isl.isl_aff_add_constant_num_si(Impl.isl.isl_aff_copy(self.getPtr()), v);
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public Aff addCoefficientSi(DimType type, int pos, int v) {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            res = Impl.isl.isl_aff_add_coefficient_si(Impl.isl.isl_aff_copy(self.getPtr()), type.value, pos, v);
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public Aff addCoefficientVal(DimType type, int pos, Val v) {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            v = v.asVal();
            res = Impl.isl.isl_aff_add_coefficient_val(Impl.isl.isl_aff_copy(self.getPtr()), type.value, pos, Impl.isl.isl_val_copy(v.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public boolean isCst() {
        boolean res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            res = Impl.isl.isl_aff_is_cst(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Aff setTupleId(DimType type, Id id) {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            id = id.asId();
            res = Impl.isl.isl_aff_set_tuple_id(Impl.isl.isl_aff_copy(self.getPtr()), type.value, Impl.isl.isl_id_copy(id.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public Aff setDimName(DimType type, int pos, String s) {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            assert pos >= 0;
            res = Impl.isl.isl_aff_set_dim_name(Impl.isl.isl_aff_copy(self.getPtr()), type.value, pos, s);
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public Aff setDimId(DimType type, int pos, Id id) {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            assert pos >= 0;
            id = id.asId();
            res = Impl.isl.isl_aff_set_dim_id(Impl.isl.isl_aff_copy(self.getPtr()), type.value, pos, Impl.isl.isl_id_copy(id.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public boolean plainIsEqual(Aff aff2) {
        boolean res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            aff2 = aff2.asAff();
            res = Impl.isl.isl_aff_plain_is_equal(self.getPtr(), aff2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean plainIsZero() {
        boolean res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            res = Impl.isl.isl_aff_plain_is_zero(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Aff getDiv(int pos) {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            res = Impl.isl.isl_aff_get_div(self.getPtr(), pos);
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public Aff neg() {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            res = Impl.isl.isl_aff_neg(Impl.isl.isl_aff_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public Aff ceil() {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            res = Impl.isl.isl_aff_ceil(Impl.isl.isl_aff_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public Aff floor() {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            res = Impl.isl.isl_aff_floor(Impl.isl.isl_aff_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public Aff modVal(Val mod) {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            mod = mod.asVal();
            res = Impl.isl.isl_aff_mod_val(Impl.isl.isl_aff_copy(self.getPtr()), Impl.isl.isl_val_copy(mod.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public Aff mul(Aff aff2) {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            aff2 = aff2.asAff();
            res = Impl.isl.isl_aff_mul(Impl.isl.isl_aff_copy(self.getPtr()), Impl.isl.isl_aff_copy(aff2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public Aff div(Aff aff2) {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            aff2 = aff2.asAff();
            res = Impl.isl.isl_aff_div(Impl.isl.isl_aff_copy(self.getPtr()), Impl.isl.isl_aff_copy(aff2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public Aff add(Aff aff2) {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            aff2 = aff2.asAff();
            res = Impl.isl.isl_aff_add(Impl.isl.isl_aff_copy(self.getPtr()), Impl.isl.isl_aff_copy(aff2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public Aff sub(Aff aff2) {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            aff2 = aff2.asAff();
            res = Impl.isl.isl_aff_sub(Impl.isl.isl_aff_copy(self.getPtr()), Impl.isl.isl_aff_copy(aff2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public Aff scaleVal(Val v) {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            v = v.asVal();
            res = Impl.isl.isl_aff_scale_val(Impl.isl.isl_aff_copy(self.getPtr()), Impl.isl.isl_val_copy(v.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public Aff scaleDownUi(int f) {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            assert f >= 0;
            res = Impl.isl.isl_aff_scale_down_ui(Impl.isl.isl_aff_copy(self.getPtr()), f);
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public Aff scaleDownVal(Val v) {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            v = v.asVal();
            res = Impl.isl.isl_aff_scale_down_val(Impl.isl.isl_aff_copy(self.getPtr()), Impl.isl.isl_val_copy(v.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public Aff insertDims(DimType type, int first, int n) {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_aff_insert_dims(Impl.isl.isl_aff_copy(self.getPtr()), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public Aff addDims(DimType type, int n) {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            assert n >= 0;
            res = Impl.isl.isl_aff_add_dims(Impl.isl.isl_aff_copy(self.getPtr()), type.value, n);
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public Aff moveDims(DimType dst_type, int dst_pos, DimType src_type, int src_pos, int n) {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            assert dst_pos >= 0;
            assert src_pos >= 0;
            assert n >= 0;
            res = Impl.isl.isl_aff_move_dims(Impl.isl.isl_aff_copy(self.getPtr()), dst_type.value, dst_pos, src_type.value, src_pos, n);
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public Aff dropDims(DimType type, int first, int n) {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_aff_drop_dims(Impl.isl.isl_aff_copy(self.getPtr()), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public Aff projectDomainOnParams() {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            res = Impl.isl.isl_aff_project_domain_on_params(Impl.isl.isl_aff_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public Aff alignParams(Space model) {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            model = model.asSpace();
            res = Impl.isl.isl_aff_align_params(Impl.isl.isl_aff_copy(self.getPtr()), Impl.isl.isl_space_copy(model.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public Aff gist(Set context) {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            context = context.asSet();
            res = Impl.isl.isl_aff_gist(Impl.isl.isl_aff_copy(self.getPtr()), Impl.isl.isl_set_copy(context.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public Aff gistParams(Set context) {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            context = context.asSet();
            res = Impl.isl.isl_aff_gist_params(Impl.isl.isl_aff_copy(self.getPtr()), Impl.isl.isl_set_copy(context.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public Aff pullbackAff(Aff aff2) {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            aff2 = aff2.asAff();
            res = Impl.isl.isl_aff_pullback_aff(Impl.isl.isl_aff_copy(self.getPtr()), Impl.isl.isl_aff_copy(aff2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public Aff pullbackMultiAff(MultiAff ma) {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            ma = ma.asMultiAff();
            res = Impl.isl.isl_aff_pullback_multi_aff(Impl.isl.isl_aff_copy(self.getPtr()), Impl.isl.isl_multi_aff_copy(ma.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public BasicSet zeroBasicSet() {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            res = Impl.isl.isl_aff_zero_basic_set(Impl.isl.isl_aff_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public BasicSet negBasicSet() {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            res = Impl.isl.isl_aff_neg_basic_set(Impl.isl.isl_aff_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public BasicSet leBasicSet(Aff aff2) {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            aff2 = aff2.asAff();
            res = Impl.isl.isl_aff_le_basic_set(Impl.isl.isl_aff_copy(self.getPtr()), Impl.isl.isl_aff_copy(aff2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public BasicSet geBasicSet(Aff aff2) {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            Aff self = this.asAff();
            aff2 = aff2.asAff();
            res = Impl.isl.isl_aff_ge_basic_set(Impl.isl.isl_aff_copy(self.getPtr()), Impl.isl.isl_aff_copy(aff2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }

}
