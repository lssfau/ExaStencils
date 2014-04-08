package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public class PwQpolynomial {
    public static class Ptr extends PointerType { public Ptr() { super(); } }
    protected Context.Ptr ctx;
    protected PointerType ptr;
    PwQpolynomial() {}
    PwQpolynomial(Context.Ptr ctx, PointerType ptr) {
        assert !ptr.getPointer().equals(Pointer.NULL);
        this.ctx = ctx;
        this.ptr = ptr;
    }
    PwQpolynomial.Ptr getPtr() { return (PwQpolynomial.Ptr)this.ptr; }
    PwQpolynomial.Ptr makePtr0() { 
        PwQpolynomial.Ptr p = (PwQpolynomial.Ptr)this.ptr;
        this.ptr = new PwQpolynomial.Ptr();
        return p;
    }
    public static PwQpolynomial zero(Space dim) {
        PwQpolynomial that = new PwQpolynomial();
        that.ctx = dim.ctx;
        synchronized(that.ctx) {
            dim = dim.asSpace();
            that.ptr = Impl.isl.isl_pw_qpolynomial_zero(Impl.isl.isl_space_copy(dim.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static PwQpolynomial alloc(Set set, Qpolynomial qp) {
        PwQpolynomial that = new PwQpolynomial();
        that.ctx = qp.ctx;
        synchronized(that.ctx) {
            set = set.asSet();
            qp = qp.asQpolynomial();
            that.ptr = Impl.isl.isl_pw_qpolynomial_alloc(Impl.isl.isl_set_copy(set.getPtr()), Impl.isl.isl_qpolynomial_copy(qp.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    // isl_pw_qpolynomial_from_qpolynomial
    public PwQpolynomial(Qpolynomial qp) {
        this.ctx = qp.ctx;
        synchronized(this.ctx) {
            qp = qp.asQpolynomial();
            this.ptr = Impl.isl.isl_pw_qpolynomial_from_qpolynomial(Impl.isl.isl_qpolynomial_copy(qp.getPtr()));
            Context.checkError(this.ctx);
        }
    }
    // isl_pw_qpolynomial_from_pw_aff
    public PwQpolynomial(PwAff pwaff) {
        this.ctx = pwaff.ctx;
        synchronized(this.ctx) {
            pwaff = pwaff.asPwAff();
            this.ptr = Impl.isl.isl_pw_qpolynomial_from_pw_aff(Impl.isl.isl_pw_aff_copy(pwaff.getPtr()));
            Context.checkError(this.ctx);
        }
    }
    // isl_pw_qpolynomial_read_from_str
    public PwQpolynomial(String str) {
        this.ctx = Context.getDefaultInstance();
        synchronized(this.ctx) {
            this.ptr = Impl.isl.isl_pw_qpolynomial_read_from_str(this.ctx, str);
            Context.checkError(this.ctx);
        }
    }
    protected void finalize() {
        synchronized(this.ctx) {
            Impl.isl.isl_pw_qpolynomial_free(getPtr());
        }
    }
    public String toString() {
        Printer p = Printer.toStr();
        p = p.printPwQpolynomial(this);
        return p.getStr();
    }
    PwQpolynomial asPwQpolynomial() {
        Class clazz = this.getClass();
        if (clazz.equals(PwQpolynomial.class))
            return this;
        try {
            Constructor<PwQpolynomial> c = PwQpolynomial.class.getConstructor(clazz);
            return c.newInstance(this);
        } catch (Exception e) {
           throw new IllegalStateException("Cannot construct PwQpolynomial from " +
               clazz.getName() + " ?", e);
        }
    }
    public boolean plainIsEqual(PwQpolynomial pwqp2) {
        boolean res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            pwqp2 = pwqp2.asPwQpolynomial();
            res = Impl.isl.isl_pw_qpolynomial_plain_is_equal(self.getPtr(), pwqp2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isZero() {
        boolean res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            res = Impl.isl.isl_pw_qpolynomial_is_zero(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Space getDomainSpace() {
        Space.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            res = Impl.isl.isl_pw_qpolynomial_get_domain_space(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public Space getSpace() {
        Space.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            res = Impl.isl.isl_pw_qpolynomial_get_space(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public PwQpolynomial resetDomainSpace(Space dim) {
        PwQpolynomial.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            dim = dim.asSpace();
            res = Impl.isl.isl_pw_qpolynomial_reset_domain_space(Impl.isl.isl_pw_qpolynomial_copy(self.getPtr()), Impl.isl.isl_space_copy(dim.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwQpolynomial(this.ctx, res);
    }
    public int dim(DimType type) {
        int res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            res = Impl.isl.isl_pw_qpolynomial_dim(self.getPtr(), type.value);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean involvesDims(DimType type, int first, int n) {
        boolean res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_pw_qpolynomial_involves_dims(self.getPtr(), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean hasEqualSpace(PwQpolynomial pwqp2) {
        boolean res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            pwqp2 = pwqp2.asPwQpolynomial();
            res = Impl.isl.isl_pw_qpolynomial_has_equal_space(self.getPtr(), pwqp2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public PwQpolynomial setDimName(DimType type, int pos, String s) {
        PwQpolynomial.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            assert pos >= 0;
            res = Impl.isl.isl_pw_qpolynomial_set_dim_name(Impl.isl.isl_pw_qpolynomial_copy(self.getPtr()), type.value, pos, s);
            Context.checkError(this.ctx);
        }
        return new PwQpolynomial(this.ctx, res);
    }
    public Set domain() {
        Set.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            res = Impl.isl.isl_pw_qpolynomial_domain(Impl.isl.isl_pw_qpolynomial_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public PwQpolynomial intersectDomain(Set set) {
        PwQpolynomial.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            set = set.asSet();
            res = Impl.isl.isl_pw_qpolynomial_intersect_domain(Impl.isl.isl_pw_qpolynomial_copy(self.getPtr()), Impl.isl.isl_set_copy(set.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwQpolynomial(this.ctx, res);
    }
    public PwQpolynomial intersectParams(Set set) {
        PwQpolynomial.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            set = set.asSet();
            res = Impl.isl.isl_pw_qpolynomial_intersect_params(Impl.isl.isl_pw_qpolynomial_copy(self.getPtr()), Impl.isl.isl_set_copy(set.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwQpolynomial(this.ctx, res);
    }
    public PwQpolynomial projectDomainOnParams() {
        PwQpolynomial.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            res = Impl.isl.isl_pw_qpolynomial_project_domain_on_params(Impl.isl.isl_pw_qpolynomial_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwQpolynomial(this.ctx, res);
    }
    public PwQpolynomial dropDims(DimType type, int first, int n) {
        PwQpolynomial.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_pw_qpolynomial_drop_dims(Impl.isl.isl_pw_qpolynomial_copy(self.getPtr()), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return new PwQpolynomial(this.ctx, res);
    }
    public PwQpolynomial splitDims(DimType type, int first, int n) {
        PwQpolynomial.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_pw_qpolynomial_split_dims(Impl.isl.isl_pw_qpolynomial_copy(self.getPtr()), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return new PwQpolynomial(this.ctx, res);
    }
    public PwQpolynomial add(PwQpolynomial pwqp2) {
        PwQpolynomial.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            pwqp2 = pwqp2.asPwQpolynomial();
            res = Impl.isl.isl_pw_qpolynomial_add(Impl.isl.isl_pw_qpolynomial_copy(self.getPtr()), Impl.isl.isl_pw_qpolynomial_copy(pwqp2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwQpolynomial(this.ctx, res);
    }
    public PwQpolynomial sub(PwQpolynomial pwqp2) {
        PwQpolynomial.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            pwqp2 = pwqp2.asPwQpolynomial();
            res = Impl.isl.isl_pw_qpolynomial_sub(Impl.isl.isl_pw_qpolynomial_copy(self.getPtr()), Impl.isl.isl_pw_qpolynomial_copy(pwqp2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwQpolynomial(this.ctx, res);
    }
    public PwQpolynomial addDisjoint(PwQpolynomial pwqp2) {
        PwQpolynomial.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            pwqp2 = pwqp2.asPwQpolynomial();
            res = Impl.isl.isl_pw_qpolynomial_add_disjoint(Impl.isl.isl_pw_qpolynomial_copy(self.getPtr()), Impl.isl.isl_pw_qpolynomial_copy(pwqp2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwQpolynomial(this.ctx, res);
    }
    public PwQpolynomial neg() {
        PwQpolynomial.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            res = Impl.isl.isl_pw_qpolynomial_neg(Impl.isl.isl_pw_qpolynomial_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwQpolynomial(this.ctx, res);
    }
    public PwQpolynomial mul(PwQpolynomial pwqp2) {
        PwQpolynomial.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            pwqp2 = pwqp2.asPwQpolynomial();
            res = Impl.isl.isl_pw_qpolynomial_mul(Impl.isl.isl_pw_qpolynomial_copy(self.getPtr()), Impl.isl.isl_pw_qpolynomial_copy(pwqp2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwQpolynomial(this.ctx, res);
    }
    public PwQpolynomial scaleVal(Val v) {
        PwQpolynomial.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            v = v.asVal();
            res = Impl.isl.isl_pw_qpolynomial_scale_val(Impl.isl.isl_pw_qpolynomial_copy(self.getPtr()), Impl.isl.isl_val_copy(v.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwQpolynomial(this.ctx, res);
    }
    public PwQpolynomial pow(int exponent) {
        PwQpolynomial.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            assert exponent >= 0;
            res = Impl.isl.isl_pw_qpolynomial_pow(Impl.isl.isl_pw_qpolynomial_copy(self.getPtr()), exponent);
            Context.checkError(this.ctx);
        }
        return new PwQpolynomial(this.ctx, res);
    }
    public PwQpolynomial insertDims(DimType type, int first, int n) {
        PwQpolynomial.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_pw_qpolynomial_insert_dims(Impl.isl.isl_pw_qpolynomial_copy(self.getPtr()), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return new PwQpolynomial(this.ctx, res);
    }
    public PwQpolynomial addDims(DimType type, int n) {
        PwQpolynomial.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            assert n >= 0;
            res = Impl.isl.isl_pw_qpolynomial_add_dims(Impl.isl.isl_pw_qpolynomial_copy(self.getPtr()), type.value, n);
            Context.checkError(this.ctx);
        }
        return new PwQpolynomial(this.ctx, res);
    }
    public PwQpolynomial moveDims(DimType dst_type, int dst_pos, DimType src_type, int src_pos, int n) {
        PwQpolynomial.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            assert dst_pos >= 0;
            assert src_pos >= 0;
            assert n >= 0;
            res = Impl.isl.isl_pw_qpolynomial_move_dims(Impl.isl.isl_pw_qpolynomial_copy(self.getPtr()), dst_type.value, dst_pos, src_type.value, src_pos, n);
            Context.checkError(this.ctx);
        }
        return new PwQpolynomial(this.ctx, res);
    }
    public PwQpolynomial fixVal(DimType type, int n, Val v) {
        PwQpolynomial.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            assert n >= 0;
            v = v.asVal();
            res = Impl.isl.isl_pw_qpolynomial_fix_val(Impl.isl.isl_pw_qpolynomial_copy(self.getPtr()), type.value, n, Impl.isl.isl_val_copy(v.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwQpolynomial(this.ctx, res);
    }
    public Val eval(Point pnt) {
        Val.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            pnt = pnt.asPoint();
            res = Impl.isl.isl_pw_qpolynomial_eval(Impl.isl.isl_pw_qpolynomial_copy(self.getPtr()), Impl.isl.isl_point_copy(pnt.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Val(this.ctx, res);
    }
    public Val max() {
        Val.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            res = Impl.isl.isl_pw_qpolynomial_max(Impl.isl.isl_pw_qpolynomial_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Val(this.ctx, res);
    }
    public Val min() {
        Val.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            res = Impl.isl.isl_pw_qpolynomial_min(Impl.isl.isl_pw_qpolynomial_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Val(this.ctx, res);
    }
    public <ExceptionTy extends Exception> void foreachPiece(final XCallback2<Set,Qpolynomial,ExceptionTy> fn) throws ExceptionTy {
        int res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            final Context.Ptr _ctx = this.ctx;
            final Throwable[] exc_info = new Throwable[1];
            Callback cb = new Callback() {
                public int callback(Set.Ptr cb_arg0, Qpolynomial.Ptr cb_arg1, Pointer _user) {
                    try {
                    fn.apply(new Set(_ctx, cb_arg0), new Qpolynomial(_ctx, cb_arg1));
                    } catch (Throwable e) {
                        exc_info[0] = e;
                        return -1;
                    }
                    return 0;
                }
            };
            res = Impl.isl.isl_pw_qpolynomial_foreach_piece(self.getPtr(), cb, Pointer.NULL);
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
    public <ExceptionTy extends Exception> void foreachLiftedPiece(final XCallback2<Set,Qpolynomial,ExceptionTy> fn) throws ExceptionTy {
        int res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            final Context.Ptr _ctx = this.ctx;
            final Throwable[] exc_info = new Throwable[1];
            Callback cb = new Callback() {
                public int callback(Set.Ptr cb_arg0, Qpolynomial.Ptr cb_arg1, Pointer _user) {
                    try {
                    fn.apply(new Set(_ctx, cb_arg0), new Qpolynomial(_ctx, cb_arg1));
                    } catch (Throwable e) {
                        exc_info[0] = e;
                        return -1;
                    }
                    return 0;
                }
            };
            res = Impl.isl.isl_pw_qpolynomial_foreach_lifted_piece(self.getPtr(), cb, Pointer.NULL);
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
    public PwQpolynomial coalesce() {
        PwQpolynomial.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            res = Impl.isl.isl_pw_qpolynomial_coalesce(Impl.isl.isl_pw_qpolynomial_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwQpolynomial(this.ctx, res);
    }
    public PwQpolynomial gist(Set context) {
        PwQpolynomial.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            context = context.asSet();
            res = Impl.isl.isl_pw_qpolynomial_gist(Impl.isl.isl_pw_qpolynomial_copy(self.getPtr()), Impl.isl.isl_set_copy(context.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwQpolynomial(this.ctx, res);
    }
    public PwQpolynomial gistParams(Set context) {
        PwQpolynomial.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            context = context.asSet();
            res = Impl.isl.isl_pw_qpolynomial_gist_params(Impl.isl.isl_pw_qpolynomial_copy(self.getPtr()), Impl.isl.isl_set_copy(context.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwQpolynomial(this.ctx, res);
    }
    public PwQpolynomial splitPeriods(int max_periods) {
        PwQpolynomial.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            res = Impl.isl.isl_pw_qpolynomial_split_periods(Impl.isl.isl_pw_qpolynomial_copy(self.getPtr()), max_periods);
            Context.checkError(this.ctx);
        }
        return new PwQpolynomial(this.ctx, res);
    }
    public PwQpolynomialFold bound(Fold type, boolean[] tight) {
        PwQpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            res = Impl.isl.isl_pw_qpolynomial_bound(Impl.isl.isl_pw_qpolynomial_copy(self.getPtr()), type.value, tight);
            Context.checkError(this.ctx);
        }
        return new PwQpolynomialFold(this.ctx, res);
    }
    public PwQpolynomial toPolynomial(int sign) {
        PwQpolynomial.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomial self = this.asPwQpolynomial();
            res = Impl.isl.isl_pw_qpolynomial_to_polynomial(Impl.isl.isl_pw_qpolynomial_copy(self.getPtr()), sign);
            Context.checkError(this.ctx);
        }
        return new PwQpolynomial(this.ctx, res);
    }

}
