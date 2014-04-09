package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public class Qpolynomial {
    public static class Ptr extends PointerType { public Ptr() { super(); } }
    protected Context.Ptr ctx;
    protected PointerType ptr;
    Qpolynomial() {}
    Qpolynomial(Context.Ptr ctx, PointerType ptr) {
        assert !ptr.getPointer().equals(Pointer.NULL);
        this.ctx = ctx;
        this.ptr = ptr;
    }
    Qpolynomial.Ptr getPtr() { return (Qpolynomial.Ptr)this.ptr; }
    Qpolynomial.Ptr makePtr0() { 
        Qpolynomial.Ptr p = (Qpolynomial.Ptr)this.ptr;
        this.ptr = new Qpolynomial.Ptr();
        return p;
    }
    public static Qpolynomial zeroOnDomain(Space dim) {
        Qpolynomial that = new Qpolynomial();
        that.ctx = dim.ctx;
        synchronized(that.ctx) {
            dim = dim.asSpace();
            that.ptr = Impl.isl.isl_qpolynomial_zero_on_domain(Impl.isl.isl_space_copy(dim.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static Qpolynomial oneOnDomain(Space dim) {
        Qpolynomial that = new Qpolynomial();
        that.ctx = dim.ctx;
        synchronized(that.ctx) {
            dim = dim.asSpace();
            that.ptr = Impl.isl.isl_qpolynomial_one_on_domain(Impl.isl.isl_space_copy(dim.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static Qpolynomial inftyOnDomain(Space dim) {
        Qpolynomial that = new Qpolynomial();
        that.ctx = dim.ctx;
        synchronized(that.ctx) {
            dim = dim.asSpace();
            that.ptr = Impl.isl.isl_qpolynomial_infty_on_domain(Impl.isl.isl_space_copy(dim.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static Qpolynomial neginftyOnDomain(Space dim) {
        Qpolynomial that = new Qpolynomial();
        that.ctx = dim.ctx;
        synchronized(that.ctx) {
            dim = dim.asSpace();
            that.ptr = Impl.isl.isl_qpolynomial_neginfty_on_domain(Impl.isl.isl_space_copy(dim.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static Qpolynomial nanOnDomain(Space dim) {
        Qpolynomial that = new Qpolynomial();
        that.ctx = dim.ctx;
        synchronized(that.ctx) {
            dim = dim.asSpace();
            that.ptr = Impl.isl.isl_qpolynomial_nan_on_domain(Impl.isl.isl_space_copy(dim.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static Qpolynomial valOnDomain(Space space, Val val) {
        Qpolynomial that = new Qpolynomial();
        that.ctx = val.ctx;
        synchronized(that.ctx) {
            space = space.asSpace();
            val = val.asVal();
            that.ptr = Impl.isl.isl_qpolynomial_val_on_domain(Impl.isl.isl_space_copy(space.getPtr()), Impl.isl.isl_val_copy(val.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static Qpolynomial varOnDomain(Space dim, DimType type, int pos) {
        Qpolynomial that = new Qpolynomial();
        that.ctx = dim.ctx;
        synchronized(that.ctx) {
            dim = dim.asSpace();
            assert pos >= 0;
            that.ptr = Impl.isl.isl_qpolynomial_var_on_domain(Impl.isl.isl_space_copy(dim.getPtr()), type.value, pos);
            Context.checkError(that.ctx);
        }
        return that;
    }
    // isl_qpolynomial_from_constraint
    public Qpolynomial(Constraint c, DimType type, int pos) {
        this.ctx = c.ctx;
        synchronized(this.ctx) {
            c = c.asConstraint();
            assert pos >= 0;
            this.ptr = Impl.isl.isl_qpolynomial_from_constraint(Impl.isl.isl_constraint_copy(c.getPtr()), type.value, pos);
            Context.checkError(this.ctx);
        }
    }
    // isl_qpolynomial_from_term
    public Qpolynomial(Term term) {
        this.ctx = term.ctx;
        synchronized(this.ctx) {
            term = term.asTerm();
            this.ptr = Impl.isl.isl_qpolynomial_from_term(Impl.isl.isl_term_copy(term.getPtr()));
            Context.checkError(this.ctx);
        }
    }
    // isl_qpolynomial_from_aff
    public Qpolynomial(Aff aff) {
        this.ctx = aff.ctx;
        synchronized(this.ctx) {
            aff = aff.asAff();
            this.ptr = Impl.isl.isl_qpolynomial_from_aff(Impl.isl.isl_aff_copy(aff.getPtr()));
            Context.checkError(this.ctx);
        }
    }
    protected void finalize() {
        synchronized(this.ctx) {
            Impl.isl.isl_qpolynomial_free(getPtr());
        }
    }
    public String toString() {
        Printer p = Printer.toStr();
        p = p.printQpolynomial(this);
        return p.getStr();
    }
    Qpolynomial asQpolynomial() {
        Class clazz = this.getClass();
        if (clazz.equals(Qpolynomial.class))
            return this;
        try {
            Constructor<Qpolynomial> c = Qpolynomial.class.getConstructor(clazz);
            return c.newInstance(this);
        } catch (Exception e) {
           throw new IllegalStateException("Cannot construct Qpolynomial from " +
               clazz.getName() + " ?", e);
        }
    }
    public Space getDomainSpace() {
        Space.Ptr res;
        synchronized(this.ctx) {
            Qpolynomial self = this.asQpolynomial();
            res = Impl.isl.isl_qpolynomial_get_domain_space(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public Space getSpace() {
        Space.Ptr res;
        synchronized(this.ctx) {
            Qpolynomial self = this.asQpolynomial();
            res = Impl.isl.isl_qpolynomial_get_space(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public int dim(DimType type) {
        int res;
        synchronized(this.ctx) {
            Qpolynomial self = this.asQpolynomial();
            res = Impl.isl.isl_qpolynomial_dim(self.getPtr(), type.value);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean involvesDims(DimType type, int first, int n) {
        boolean res;
        synchronized(this.ctx) {
            Qpolynomial self = this.asQpolynomial();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_qpolynomial_involves_dims(self.getPtr(), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Val getConstantVal() {
        Val.Ptr res;
        synchronized(this.ctx) {
            Qpolynomial self = this.asQpolynomial();
            res = Impl.isl.isl_qpolynomial_get_constant_val(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new Val(this.ctx, res);
    }
    public Qpolynomial setDimName(DimType type, int pos, String s) {
        Qpolynomial.Ptr res;
        synchronized(this.ctx) {
            Qpolynomial self = this.asQpolynomial();
            assert pos >= 0;
            res = Impl.isl.isl_qpolynomial_set_dim_name(Impl.isl.isl_qpolynomial_copy(self.getPtr()), type.value, pos, s);
            Context.checkError(this.ctx);
        }
        return new Qpolynomial(this.ctx, res);
    }
    public boolean plainIsEqual(Qpolynomial qp2) {
        boolean res;
        synchronized(this.ctx) {
            Qpolynomial self = this.asQpolynomial();
            qp2 = qp2.asQpolynomial();
            res = Impl.isl.isl_qpolynomial_plain_is_equal(self.getPtr(), qp2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isZero() {
        boolean res;
        synchronized(this.ctx) {
            Qpolynomial self = this.asQpolynomial();
            res = Impl.isl.isl_qpolynomial_is_zero(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isNan() {
        boolean res;
        synchronized(this.ctx) {
            Qpolynomial self = this.asQpolynomial();
            res = Impl.isl.isl_qpolynomial_is_nan(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isInfty() {
        boolean res;
        synchronized(this.ctx) {
            Qpolynomial self = this.asQpolynomial();
            res = Impl.isl.isl_qpolynomial_is_infty(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isNeginfty() {
        boolean res;
        synchronized(this.ctx) {
            Qpolynomial self = this.asQpolynomial();
            res = Impl.isl.isl_qpolynomial_is_neginfty(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public int sgn() {
        int res;
        synchronized(this.ctx) {
            Qpolynomial self = this.asQpolynomial();
            res = Impl.isl.isl_qpolynomial_sgn(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Qpolynomial neg() {
        Qpolynomial.Ptr res;
        synchronized(this.ctx) {
            Qpolynomial self = this.asQpolynomial();
            res = Impl.isl.isl_qpolynomial_neg(Impl.isl.isl_qpolynomial_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Qpolynomial(this.ctx, res);
    }
    public Qpolynomial add(Qpolynomial qp2) {
        Qpolynomial.Ptr res;
        synchronized(this.ctx) {
            Qpolynomial self = this.asQpolynomial();
            qp2 = qp2.asQpolynomial();
            res = Impl.isl.isl_qpolynomial_add(Impl.isl.isl_qpolynomial_copy(self.getPtr()), Impl.isl.isl_qpolynomial_copy(qp2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Qpolynomial(this.ctx, res);
    }
    public Qpolynomial sub(Qpolynomial qp2) {
        Qpolynomial.Ptr res;
        synchronized(this.ctx) {
            Qpolynomial self = this.asQpolynomial();
            qp2 = qp2.asQpolynomial();
            res = Impl.isl.isl_qpolynomial_sub(Impl.isl.isl_qpolynomial_copy(self.getPtr()), Impl.isl.isl_qpolynomial_copy(qp2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Qpolynomial(this.ctx, res);
    }
    public Qpolynomial mul(Qpolynomial qp2) {
        Qpolynomial.Ptr res;
        synchronized(this.ctx) {
            Qpolynomial self = this.asQpolynomial();
            qp2 = qp2.asQpolynomial();
            res = Impl.isl.isl_qpolynomial_mul(Impl.isl.isl_qpolynomial_copy(self.getPtr()), Impl.isl.isl_qpolynomial_copy(qp2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Qpolynomial(this.ctx, res);
    }
    public Qpolynomial pow(int power) {
        Qpolynomial.Ptr res;
        synchronized(this.ctx) {
            Qpolynomial self = this.asQpolynomial();
            assert power >= 0;
            res = Impl.isl.isl_qpolynomial_pow(Impl.isl.isl_qpolynomial_copy(self.getPtr()), power);
            Context.checkError(this.ctx);
        }
        return new Qpolynomial(this.ctx, res);
    }
    public Qpolynomial scaleVal(Val v) {
        Qpolynomial.Ptr res;
        synchronized(this.ctx) {
            Qpolynomial self = this.asQpolynomial();
            v = v.asVal();
            res = Impl.isl.isl_qpolynomial_scale_val(Impl.isl.isl_qpolynomial_copy(self.getPtr()), Impl.isl.isl_val_copy(v.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Qpolynomial(this.ctx, res);
    }
    public Qpolynomial insertDims(DimType type, int first, int n) {
        Qpolynomial.Ptr res;
        synchronized(this.ctx) {
            Qpolynomial self = this.asQpolynomial();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_qpolynomial_insert_dims(Impl.isl.isl_qpolynomial_copy(self.getPtr()), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return new Qpolynomial(this.ctx, res);
    }
    public Qpolynomial addDims(DimType type, int n) {
        Qpolynomial.Ptr res;
        synchronized(this.ctx) {
            Qpolynomial self = this.asQpolynomial();
            assert n >= 0;
            res = Impl.isl.isl_qpolynomial_add_dims(Impl.isl.isl_qpolynomial_copy(self.getPtr()), type.value, n);
            Context.checkError(this.ctx);
        }
        return new Qpolynomial(this.ctx, res);
    }
    public Qpolynomial moveDims(DimType dst_type, int dst_pos, DimType src_type, int src_pos, int n) {
        Qpolynomial.Ptr res;
        synchronized(this.ctx) {
            Qpolynomial self = this.asQpolynomial();
            assert dst_pos >= 0;
            assert src_pos >= 0;
            assert n >= 0;
            res = Impl.isl.isl_qpolynomial_move_dims(Impl.isl.isl_qpolynomial_copy(self.getPtr()), dst_type.value, dst_pos, src_type.value, src_pos, n);
            Context.checkError(this.ctx);
        }
        return new Qpolynomial(this.ctx, res);
    }
    public Qpolynomial projectDomainOnParams() {
        Qpolynomial.Ptr res;
        synchronized(this.ctx) {
            Qpolynomial self = this.asQpolynomial();
            res = Impl.isl.isl_qpolynomial_project_domain_on_params(Impl.isl.isl_qpolynomial_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Qpolynomial(this.ctx, res);
    }
    public Qpolynomial dropDims(DimType type, int first, int n) {
        Qpolynomial.Ptr res;
        synchronized(this.ctx) {
            Qpolynomial self = this.asQpolynomial();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_qpolynomial_drop_dims(Impl.isl.isl_qpolynomial_copy(self.getPtr()), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return new Qpolynomial(this.ctx, res);
    }
    public Qpolynomial substitute(DimType type, int first, int n, Qpolynomial[] subs) {
        Qpolynomial.Ptr res;
        synchronized(this.ctx) {
            Qpolynomial self = this.asQpolynomial();
            assert first >= 0;
            assert n >= 0;
            assert subs == null || subs.length == 1;
        Qpolynomial.Ptr[] _subs = subs != null ? new Qpolynomial.Ptr[1] : null;
            res = Impl.isl.isl_qpolynomial_substitute(Impl.isl.isl_qpolynomial_copy(self.getPtr()), type.value, first, n, _subs);
        if (subs != null)
            subs[0] = new Qpolynomial(this.ctx, _subs[0]);
            Context.checkError(this.ctx);
        }
        return new Qpolynomial(this.ctx, res);
    }
    public <ExceptionTy extends Exception> void asPolynomialOnDomain(BasicSet bset, final XCallback2<BasicSet,Qpolynomial,ExceptionTy> fn) throws ExceptionTy {
        int res;
        synchronized(this.ctx) {
            Qpolynomial self = this.asQpolynomial();
            bset = bset.asBasicSet();
            final Context.Ptr _ctx = this.ctx;
            final Throwable[] exc_info = new Throwable[1];
            Callback cb = new Callback() {
                public int callback(BasicSet.Ptr cb_arg0, Qpolynomial.Ptr cb_arg1, Pointer _user) {
                    try {
                    fn.apply(new BasicSet(_ctx, cb_arg0), new Qpolynomial(_ctx, cb_arg1));
                    } catch (Throwable e) {
                        exc_info[0] = e;
                        return -1;
                    }
                    return 0;
                }
            };
            res = Impl.isl.isl_qpolynomial_as_polynomial_on_domain(self.getPtr(), bset.getPtr(), cb, Pointer.NULL);
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
    public Qpolynomial homogenize() {
        Qpolynomial.Ptr res;
        synchronized(this.ctx) {
            Qpolynomial self = this.asQpolynomial();
            res = Impl.isl.isl_qpolynomial_homogenize(Impl.isl.isl_qpolynomial_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Qpolynomial(this.ctx, res);
    }
    public Qpolynomial alignParams(Space model) {
        Qpolynomial.Ptr res;
        synchronized(this.ctx) {
            Qpolynomial self = this.asQpolynomial();
            model = model.asSpace();
            res = Impl.isl.isl_qpolynomial_align_params(Impl.isl.isl_qpolynomial_copy(self.getPtr()), Impl.isl.isl_space_copy(model.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Qpolynomial(this.ctx, res);
    }
    public <ExceptionTy extends Exception> void foreachTerm(final XCallback1<Term,ExceptionTy> fn) throws ExceptionTy {
        int res;
        synchronized(this.ctx) {
            Qpolynomial self = this.asQpolynomial();
            final Context.Ptr _ctx = this.ctx;
            final Throwable[] exc_info = new Throwable[1];
            Callback cb = new Callback() {
                public int callback(Term.Ptr cb_arg0, Pointer _user) {
                    try {
                    fn.apply(new Term(_ctx, cb_arg0));
                    } catch (Throwable e) {
                        exc_info[0] = e;
                        return -1;
                    }
                    return 0;
                }
            };
            res = Impl.isl.isl_qpolynomial_foreach_term(self.getPtr(), cb, Pointer.NULL);
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
    public Val eval(Point pnt) {
        Val.Ptr res;
        synchronized(this.ctx) {
            Qpolynomial self = this.asQpolynomial();
            pnt = pnt.asPoint();
            res = Impl.isl.isl_qpolynomial_eval(Impl.isl.isl_qpolynomial_copy(self.getPtr()), Impl.isl.isl_point_copy(pnt.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Val(this.ctx, res);
    }
    public Qpolynomial gistParams(Set context) {
        Qpolynomial.Ptr res;
        synchronized(this.ctx) {
            Qpolynomial self = this.asQpolynomial();
            context = context.asSet();
            res = Impl.isl.isl_qpolynomial_gist_params(Impl.isl.isl_qpolynomial_copy(self.getPtr()), Impl.isl.isl_set_copy(context.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Qpolynomial(this.ctx, res);
    }
    public Qpolynomial gist(Set context) {
        Qpolynomial.Ptr res;
        synchronized(this.ctx) {
            Qpolynomial self = this.asQpolynomial();
            context = context.asSet();
            res = Impl.isl.isl_qpolynomial_gist(Impl.isl.isl_qpolynomial_copy(self.getPtr()), Impl.isl.isl_set_copy(context.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Qpolynomial(this.ctx, res);
    }

}
