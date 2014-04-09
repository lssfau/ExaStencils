package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public class PwQpolynomialFold {
    public static class Ptr extends PointerType { public Ptr() { super(); } }
    protected Context.Ptr ctx;
    protected PointerType ptr;
    PwQpolynomialFold() {}
    PwQpolynomialFold(Context.Ptr ctx, PointerType ptr) {
        assert !ptr.getPointer().equals(Pointer.NULL);
        this.ctx = ctx;
        this.ptr = ptr;
    }
    PwQpolynomialFold.Ptr getPtr() { return (PwQpolynomialFold.Ptr)this.ptr; }
    PwQpolynomialFold.Ptr makePtr0() { 
        PwQpolynomialFold.Ptr p = (PwQpolynomialFold.Ptr)this.ptr;
        this.ptr = new PwQpolynomialFold.Ptr();
        return p;
    }
    // isl_pw_qpolynomial_fold_from_pw_qpolynomial
    public PwQpolynomialFold(Fold type, PwQpolynomial pwqp) {
        this.ctx = pwqp.ctx;
        synchronized(this.ctx) {
            pwqp = pwqp.asPwQpolynomial();
            this.ptr = Impl.isl.isl_pw_qpolynomial_fold_from_pw_qpolynomial(type.value, Impl.isl.isl_pw_qpolynomial_copy(pwqp.getPtr()));
            Context.checkError(this.ctx);
        }
    }
    public static PwQpolynomialFold alloc(Fold type, Set set, QpolynomialFold fold) {
        PwQpolynomialFold that = new PwQpolynomialFold();
        that.ctx = fold.ctx;
        synchronized(that.ctx) {
            set = set.asSet();
            fold = fold.asQpolynomialFold();
            that.ptr = Impl.isl.isl_pw_qpolynomial_fold_alloc(type.value, Impl.isl.isl_set_copy(set.getPtr()), Impl.isl.isl_qpolynomial_fold_copy(fold.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static PwQpolynomialFold zero(Space dim, Fold type) {
        PwQpolynomialFold that = new PwQpolynomialFold();
        that.ctx = dim.ctx;
        synchronized(that.ctx) {
            dim = dim.asSpace();
            that.ptr = Impl.isl.isl_pw_qpolynomial_fold_zero(Impl.isl.isl_space_copy(dim.getPtr()), type.value);
            Context.checkError(that.ctx);
        }
        return that;
    }
    protected void finalize() {
        synchronized(this.ctx) {
            Impl.isl.isl_pw_qpolynomial_fold_free(getPtr());
        }
    }
    public String toString() {
        Printer p = Printer.toStr();
        p = p.printPwQpolynomialFold(this);
        return p.getStr();
    }
    PwQpolynomialFold asPwQpolynomialFold() {
        Class clazz = this.getClass();
        if (clazz.equals(PwQpolynomialFold.class))
            return this;
        try {
            Constructor<PwQpolynomialFold> c = PwQpolynomialFold.class.getConstructor(clazz);
            return c.newInstance(this);
        } catch (Exception e) {
           throw new IllegalStateException("Cannot construct PwQpolynomialFold from " +
               clazz.getName() + " ?", e);
        }
    }
    public boolean plainIsEqual(PwQpolynomialFold pwf2) {
        boolean res;
        synchronized(this.ctx) {
            PwQpolynomialFold self = this.asPwQpolynomialFold();
            pwf2 = pwf2.asPwQpolynomialFold();
            res = Impl.isl.isl_pw_qpolynomial_fold_plain_is_equal(self.getPtr(), pwf2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isZero() {
        boolean res;
        synchronized(this.ctx) {
            PwQpolynomialFold self = this.asPwQpolynomialFold();
            res = Impl.isl.isl_pw_qpolynomial_fold_is_zero(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Space getDomainSpace() {
        Space.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomialFold self = this.asPwQpolynomialFold();
            res = Impl.isl.isl_pw_qpolynomial_fold_get_domain_space(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public Space getSpace() {
        Space.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomialFold self = this.asPwQpolynomialFold();
            res = Impl.isl.isl_pw_qpolynomial_fold_get_space(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public PwQpolynomialFold resetSpace(Space dim) {
        PwQpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomialFold self = this.asPwQpolynomialFold();
            dim = dim.asSpace();
            res = Impl.isl.isl_pw_qpolynomial_fold_reset_space(Impl.isl.isl_pw_qpolynomial_fold_copy(self.getPtr()), Impl.isl.isl_space_copy(dim.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwQpolynomialFold(this.ctx, res);
    }
    public int dim(DimType type) {
        int res;
        synchronized(this.ctx) {
            PwQpolynomialFold self = this.asPwQpolynomialFold();
            res = Impl.isl.isl_pw_qpolynomial_fold_dim(self.getPtr(), type.value);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean hasEqualSpace(PwQpolynomialFold pwf2) {
        boolean res;
        synchronized(this.ctx) {
            PwQpolynomialFold self = this.asPwQpolynomialFold();
            pwf2 = pwf2.asPwQpolynomialFold();
            res = Impl.isl.isl_pw_qpolynomial_fold_has_equal_space(self.getPtr(), pwf2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public int size() {
        int res;
        synchronized(this.ctx) {
            PwQpolynomialFold self = this.asPwQpolynomialFold();
            res = Impl.isl.isl_pw_qpolynomial_fold_size(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public PwQpolynomialFold setDimName(DimType type, int pos, String s) {
        PwQpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomialFold self = this.asPwQpolynomialFold();
            assert pos >= 0;
            res = Impl.isl.isl_pw_qpolynomial_fold_set_dim_name(Impl.isl.isl_pw_qpolynomial_fold_copy(self.getPtr()), type.value, pos, s);
            Context.checkError(this.ctx);
        }
        return new PwQpolynomialFold(this.ctx, res);
    }
    public Set domain() {
        Set.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomialFold self = this.asPwQpolynomialFold();
            res = Impl.isl.isl_pw_qpolynomial_fold_domain(Impl.isl.isl_pw_qpolynomial_fold_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public PwQpolynomialFold intersectDomain(Set set) {
        PwQpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomialFold self = this.asPwQpolynomialFold();
            set = set.asSet();
            res = Impl.isl.isl_pw_qpolynomial_fold_intersect_domain(Impl.isl.isl_pw_qpolynomial_fold_copy(self.getPtr()), Impl.isl.isl_set_copy(set.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwQpolynomialFold(this.ctx, res);
    }
    public PwQpolynomialFold intersectParams(Set set) {
        PwQpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomialFold self = this.asPwQpolynomialFold();
            set = set.asSet();
            res = Impl.isl.isl_pw_qpolynomial_fold_intersect_params(Impl.isl.isl_pw_qpolynomial_fold_copy(self.getPtr()), Impl.isl.isl_set_copy(set.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwQpolynomialFold(this.ctx, res);
    }
    public PwQpolynomialFold add(PwQpolynomialFold pwf2) {
        PwQpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomialFold self = this.asPwQpolynomialFold();
            pwf2 = pwf2.asPwQpolynomialFold();
            res = Impl.isl.isl_pw_qpolynomial_fold_add(Impl.isl.isl_pw_qpolynomial_fold_copy(self.getPtr()), Impl.isl.isl_pw_qpolynomial_fold_copy(pwf2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwQpolynomialFold(this.ctx, res);
    }
    public PwQpolynomialFold fold(PwQpolynomialFold pwf2) {
        PwQpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomialFold self = this.asPwQpolynomialFold();
            pwf2 = pwf2.asPwQpolynomialFold();
            res = Impl.isl.isl_pw_qpolynomial_fold_fold(Impl.isl.isl_pw_qpolynomial_fold_copy(self.getPtr()), Impl.isl.isl_pw_qpolynomial_fold_copy(pwf2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwQpolynomialFold(this.ctx, res);
    }
    public PwQpolynomialFold addDisjoint(PwQpolynomialFold pwf2) {
        PwQpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomialFold self = this.asPwQpolynomialFold();
            pwf2 = pwf2.asPwQpolynomialFold();
            res = Impl.isl.isl_pw_qpolynomial_fold_add_disjoint(Impl.isl.isl_pw_qpolynomial_fold_copy(self.getPtr()), Impl.isl.isl_pw_qpolynomial_fold_copy(pwf2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwQpolynomialFold(this.ctx, res);
    }
    public PwQpolynomialFold scaleVal(Val v) {
        PwQpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomialFold self = this.asPwQpolynomialFold();
            v = v.asVal();
            res = Impl.isl.isl_pw_qpolynomial_fold_scale_val(Impl.isl.isl_pw_qpolynomial_fold_copy(self.getPtr()), Impl.isl.isl_val_copy(v.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwQpolynomialFold(this.ctx, res);
    }
    public PwQpolynomialFold projectDomainOnParams() {
        PwQpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomialFold self = this.asPwQpolynomialFold();
            res = Impl.isl.isl_pw_qpolynomial_fold_project_domain_on_params(Impl.isl.isl_pw_qpolynomial_fold_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwQpolynomialFold(this.ctx, res);
    }
    public PwQpolynomialFold dropDims(DimType type, int first, int n) {
        PwQpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomialFold self = this.asPwQpolynomialFold();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_pw_qpolynomial_fold_drop_dims(Impl.isl.isl_pw_qpolynomial_fold_copy(self.getPtr()), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return new PwQpolynomialFold(this.ctx, res);
    }
    public PwQpolynomialFold moveDims(DimType dst_type, int dst_pos, DimType src_type, int src_pos, int n) {
        PwQpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomialFold self = this.asPwQpolynomialFold();
            assert dst_pos >= 0;
            assert src_pos >= 0;
            assert n >= 0;
            res = Impl.isl.isl_pw_qpolynomial_fold_move_dims(Impl.isl.isl_pw_qpolynomial_fold_copy(self.getPtr()), dst_type.value, dst_pos, src_type.value, src_pos, n);
            Context.checkError(this.ctx);
        }
        return new PwQpolynomialFold(this.ctx, res);
    }
    public Val eval(Point pnt) {
        Val.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomialFold self = this.asPwQpolynomialFold();
            pnt = pnt.asPoint();
            res = Impl.isl.isl_pw_qpolynomial_fold_eval(Impl.isl.isl_pw_qpolynomial_fold_copy(self.getPtr()), Impl.isl.isl_point_copy(pnt.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Val(this.ctx, res);
    }
    public <ExceptionTy extends Exception> void foreachPiece(final XCallback2<Set,QpolynomialFold,ExceptionTy> fn) throws ExceptionTy {
        int res;
        synchronized(this.ctx) {
            PwQpolynomialFold self = this.asPwQpolynomialFold();
            final Context.Ptr _ctx = this.ctx;
            final Throwable[] exc_info = new Throwable[1];
            Callback cb = new Callback() {
                public int callback(Set.Ptr cb_arg0, QpolynomialFold.Ptr cb_arg1, Pointer _user) {
                    try {
                    fn.apply(new Set(_ctx, cb_arg0), new QpolynomialFold(_ctx, cb_arg1));
                    } catch (Throwable e) {
                        exc_info[0] = e;
                        return -1;
                    }
                    return 0;
                }
            };
            res = Impl.isl.isl_pw_qpolynomial_fold_foreach_piece(self.getPtr(), cb, Pointer.NULL);
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
    public <ExceptionTy extends Exception> void foreachLiftedPiece(final XCallback2<Set,QpolynomialFold,ExceptionTy> fn) throws ExceptionTy {
        int res;
        synchronized(this.ctx) {
            PwQpolynomialFold self = this.asPwQpolynomialFold();
            final Context.Ptr _ctx = this.ctx;
            final Throwable[] exc_info = new Throwable[1];
            Callback cb = new Callback() {
                public int callback(Set.Ptr cb_arg0, QpolynomialFold.Ptr cb_arg1, Pointer _user) {
                    try {
                    fn.apply(new Set(_ctx, cb_arg0), new QpolynomialFold(_ctx, cb_arg1));
                    } catch (Throwable e) {
                        exc_info[0] = e;
                        return -1;
                    }
                    return 0;
                }
            };
            res = Impl.isl.isl_pw_qpolynomial_fold_foreach_lifted_piece(self.getPtr(), cb, Pointer.NULL);
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
    public PwQpolynomialFold coalesce() {
        PwQpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomialFold self = this.asPwQpolynomialFold();
            res = Impl.isl.isl_pw_qpolynomial_fold_coalesce(Impl.isl.isl_pw_qpolynomial_fold_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwQpolynomialFold(this.ctx, res);
    }
    public PwQpolynomialFold gist(Set context) {
        PwQpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomialFold self = this.asPwQpolynomialFold();
            context = context.asSet();
            res = Impl.isl.isl_pw_qpolynomial_fold_gist(Impl.isl.isl_pw_qpolynomial_fold_copy(self.getPtr()), Impl.isl.isl_set_copy(context.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwQpolynomialFold(this.ctx, res);
    }
    public PwQpolynomialFold gistParams(Set context) {
        PwQpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomialFold self = this.asPwQpolynomialFold();
            context = context.asSet();
            res = Impl.isl.isl_pw_qpolynomial_fold_gist_params(Impl.isl.isl_pw_qpolynomial_fold_copy(self.getPtr()), Impl.isl.isl_set_copy(context.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwQpolynomialFold(this.ctx, res);
    }
    public Val max() {
        Val.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomialFold self = this.asPwQpolynomialFold();
            res = Impl.isl.isl_pw_qpolynomial_fold_max(Impl.isl.isl_pw_qpolynomial_fold_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Val(this.ctx, res);
    }
    public Val min() {
        Val.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomialFold self = this.asPwQpolynomialFold();
            res = Impl.isl.isl_pw_qpolynomial_fold_min(Impl.isl.isl_pw_qpolynomial_fold_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Val(this.ctx, res);
    }
    public PwQpolynomialFold bound(boolean[] tight) {
        PwQpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            PwQpolynomialFold self = this.asPwQpolynomialFold();
            res = Impl.isl.isl_pw_qpolynomial_fold_bound(Impl.isl.isl_pw_qpolynomial_fold_copy(self.getPtr()), tight);
            Context.checkError(this.ctx);
        }
        return new PwQpolynomialFold(this.ctx, res);
    }

}
