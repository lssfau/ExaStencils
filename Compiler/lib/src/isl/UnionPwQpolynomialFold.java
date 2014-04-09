package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public class UnionPwQpolynomialFold {
    public static class Ptr extends PointerType { public Ptr() { super(); } }
    protected Context.Ptr ctx;
    protected PointerType ptr;
    UnionPwQpolynomialFold() {}
    UnionPwQpolynomialFold(Context.Ptr ctx, PointerType ptr) {
        assert !ptr.getPointer().equals(Pointer.NULL);
        this.ctx = ctx;
        this.ptr = ptr;
    }
    UnionPwQpolynomialFold.Ptr getPtr() { return (UnionPwQpolynomialFold.Ptr)this.ptr; }
    UnionPwQpolynomialFold.Ptr makePtr0() { 
        UnionPwQpolynomialFold.Ptr p = (UnionPwQpolynomialFold.Ptr)this.ptr;
        this.ptr = new UnionPwQpolynomialFold.Ptr();
        return p;
    }
    // isl_union_pw_qpolynomial_fold_from_pw_qpolynomial_fold
    public UnionPwQpolynomialFold(PwQpolynomialFold pwf) {
        this.ctx = pwf.ctx;
        synchronized(this.ctx) {
            pwf = pwf.asPwQpolynomialFold();
            this.ptr = Impl.isl.isl_union_pw_qpolynomial_fold_from_pw_qpolynomial_fold(Impl.isl.isl_pw_qpolynomial_fold_copy(pwf.getPtr()));
            Context.checkError(this.ctx);
        }
    }
    public static UnionPwQpolynomialFold zero(Space dim, Fold type) {
        UnionPwQpolynomialFold that = new UnionPwQpolynomialFold();
        that.ctx = dim.ctx;
        synchronized(that.ctx) {
            dim = dim.asSpace();
            that.ptr = Impl.isl.isl_union_pw_qpolynomial_fold_zero(Impl.isl.isl_space_copy(dim.getPtr()), type.value);
            Context.checkError(that.ctx);
        }
        return that;
    }
    protected void finalize() {
        synchronized(this.ctx) {
            Impl.isl.isl_union_pw_qpolynomial_fold_free(getPtr());
        }
    }
    public String toString() {
        Printer p = Printer.toStr();
        p = p.printUnionPwQpolynomialFold(this);
        return p.getStr();
    }
    UnionPwQpolynomialFold asUnionPwQpolynomialFold() {
        Class clazz = this.getClass();
        if (clazz.equals(UnionPwQpolynomialFold.class))
            return this;
        try {
            Constructor<UnionPwQpolynomialFold> c = UnionPwQpolynomialFold.class.getConstructor(clazz);
            return c.newInstance(this);
        } catch (Exception e) {
           throw new IllegalStateException("Cannot construct UnionPwQpolynomialFold from " +
               clazz.getName() + " ?", e);
        }
    }
    public boolean plainIsEqual(UnionPwQpolynomialFold upwf2) {
        boolean res;
        synchronized(this.ctx) {
            UnionPwQpolynomialFold self = this.asUnionPwQpolynomialFold();
            upwf2 = upwf2.asUnionPwQpolynomialFold();
            res = Impl.isl.isl_union_pw_qpolynomial_fold_plain_is_equal(self.getPtr(), upwf2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public UnionPwQpolynomialFold foldPwQpolynomialFold(PwQpolynomialFold pwqp) {
        UnionPwQpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            UnionPwQpolynomialFold self = this.asUnionPwQpolynomialFold();
            pwqp = pwqp.asPwQpolynomialFold();
            res = Impl.isl.isl_union_pw_qpolynomial_fold_fold_pw_qpolynomial_fold(Impl.isl.isl_union_pw_qpolynomial_fold_copy(self.getPtr()), Impl.isl.isl_pw_qpolynomial_fold_copy(pwqp.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionPwQpolynomialFold(this.ctx, res);
    }
    public UnionPwQpolynomialFold fold(UnionPwQpolynomialFold upwf2) {
        UnionPwQpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            UnionPwQpolynomialFold self = this.asUnionPwQpolynomialFold();
            upwf2 = upwf2.asUnionPwQpolynomialFold();
            res = Impl.isl.isl_union_pw_qpolynomial_fold_fold(Impl.isl.isl_union_pw_qpolynomial_fold_copy(self.getPtr()), Impl.isl.isl_union_pw_qpolynomial_fold_copy(upwf2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionPwQpolynomialFold(this.ctx, res);
    }
    public UnionPwQpolynomialFold addUnionPwQpolynomial(UnionPwQpolynomial upwqp) {
        UnionPwQpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            UnionPwQpolynomialFold self = this.asUnionPwQpolynomialFold();
            upwqp = upwqp.asUnionPwQpolynomial();
            res = Impl.isl.isl_union_pw_qpolynomial_fold_add_union_pw_qpolynomial(Impl.isl.isl_union_pw_qpolynomial_fold_copy(self.getPtr()), Impl.isl.isl_union_pw_qpolynomial_copy(upwqp.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionPwQpolynomialFold(this.ctx, res);
    }
    public UnionPwQpolynomialFold scaleVal(Val v) {
        UnionPwQpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            UnionPwQpolynomialFold self = this.asUnionPwQpolynomialFold();
            v = v.asVal();
            res = Impl.isl.isl_union_pw_qpolynomial_fold_scale_val(Impl.isl.isl_union_pw_qpolynomial_fold_copy(self.getPtr()), Impl.isl.isl_val_copy(v.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionPwQpolynomialFold(this.ctx, res);
    }
    public UnionSet domain() {
        UnionSet.Ptr res;
        synchronized(this.ctx) {
            UnionPwQpolynomialFold self = this.asUnionPwQpolynomialFold();
            res = Impl.isl.isl_union_pw_qpolynomial_fold_domain(Impl.isl.isl_union_pw_qpolynomial_fold_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionSet(this.ctx, res);
    }
    public UnionPwQpolynomialFold intersectDomain(UnionSet uset) {
        UnionPwQpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            UnionPwQpolynomialFold self = this.asUnionPwQpolynomialFold();
            uset = uset.asUnionSet();
            res = Impl.isl.isl_union_pw_qpolynomial_fold_intersect_domain(Impl.isl.isl_union_pw_qpolynomial_fold_copy(self.getPtr()), Impl.isl.isl_union_set_copy(uset.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionPwQpolynomialFold(this.ctx, res);
    }
    public UnionPwQpolynomialFold intersectParams(Set set) {
        UnionPwQpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            UnionPwQpolynomialFold self = this.asUnionPwQpolynomialFold();
            set = set.asSet();
            res = Impl.isl.isl_union_pw_qpolynomial_fold_intersect_params(Impl.isl.isl_union_pw_qpolynomial_fold_copy(self.getPtr()), Impl.isl.isl_set_copy(set.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionPwQpolynomialFold(this.ctx, res);
    }
    public Fold getType() {
        int res;
        synchronized(this.ctx) {
            UnionPwQpolynomialFold self = this.asUnionPwQpolynomialFold();
            res = Impl.isl.isl_union_pw_qpolynomial_fold_get_type(self.getPtr());
            Context.checkError(this.ctx);
        }
        switch(res) {
        case 2: return Fold.List;
        case 1: return Fold.Max;
        case 0: return Fold.Min;
        default: throw new IllegalStateException("No enum constant in Fold for value " + (res) + "?");
        }
    }
    public Space getSpace() {
        Space.Ptr res;
        synchronized(this.ctx) {
            UnionPwQpolynomialFold self = this.asUnionPwQpolynomialFold();
            res = Impl.isl.isl_union_pw_qpolynomial_fold_get_space(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public Val eval(Point pnt) {
        Val.Ptr res;
        synchronized(this.ctx) {
            UnionPwQpolynomialFold self = this.asUnionPwQpolynomialFold();
            pnt = pnt.asPoint();
            res = Impl.isl.isl_union_pw_qpolynomial_fold_eval(Impl.isl.isl_union_pw_qpolynomial_fold_copy(self.getPtr()), Impl.isl.isl_point_copy(pnt.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Val(this.ctx, res);
    }
    public UnionPwQpolynomialFold coalesce() {
        UnionPwQpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            UnionPwQpolynomialFold self = this.asUnionPwQpolynomialFold();
            res = Impl.isl.isl_union_pw_qpolynomial_fold_coalesce(Impl.isl.isl_union_pw_qpolynomial_fold_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionPwQpolynomialFold(this.ctx, res);
    }
    public UnionPwQpolynomialFold gist(UnionSet context) {
        UnionPwQpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            UnionPwQpolynomialFold self = this.asUnionPwQpolynomialFold();
            context = context.asUnionSet();
            res = Impl.isl.isl_union_pw_qpolynomial_fold_gist(Impl.isl.isl_union_pw_qpolynomial_fold_copy(self.getPtr()), Impl.isl.isl_union_set_copy(context.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionPwQpolynomialFold(this.ctx, res);
    }
    public UnionPwQpolynomialFold gistParams(Set context) {
        UnionPwQpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            UnionPwQpolynomialFold self = this.asUnionPwQpolynomialFold();
            context = context.asSet();
            res = Impl.isl.isl_union_pw_qpolynomial_fold_gist_params(Impl.isl.isl_union_pw_qpolynomial_fold_copy(self.getPtr()), Impl.isl.isl_set_copy(context.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionPwQpolynomialFold(this.ctx, res);
    }
    public UnionPwQpolynomialFold alignParams(Space model) {
        UnionPwQpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            UnionPwQpolynomialFold self = this.asUnionPwQpolynomialFold();
            model = model.asSpace();
            res = Impl.isl.isl_union_pw_qpolynomial_fold_align_params(Impl.isl.isl_union_pw_qpolynomial_fold_copy(self.getPtr()), Impl.isl.isl_space_copy(model.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionPwQpolynomialFold(this.ctx, res);
    }
    public <ExceptionTy extends Exception> void foreachPwQpolynomialFold(final XCallback1<PwQpolynomialFold,ExceptionTy> fn) throws ExceptionTy {
        int res;
        synchronized(this.ctx) {
            UnionPwQpolynomialFold self = this.asUnionPwQpolynomialFold();
            final Context.Ptr _ctx = this.ctx;
            final Throwable[] exc_info = new Throwable[1];
            Callback cb = new Callback() {
                public int callback(PwQpolynomialFold.Ptr cb_arg0, Pointer _user) {
                    try {
                    fn.apply(new PwQpolynomialFold(_ctx, cb_arg0));
                    } catch (Throwable e) {
                        exc_info[0] = e;
                        return -1;
                    }
                    return 0;
                }
            };
            res = Impl.isl.isl_union_pw_qpolynomial_fold_foreach_pw_qpolynomial_fold(self.getPtr(), cb, Pointer.NULL);
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
    public PwQpolynomialFold extractPwQpolynomialFold(Space dim) {
        PwQpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            UnionPwQpolynomialFold self = this.asUnionPwQpolynomialFold();
            dim = dim.asSpace();
            res = Impl.isl.isl_union_pw_qpolynomial_fold_extract_pw_qpolynomial_fold(self.getPtr(), Impl.isl.isl_space_copy(dim.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwQpolynomialFold(this.ctx, res);
    }

}
