package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public class UnionPwQpolynomial {
    public static class Ptr extends PointerType { public Ptr() { super(); } }
    protected Context.Ptr ctx;
    protected PointerType ptr;
    UnionPwQpolynomial() {}
    UnionPwQpolynomial(Context.Ptr ctx, PointerType ptr) {
        assert !ptr.getPointer().equals(Pointer.NULL);
        this.ctx = ctx;
        this.ptr = ptr;
    }
    UnionPwQpolynomial.Ptr getPtr() { return (UnionPwQpolynomial.Ptr)this.ptr; }
    UnionPwQpolynomial.Ptr makePtr0() { 
        UnionPwQpolynomial.Ptr p = (UnionPwQpolynomial.Ptr)this.ptr;
        this.ptr = new UnionPwQpolynomial.Ptr();
        return p;
    }
    // isl_union_pw_qpolynomial_from_pw_qpolynomial
    public UnionPwQpolynomial(PwQpolynomial pwqp) {
        this.ctx = pwqp.ctx;
        synchronized(this.ctx) {
            pwqp = pwqp.asPwQpolynomial();
            this.ptr = Impl.isl.isl_union_pw_qpolynomial_from_pw_qpolynomial(Impl.isl.isl_pw_qpolynomial_copy(pwqp.getPtr()));
            Context.checkError(this.ctx);
        }
    }
    public static UnionPwQpolynomial zero(Space dim) {
        UnionPwQpolynomial that = new UnionPwQpolynomial();
        that.ctx = dim.ctx;
        synchronized(that.ctx) {
            dim = dim.asSpace();
            that.ptr = Impl.isl.isl_union_pw_qpolynomial_zero(Impl.isl.isl_space_copy(dim.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    // isl_union_pw_qpolynomial_read_from_str
    public UnionPwQpolynomial(String str) {
        this.ctx = Context.getDefaultInstance();
        synchronized(this.ctx) {
            this.ptr = Impl.isl.isl_union_pw_qpolynomial_read_from_str(this.ctx, str);
            Context.checkError(this.ctx);
        }
    }
    protected void finalize() {
        synchronized(this.ctx) {
            Impl.isl.isl_union_pw_qpolynomial_free(getPtr());
        }
    }
    public String toString() {
        Printer p = Printer.toStr();
        p = p.printUnionPwQpolynomial(this);
        return p.getStr();
    }
    UnionPwQpolynomial asUnionPwQpolynomial() {
        Class clazz = this.getClass();
        if (clazz.equals(UnionPwQpolynomial.class))
            return this;
        try {
            Constructor<UnionPwQpolynomial> c = UnionPwQpolynomial.class.getConstructor(clazz);
            return c.newInstance(this);
        } catch (Exception e) {
           throw new IllegalStateException("Cannot construct UnionPwQpolynomial from " +
               clazz.getName() + " ?", e);
        }
    }
    public boolean plainIsEqual(UnionPwQpolynomial upwqp2) {
        boolean res;
        synchronized(this.ctx) {
            UnionPwQpolynomial self = this.asUnionPwQpolynomial();
            upwqp2 = upwqp2.asUnionPwQpolynomial();
            res = Impl.isl.isl_union_pw_qpolynomial_plain_is_equal(self.getPtr(), upwqp2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public UnionPwQpolynomial addPwQpolynomial(PwQpolynomial pwqp) {
        UnionPwQpolynomial.Ptr res;
        synchronized(this.ctx) {
            UnionPwQpolynomial self = this.asUnionPwQpolynomial();
            pwqp = pwqp.asPwQpolynomial();
            res = Impl.isl.isl_union_pw_qpolynomial_add_pw_qpolynomial(Impl.isl.isl_union_pw_qpolynomial_copy(self.getPtr()), Impl.isl.isl_pw_qpolynomial_copy(pwqp.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionPwQpolynomial(this.ctx, res);
    }
    public UnionPwQpolynomial add(UnionPwQpolynomial upwqp2) {
        UnionPwQpolynomial.Ptr res;
        synchronized(this.ctx) {
            UnionPwQpolynomial self = this.asUnionPwQpolynomial();
            upwqp2 = upwqp2.asUnionPwQpolynomial();
            res = Impl.isl.isl_union_pw_qpolynomial_add(Impl.isl.isl_union_pw_qpolynomial_copy(self.getPtr()), Impl.isl.isl_union_pw_qpolynomial_copy(upwqp2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionPwQpolynomial(this.ctx, res);
    }
    public UnionPwQpolynomial sub(UnionPwQpolynomial upwqp2) {
        UnionPwQpolynomial.Ptr res;
        synchronized(this.ctx) {
            UnionPwQpolynomial self = this.asUnionPwQpolynomial();
            upwqp2 = upwqp2.asUnionPwQpolynomial();
            res = Impl.isl.isl_union_pw_qpolynomial_sub(Impl.isl.isl_union_pw_qpolynomial_copy(self.getPtr()), Impl.isl.isl_union_pw_qpolynomial_copy(upwqp2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionPwQpolynomial(this.ctx, res);
    }
    public UnionPwQpolynomial mul(UnionPwQpolynomial upwqp2) {
        UnionPwQpolynomial.Ptr res;
        synchronized(this.ctx) {
            UnionPwQpolynomial self = this.asUnionPwQpolynomial();
            upwqp2 = upwqp2.asUnionPwQpolynomial();
            res = Impl.isl.isl_union_pw_qpolynomial_mul(Impl.isl.isl_union_pw_qpolynomial_copy(self.getPtr()), Impl.isl.isl_union_pw_qpolynomial_copy(upwqp2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionPwQpolynomial(this.ctx, res);
    }
    public UnionPwQpolynomial scaleVal(Val v) {
        UnionPwQpolynomial.Ptr res;
        synchronized(this.ctx) {
            UnionPwQpolynomial self = this.asUnionPwQpolynomial();
            v = v.asVal();
            res = Impl.isl.isl_union_pw_qpolynomial_scale_val(Impl.isl.isl_union_pw_qpolynomial_copy(self.getPtr()), Impl.isl.isl_val_copy(v.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionPwQpolynomial(this.ctx, res);
    }
    public UnionSet domain() {
        UnionSet.Ptr res;
        synchronized(this.ctx) {
            UnionPwQpolynomial self = this.asUnionPwQpolynomial();
            res = Impl.isl.isl_union_pw_qpolynomial_domain(Impl.isl.isl_union_pw_qpolynomial_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionSet(this.ctx, res);
    }
    public UnionPwQpolynomial intersectDomain(UnionSet uset) {
        UnionPwQpolynomial.Ptr res;
        synchronized(this.ctx) {
            UnionPwQpolynomial self = this.asUnionPwQpolynomial();
            uset = uset.asUnionSet();
            res = Impl.isl.isl_union_pw_qpolynomial_intersect_domain(Impl.isl.isl_union_pw_qpolynomial_copy(self.getPtr()), Impl.isl.isl_union_set_copy(uset.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionPwQpolynomial(this.ctx, res);
    }
    public UnionPwQpolynomial intersectParams(Set set) {
        UnionPwQpolynomial.Ptr res;
        synchronized(this.ctx) {
            UnionPwQpolynomial self = this.asUnionPwQpolynomial();
            set = set.asSet();
            res = Impl.isl.isl_union_pw_qpolynomial_intersect_params(Impl.isl.isl_union_pw_qpolynomial_copy(self.getPtr()), Impl.isl.isl_set_copy(set.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionPwQpolynomial(this.ctx, res);
    }
    public Space getSpace() {
        Space.Ptr res;
        synchronized(this.ctx) {
            UnionPwQpolynomial self = this.asUnionPwQpolynomial();
            res = Impl.isl.isl_union_pw_qpolynomial_get_space(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public Val eval(Point pnt) {
        Val.Ptr res;
        synchronized(this.ctx) {
            UnionPwQpolynomial self = this.asUnionPwQpolynomial();
            pnt = pnt.asPoint();
            res = Impl.isl.isl_union_pw_qpolynomial_eval(Impl.isl.isl_union_pw_qpolynomial_copy(self.getPtr()), Impl.isl.isl_point_copy(pnt.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Val(this.ctx, res);
    }
    public UnionPwQpolynomial coalesce() {
        UnionPwQpolynomial.Ptr res;
        synchronized(this.ctx) {
            UnionPwQpolynomial self = this.asUnionPwQpolynomial();
            res = Impl.isl.isl_union_pw_qpolynomial_coalesce(Impl.isl.isl_union_pw_qpolynomial_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionPwQpolynomial(this.ctx, res);
    }
    public UnionPwQpolynomial gist(UnionSet context) {
        UnionPwQpolynomial.Ptr res;
        synchronized(this.ctx) {
            UnionPwQpolynomial self = this.asUnionPwQpolynomial();
            context = context.asUnionSet();
            res = Impl.isl.isl_union_pw_qpolynomial_gist(Impl.isl.isl_union_pw_qpolynomial_copy(self.getPtr()), Impl.isl.isl_union_set_copy(context.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionPwQpolynomial(this.ctx, res);
    }
    public UnionPwQpolynomial gistParams(Set context) {
        UnionPwQpolynomial.Ptr res;
        synchronized(this.ctx) {
            UnionPwQpolynomial self = this.asUnionPwQpolynomial();
            context = context.asSet();
            res = Impl.isl.isl_union_pw_qpolynomial_gist_params(Impl.isl.isl_union_pw_qpolynomial_copy(self.getPtr()), Impl.isl.isl_set_copy(context.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionPwQpolynomial(this.ctx, res);
    }
    public UnionPwQpolynomial alignParams(Space model) {
        UnionPwQpolynomial.Ptr res;
        synchronized(this.ctx) {
            UnionPwQpolynomial self = this.asUnionPwQpolynomial();
            model = model.asSpace();
            res = Impl.isl.isl_union_pw_qpolynomial_align_params(Impl.isl.isl_union_pw_qpolynomial_copy(self.getPtr()), Impl.isl.isl_space_copy(model.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionPwQpolynomial(this.ctx, res);
    }
    public <ExceptionTy extends Exception> void foreachPwQpolynomial(final XCallback1<PwQpolynomial,ExceptionTy> fn) throws ExceptionTy {
        int res;
        synchronized(this.ctx) {
            UnionPwQpolynomial self = this.asUnionPwQpolynomial();
            final Context.Ptr _ctx = this.ctx;
            final Throwable[] exc_info = new Throwable[1];
            Callback cb = new Callback() {
                public int callback(PwQpolynomial.Ptr cb_arg0, Pointer _user) {
                    try {
                    fn.apply(new PwQpolynomial(_ctx, cb_arg0));
                    } catch (Throwable e) {
                        exc_info[0] = e;
                        return -1;
                    }
                    return 0;
                }
            };
            res = Impl.isl.isl_union_pw_qpolynomial_foreach_pw_qpolynomial(self.getPtr(), cb, Pointer.NULL);
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
    public PwQpolynomial extractPwQpolynomial(Space dim) {
        PwQpolynomial.Ptr res;
        synchronized(this.ctx) {
            UnionPwQpolynomial self = this.asUnionPwQpolynomial();
            dim = dim.asSpace();
            res = Impl.isl.isl_union_pw_qpolynomial_extract_pw_qpolynomial(self.getPtr(), Impl.isl.isl_space_copy(dim.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwQpolynomial(this.ctx, res);
    }
    public UnionPwQpolynomialFold bound(Fold type, boolean[] tight) {
        UnionPwQpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            UnionPwQpolynomial self = this.asUnionPwQpolynomial();
            res = Impl.isl.isl_union_pw_qpolynomial_bound(Impl.isl.isl_union_pw_qpolynomial_copy(self.getPtr()), type.value, tight);
            Context.checkError(this.ctx);
        }
        return new UnionPwQpolynomialFold(this.ctx, res);
    }
    public UnionPwQpolynomial toPolynomial(int sign) {
        UnionPwQpolynomial.Ptr res;
        synchronized(this.ctx) {
            UnionPwQpolynomial self = this.asUnionPwQpolynomial();
            res = Impl.isl.isl_union_pw_qpolynomial_to_polynomial(Impl.isl.isl_union_pw_qpolynomial_copy(self.getPtr()), sign);
            Context.checkError(this.ctx);
        }
        return new UnionPwQpolynomial(this.ctx, res);
    }

}
