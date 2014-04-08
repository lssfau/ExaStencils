package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public class UnionSet {
    public static class Ptr extends PointerType { public Ptr() { super(); } }
    protected Context.Ptr ctx;
    protected PointerType ptr;
    UnionSet() {}
    UnionSet(Context.Ptr ctx, PointerType ptr) {
        assert !ptr.getPointer().equals(Pointer.NULL);
        this.ctx = ctx;
        this.ptr = ptr;
    }
    UnionSet.Ptr getPtr() { return (UnionSet.Ptr)this.ptr; }
    UnionSet.Ptr makePtr0() { 
        UnionSet.Ptr p = (UnionSet.Ptr)this.ptr;
        this.ptr = new UnionSet.Ptr();
        return p;
    }
    // isl_union_set_from_basic_set
    public UnionSet(BasicSet bset) {
        this.ctx = bset.ctx;
        synchronized(this.ctx) {
            bset = bset.asBasicSet();
            this.ptr = Impl.isl.isl_union_set_from_basic_set(Impl.isl.isl_basic_set_copy(bset.getPtr()));
            Context.checkError(this.ctx);
        }
    }
    // isl_union_set_from_set
    public UnionSet(Set set) {
        this.ctx = set.ctx;
        synchronized(this.ctx) {
            set = set.asSet();
            this.ptr = Impl.isl.isl_union_set_from_set(Impl.isl.isl_set_copy(set.getPtr()));
            Context.checkError(this.ctx);
        }
    }
    public static UnionSet empty(Space dim) {
        UnionSet that = new UnionSet();
        that.ctx = dim.ctx;
        synchronized(that.ctx) {
            dim = dim.asSpace();
            that.ptr = Impl.isl.isl_union_set_empty(Impl.isl.isl_space_copy(dim.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    // isl_union_set_read_from_str
    public UnionSet(String str) {
        this.ctx = Context.getDefaultInstance();
        synchronized(this.ctx) {
            this.ptr = Impl.isl.isl_union_set_read_from_str(this.ctx, str);
            Context.checkError(this.ctx);
        }
    }
    protected void finalize() {
        synchronized(this.ctx) {
            Impl.isl.isl_union_set_free(getPtr());
        }
    }
    public String toString() {
        Printer p = Printer.toStr();
        p = p.printUnionSet(this);
        return p.getStr();
    }
    UnionSet asUnionSet() {
        Class clazz = this.getClass();
        if (clazz.equals(UnionSet.class))
            return this;
        try {
            Constructor<UnionSet> c = UnionSet.class.getConstructor(clazz);
            return c.newInstance(this);
        } catch (Exception e) {
           throw new IllegalStateException("Cannot construct UnionSet from " +
               clazz.getName() + " ?", e);
        }
    }
    public UnionMap identity() {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            res = Impl.isl.isl_union_set_identity(Impl.isl.isl_union_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap unwrap() {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            res = Impl.isl.isl_union_set_unwrap(Impl.isl.isl_union_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionSet alignParams(Space model) {
        UnionSet.Ptr res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            model = model.asSpace();
            res = Impl.isl.isl_union_set_align_params(Impl.isl.isl_union_set_copy(self.getPtr()), Impl.isl.isl_space_copy(model.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionSet(this.ctx, res);
    }
    public Space getSpace() {
        Space.Ptr res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            res = Impl.isl.isl_union_set_get_space(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public UnionSet universe() {
        UnionSet.Ptr res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            res = Impl.isl.isl_union_set_universe(Impl.isl.isl_union_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionSet(this.ctx, res);
    }
    public Set params() {
        Set.Ptr res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            res = Impl.isl.isl_union_set_params(Impl.isl.isl_union_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public UnionSet detectEqualities() {
        UnionSet.Ptr res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            res = Impl.isl.isl_union_set_detect_equalities(Impl.isl.isl_union_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionSet(this.ctx, res);
    }
    public UnionSet affineHull() {
        UnionSet.Ptr res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            res = Impl.isl.isl_union_set_affine_hull(Impl.isl.isl_union_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionSet(this.ctx, res);
    }
    public UnionSet polyhedralHull() {
        UnionSet.Ptr res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            res = Impl.isl.isl_union_set_polyhedral_hull(Impl.isl.isl_union_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionSet(this.ctx, res);
    }
    public UnionSet coalesce() {
        UnionSet.Ptr res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            res = Impl.isl.isl_union_set_coalesce(Impl.isl.isl_union_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionSet(this.ctx, res);
    }
    public UnionSet lexmin() {
        UnionSet.Ptr res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            res = Impl.isl.isl_union_set_lexmin(Impl.isl.isl_union_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionSet(this.ctx, res);
    }
    public UnionSet lexmax() {
        UnionSet.Ptr res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            res = Impl.isl.isl_union_set_lexmax(Impl.isl.isl_union_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionSet(this.ctx, res);
    }
    public UnionSet addSet(Set set) {
        UnionSet.Ptr res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            set = set.asSet();
            res = Impl.isl.isl_union_set_add_set(Impl.isl.isl_union_set_copy(self.getPtr()), Impl.isl.isl_set_copy(set.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionSet(this.ctx, res);
    }
    public UnionSet union(UnionSet uset2) {
        UnionSet.Ptr res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            uset2 = uset2.asUnionSet();
            res = Impl.isl.isl_union_set_union(Impl.isl.isl_union_set_copy(self.getPtr()), Impl.isl.isl_union_set_copy(uset2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionSet(this.ctx, res);
    }
    public UnionSet subtract(UnionSet uset2) {
        UnionSet.Ptr res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            uset2 = uset2.asUnionSet();
            res = Impl.isl.isl_union_set_subtract(Impl.isl.isl_union_set_copy(self.getPtr()), Impl.isl.isl_union_set_copy(uset2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionSet(this.ctx, res);
    }
    public UnionSet intersect(UnionSet uset2) {
        UnionSet.Ptr res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            uset2 = uset2.asUnionSet();
            res = Impl.isl.isl_union_set_intersect(Impl.isl.isl_union_set_copy(self.getPtr()), Impl.isl.isl_union_set_copy(uset2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionSet(this.ctx, res);
    }
    public UnionSet intersectParams(Set set) {
        UnionSet.Ptr res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            set = set.asSet();
            res = Impl.isl.isl_union_set_intersect_params(Impl.isl.isl_union_set_copy(self.getPtr()), Impl.isl.isl_set_copy(set.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionSet(this.ctx, res);
    }
    public UnionSet gist(UnionSet context) {
        UnionSet.Ptr res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            context = context.asUnionSet();
            res = Impl.isl.isl_union_set_gist(Impl.isl.isl_union_set_copy(self.getPtr()), Impl.isl.isl_union_set_copy(context.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionSet(this.ctx, res);
    }
    public UnionSet gistParams(Set set) {
        UnionSet.Ptr res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            set = set.asSet();
            res = Impl.isl.isl_union_set_gist_params(Impl.isl.isl_union_set_copy(self.getPtr()), Impl.isl.isl_set_copy(set.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionSet(this.ctx, res);
    }
    public UnionSet apply(UnionMap umap) {
        UnionSet.Ptr res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            umap = umap.asUnionMap();
            res = Impl.isl.isl_union_set_apply(Impl.isl.isl_union_set_copy(self.getPtr()), Impl.isl.isl_union_map_copy(umap.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionSet(this.ctx, res);
    }
    public boolean isParams() {
        boolean res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            res = Impl.isl.isl_union_set_is_params(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isEmpty() {
        boolean res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            res = Impl.isl.isl_union_set_is_empty(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isSubset(UnionSet uset2) {
        boolean res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            uset2 = uset2.asUnionSet();
            res = Impl.isl.isl_union_set_is_subset(self.getPtr(), uset2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isEqual(UnionSet uset2) {
        boolean res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            uset2 = uset2.asUnionSet();
            res = Impl.isl.isl_union_set_is_equal(self.getPtr(), uset2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isStrictSubset(UnionSet uset2) {
        boolean res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            uset2 = uset2.asUnionSet();
            res = Impl.isl.isl_union_set_is_strict_subset(self.getPtr(), uset2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public int nSet() {
        int res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            res = Impl.isl.isl_union_set_n_set(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public <ExceptionTy extends Exception> void foreachSet(final XCallback1<Set,ExceptionTy> fn) throws ExceptionTy {
        int res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            final Context.Ptr _ctx = this.ctx;
            final Throwable[] exc_info = new Throwable[1];
            Callback cb = new Callback() {
                public int callback(Set.Ptr cb_arg0, Pointer _user) {
                    try {
                    fn.apply(new Set(_ctx, cb_arg0));
                    } catch (Throwable e) {
                        exc_info[0] = e;
                        return -1;
                    }
                    return 0;
                }
            };
            res = Impl.isl.isl_union_set_foreach_set(self.getPtr(), cb, Pointer.NULL);
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
    public int contains(Space dim) {
        int res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            dim = dim.asSpace();
            res = Impl.isl.isl_union_set_contains(self.getPtr(), dim.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Set extractSet(Space dim) {
        Set.Ptr res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            dim = dim.asSpace();
            res = Impl.isl.isl_union_set_extract_set(self.getPtr(), Impl.isl.isl_space_copy(dim.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public <ExceptionTy extends Exception> void foreachPoint(final XCallback1<Point,ExceptionTy> fn) throws ExceptionTy {
        int res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            final Context.Ptr _ctx = this.ctx;
            final Throwable[] exc_info = new Throwable[1];
            Callback cb = new Callback() {
                public int callback(Point.Ptr cb_arg0, Pointer _user) {
                    try {
                    fn.apply(new Point(_ctx, cb_arg0));
                    } catch (Throwable e) {
                        exc_info[0] = e;
                        return -1;
                    }
                    return 0;
                }
            };
            res = Impl.isl.isl_union_set_foreach_point(self.getPtr(), cb, Pointer.NULL);
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
    public BasicSet sample() {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            res = Impl.isl.isl_union_set_sample(Impl.isl.isl_union_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public UnionSet lift() {
        UnionSet.Ptr res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            res = Impl.isl.isl_union_set_lift(Impl.isl.isl_union_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionSet(this.ctx, res);
    }
    public UnionMap lexLtUnionSet(UnionSet uset2) {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            uset2 = uset2.asUnionSet();
            res = Impl.isl.isl_union_set_lex_lt_union_set(Impl.isl.isl_union_set_copy(self.getPtr()), Impl.isl.isl_union_set_copy(uset2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap lexLeUnionSet(UnionSet uset2) {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            uset2 = uset2.asUnionSet();
            res = Impl.isl.isl_union_set_lex_le_union_set(Impl.isl.isl_union_set_copy(self.getPtr()), Impl.isl.isl_union_set_copy(uset2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap lexGtUnionSet(UnionSet uset2) {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            uset2 = uset2.asUnionSet();
            res = Impl.isl.isl_union_set_lex_gt_union_set(Impl.isl.isl_union_set_copy(self.getPtr()), Impl.isl.isl_union_set_copy(uset2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap lexGeUnionSet(UnionSet uset2) {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            uset2 = uset2.asUnionSet();
            res = Impl.isl.isl_union_set_lex_ge_union_set(Impl.isl.isl_union_set_copy(self.getPtr()), Impl.isl.isl_union_set_copy(uset2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionSet coefficients() {
        UnionSet.Ptr res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            res = Impl.isl.isl_union_set_coefficients(Impl.isl.isl_union_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionSet(this.ctx, res);
    }
    public UnionSet solutions() {
        UnionSet.Ptr res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            res = Impl.isl.isl_union_set_solutions(Impl.isl.isl_union_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionSet(this.ctx, res);
    }
    public Schedule computeSchedule(UnionMap validity, UnionMap proximity) {
        Schedule.Ptr res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            validity = validity.asUnionMap();
            proximity = proximity.asUnionMap();
            res = Impl.isl.isl_union_set_compute_schedule(Impl.isl.isl_union_set_copy(self.getPtr()), Impl.isl.isl_union_map_copy(validity.getPtr()), Impl.isl.isl_union_map_copy(proximity.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Schedule(this.ctx, res);
    }
    public UnionPwQpolynomialFold applyUnionPwQpolynomialFold(UnionPwQpolynomialFold upwf, boolean[] tight) {
        UnionPwQpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            UnionSet self = this.asUnionSet();
            upwf = upwf.asUnionPwQpolynomialFold();
            res = Impl.isl.isl_union_set_apply_union_pw_qpolynomial_fold(Impl.isl.isl_union_set_copy(self.getPtr()), Impl.isl.isl_union_pw_qpolynomial_fold_copy(upwf.getPtr()), tight);
            Context.checkError(this.ctx);
        }
        return new UnionPwQpolynomialFold(this.ctx, res);
    }

}
