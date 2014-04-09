package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public class UnionMap {
    public static class Ptr extends PointerType { public Ptr() { super(); } }
    protected Context.Ptr ctx;
    protected PointerType ptr;
    UnionMap() {}
    UnionMap(Context.Ptr ctx, PointerType ptr) {
        assert !ptr.getPointer().equals(Pointer.NULL);
        this.ctx = ctx;
        this.ptr = ptr;
    }
    UnionMap.Ptr getPtr() { return (UnionMap.Ptr)this.ptr; }
    UnionMap.Ptr makePtr0() { 
        UnionMap.Ptr p = (UnionMap.Ptr)this.ptr;
        this.ptr = new UnionMap.Ptr();
        return p;
    }
    // isl_union_map_from_basic_map
    public UnionMap(BasicMap bmap) {
        this.ctx = bmap.ctx;
        synchronized(this.ctx) {
            bmap = bmap.asBasicMap();
            this.ptr = Impl.isl.isl_union_map_from_basic_map(Impl.isl.isl_basic_map_copy(bmap.getPtr()));
            Context.checkError(this.ctx);
        }
    }
    // isl_union_map_from_map
    public UnionMap(Map map) {
        this.ctx = map.ctx;
        synchronized(this.ctx) {
            map = map.asMap();
            this.ptr = Impl.isl.isl_union_map_from_map(Impl.isl.isl_map_copy(map.getPtr()));
            Context.checkError(this.ctx);
        }
    }
    public static UnionMap universe(UnionMap umap) {
        UnionMap that = new UnionMap();
        that.ctx = umap.ctx;
        synchronized(that.ctx) {
            umap = umap.asUnionMap();
            that.ptr = Impl.isl.isl_union_map_universe(Impl.isl.isl_union_map_copy(umap.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static UnionMap fromDomain(UnionSet uset) {
        UnionMap that = new UnionMap();
        that.ctx = uset.ctx;
        synchronized(that.ctx) {
            uset = uset.asUnionSet();
            that.ptr = Impl.isl.isl_union_map_from_domain(Impl.isl.isl_union_set_copy(uset.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static UnionMap fromRange(UnionSet uset) {
        UnionMap that = new UnionMap();
        that.ctx = uset.ctx;
        synchronized(that.ctx) {
            uset = uset.asUnionSet();
            that.ptr = Impl.isl.isl_union_map_from_range(Impl.isl.isl_union_set_copy(uset.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    // isl_union_map_read_from_str
    public UnionMap(String str) {
        this.ctx = Context.getDefaultInstance();
        synchronized(this.ctx) {
            this.ptr = Impl.isl.isl_union_map_read_from_str(this.ctx, str);
            Context.checkError(this.ctx);
        }
    }
    protected void finalize() {
        synchronized(this.ctx) {
            Impl.isl.isl_union_map_free(getPtr());
        }
    }
    public String toString() {
        Printer p = Printer.toStr();
        p = p.printUnionMap(this);
        return p.getStr();
    }
    UnionMap asUnionMap() {
        Class clazz = this.getClass();
        if (clazz.equals(UnionMap.class))
            return this;
        try {
            Constructor<UnionMap> c = UnionMap.class.getConstructor(clazz);
            return c.newInstance(this);
        } catch (Exception e) {
           throw new IllegalStateException("Cannot construct UnionMap from " +
               clazz.getName() + " ?", e);
        }
    }
    public Set params() {
        Set.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            res = Impl.isl.isl_union_map_params(Impl.isl.isl_union_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public UnionSet domain() {
        UnionSet.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            res = Impl.isl.isl_union_map_domain(Impl.isl.isl_union_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionSet(this.ctx, res);
    }
    public UnionSet range() {
        UnionSet.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            res = Impl.isl.isl_union_map_range(Impl.isl.isl_union_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionSet(this.ctx, res);
    }
    public UnionMap domainMap() {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            res = Impl.isl.isl_union_map_domain_map(Impl.isl.isl_union_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap rangeMap() {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            res = Impl.isl.isl_union_map_range_map(Impl.isl.isl_union_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap affineHull() {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            res = Impl.isl.isl_union_map_affine_hull(Impl.isl.isl_union_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap polyhedralHull() {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            res = Impl.isl.isl_union_map_polyhedral_hull(Impl.isl.isl_union_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap coalesce() {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            res = Impl.isl.isl_union_map_coalesce(Impl.isl.isl_union_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap computeDivs() {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            res = Impl.isl.isl_union_map_compute_divs(Impl.isl.isl_union_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap lexmin() {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            res = Impl.isl.isl_union_map_lexmin(Impl.isl.isl_union_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap lexmax() {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            res = Impl.isl.isl_union_map_lexmax(Impl.isl.isl_union_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap addMap(Map map) {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            map = map.asMap();
            res = Impl.isl.isl_union_map_add_map(Impl.isl.isl_union_map_copy(self.getPtr()), Impl.isl.isl_map_copy(map.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap union(UnionMap umap2) {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            umap2 = umap2.asUnionMap();
            res = Impl.isl.isl_union_map_union(Impl.isl.isl_union_map_copy(self.getPtr()), Impl.isl.isl_union_map_copy(umap2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap subtract(UnionMap umap2) {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            umap2 = umap2.asUnionMap();
            res = Impl.isl.isl_union_map_subtract(Impl.isl.isl_union_map_copy(self.getPtr()), Impl.isl.isl_union_map_copy(umap2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap intersect(UnionMap umap2) {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            umap2 = umap2.asUnionMap();
            res = Impl.isl.isl_union_map_intersect(Impl.isl.isl_union_map_copy(self.getPtr()), Impl.isl.isl_union_map_copy(umap2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap intersectParams(Set set) {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            set = set.asSet();
            res = Impl.isl.isl_union_map_intersect_params(Impl.isl.isl_union_map_copy(self.getPtr()), Impl.isl.isl_set_copy(set.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap gist(UnionMap context) {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            context = context.asUnionMap();
            res = Impl.isl.isl_union_map_gist(Impl.isl.isl_union_map_copy(self.getPtr()), Impl.isl.isl_union_map_copy(context.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap gistParams(Set set) {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            set = set.asSet();
            res = Impl.isl.isl_union_map_gist_params(Impl.isl.isl_union_map_copy(self.getPtr()), Impl.isl.isl_set_copy(set.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap gistDomain(UnionSet uset) {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            uset = uset.asUnionSet();
            res = Impl.isl.isl_union_map_gist_domain(Impl.isl.isl_union_map_copy(self.getPtr()), Impl.isl.isl_union_set_copy(uset.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap gistRange(UnionSet uset) {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            uset = uset.asUnionSet();
            res = Impl.isl.isl_union_map_gist_range(Impl.isl.isl_union_map_copy(self.getPtr()), Impl.isl.isl_union_set_copy(uset.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap intersectDomain(UnionSet uset) {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            uset = uset.asUnionSet();
            res = Impl.isl.isl_union_map_intersect_domain(Impl.isl.isl_union_map_copy(self.getPtr()), Impl.isl.isl_union_set_copy(uset.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap intersectRange(UnionSet uset) {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            uset = uset.asUnionSet();
            res = Impl.isl.isl_union_map_intersect_range(Impl.isl.isl_union_map_copy(self.getPtr()), Impl.isl.isl_union_set_copy(uset.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap subtractDomain(UnionSet dom) {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            dom = dom.asUnionSet();
            res = Impl.isl.isl_union_map_subtract_domain(Impl.isl.isl_union_map_copy(self.getPtr()), Impl.isl.isl_union_set_copy(dom.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap subtractRange(UnionSet dom) {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            dom = dom.asUnionSet();
            res = Impl.isl.isl_union_map_subtract_range(Impl.isl.isl_union_map_copy(self.getPtr()), Impl.isl.isl_union_set_copy(dom.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap applyDomain(UnionMap umap2) {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            umap2 = umap2.asUnionMap();
            res = Impl.isl.isl_union_map_apply_domain(Impl.isl.isl_union_map_copy(self.getPtr()), Impl.isl.isl_union_map_copy(umap2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap applyRange(UnionMap umap2) {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            umap2 = umap2.asUnionMap();
            res = Impl.isl.isl_union_map_apply_range(Impl.isl.isl_union_map_copy(self.getPtr()), Impl.isl.isl_union_map_copy(umap2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap reverse() {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            res = Impl.isl.isl_union_map_reverse(Impl.isl.isl_union_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap detectEqualities() {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            res = Impl.isl.isl_union_map_detect_equalities(Impl.isl.isl_union_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionSet deltas() {
        UnionSet.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            res = Impl.isl.isl_union_map_deltas(Impl.isl.isl_union_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionSet(this.ctx, res);
    }
    public boolean isEmpty() {
        boolean res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            res = Impl.isl.isl_union_map_is_empty(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isSingleValued() {
        boolean res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            res = Impl.isl.isl_union_map_is_single_valued(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isInjective() {
        boolean res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            res = Impl.isl.isl_union_map_is_injective(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isBijective() {
        boolean res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            res = Impl.isl.isl_union_map_is_bijective(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isSubset(UnionMap umap2) {
        boolean res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            umap2 = umap2.asUnionMap();
            res = Impl.isl.isl_union_map_is_subset(self.getPtr(), umap2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isEqual(UnionMap umap2) {
        boolean res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            umap2 = umap2.asUnionMap();
            res = Impl.isl.isl_union_map_is_equal(self.getPtr(), umap2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isStrictSubset(UnionMap umap2) {
        boolean res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            umap2 = umap2.asUnionMap();
            res = Impl.isl.isl_union_map_is_strict_subset(self.getPtr(), umap2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public int nMap() {
        int res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            res = Impl.isl.isl_union_map_n_map(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public <ExceptionTy extends Exception> void foreachMap(final XCallback1<Map,ExceptionTy> fn) throws ExceptionTy {
        int res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            final Context.Ptr _ctx = this.ctx;
            final Throwable[] exc_info = new Throwable[1];
            Callback cb = new Callback() {
                public int callback(Map.Ptr cb_arg0, Pointer _user) {
                    try {
                    fn.apply(new Map(_ctx, cb_arg0));
                    } catch (Throwable e) {
                        exc_info[0] = e;
                        return -1;
                    }
                    return 0;
                }
            };
            res = Impl.isl.isl_union_map_foreach_map(self.getPtr(), cb, Pointer.NULL);
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
            UnionMap self = this.asUnionMap();
            dim = dim.asSpace();
            res = Impl.isl.isl_union_map_contains(self.getPtr(), dim.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Map extractMap(Space dim) {
        Map.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            dim = dim.asSpace();
            res = Impl.isl.isl_union_map_extract_map(self.getPtr(), Impl.isl.isl_space_copy(dim.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public BasicMap sample() {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            res = Impl.isl.isl_union_map_sample(Impl.isl.isl_union_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public UnionMap fixedPowerVal(Val exp) {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            exp = exp.asVal();
            res = Impl.isl.isl_union_map_fixed_power_val(Impl.isl.isl_union_map_copy(self.getPtr()), Impl.isl.isl_val_copy(exp.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap power(boolean[] exact) {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            res = Impl.isl.isl_union_map_power(Impl.isl.isl_union_map_copy(self.getPtr()), exact);
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap transitiveClosure(boolean[] exact) {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            res = Impl.isl.isl_union_map_transitive_closure(Impl.isl.isl_union_map_copy(self.getPtr()), exact);
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap lexLtUnionMap(UnionMap umap2) {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            umap2 = umap2.asUnionMap();
            res = Impl.isl.isl_union_map_lex_lt_union_map(Impl.isl.isl_union_map_copy(self.getPtr()), Impl.isl.isl_union_map_copy(umap2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap lexLeUnionMap(UnionMap umap2) {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            umap2 = umap2.asUnionMap();
            res = Impl.isl.isl_union_map_lex_le_union_map(Impl.isl.isl_union_map_copy(self.getPtr()), Impl.isl.isl_union_map_copy(umap2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap lexGtUnionMap(UnionMap umap2) {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            umap2 = umap2.asUnionMap();
            res = Impl.isl.isl_union_map_lex_gt_union_map(Impl.isl.isl_union_map_copy(self.getPtr()), Impl.isl.isl_union_map_copy(umap2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap lexGeUnionMap(UnionMap umap2) {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            umap2 = umap2.asUnionMap();
            res = Impl.isl.isl_union_map_lex_ge_union_map(Impl.isl.isl_union_map_copy(self.getPtr()), Impl.isl.isl_union_map_copy(umap2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionSet wrap() {
        UnionSet.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            res = Impl.isl.isl_union_map_wrap(Impl.isl.isl_union_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionSet(this.ctx, res);
    }
    public UnionMap zip() {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            res = Impl.isl.isl_union_map_zip(Impl.isl.isl_union_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap curry() {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            res = Impl.isl.isl_union_map_curry(Impl.isl.isl_union_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap uncurry() {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            res = Impl.isl.isl_union_map_uncurry(Impl.isl.isl_union_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public UnionMap alignParams(Space model) {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            model = model.asSpace();
            res = Impl.isl.isl_union_map_align_params(Impl.isl.isl_union_map_copy(self.getPtr()), Impl.isl.isl_space_copy(model.getPtr()));
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public int computeFlow(UnionMap must_source, UnionMap may_source, UnionMap schedule, UnionMap[] must_dep, UnionMap[] may_dep, UnionMap[] must_no_source, UnionMap[] may_no_source) {
        int res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            must_source = must_source.asUnionMap();
            may_source = may_source.asUnionMap();
            schedule = schedule.asUnionMap();
            assert must_dep == null || must_dep.length == 1;
        UnionMap.Ptr[] _must_dep = must_dep != null ? new UnionMap.Ptr[1] : null;
            assert may_dep == null || may_dep.length == 1;
        UnionMap.Ptr[] _may_dep = may_dep != null ? new UnionMap.Ptr[1] : null;
            assert must_no_source == null || must_no_source.length == 1;
        UnionMap.Ptr[] _must_no_source = must_no_source != null ? new UnionMap.Ptr[1] : null;
            assert may_no_source == null || may_no_source.length == 1;
        UnionMap.Ptr[] _may_no_source = may_no_source != null ? new UnionMap.Ptr[1] : null;
            res = Impl.isl.isl_union_map_compute_flow(Impl.isl.isl_union_map_copy(self.getPtr()), Impl.isl.isl_union_map_copy(must_source.getPtr()), Impl.isl.isl_union_map_copy(may_source.getPtr()), Impl.isl.isl_union_map_copy(schedule.getPtr()), _must_dep, _may_dep, _must_no_source, _may_no_source);
        if (must_dep != null)
            must_dep[0] = new UnionMap(this.ctx, _must_dep[0]);
        if (may_dep != null)
            may_dep[0] = new UnionMap(this.ctx, _may_dep[0]);
        if (must_no_source != null)
            must_no_source[0] = new UnionMap(this.ctx, _must_no_source[0]);
        if (may_no_source != null)
            may_no_source[0] = new UnionMap(this.ctx, _may_no_source[0]);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public UnionPwQpolynomialFold applyUnionPwQpolynomialFold(UnionPwQpolynomialFold upwf, boolean[] tight) {
        UnionPwQpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            UnionMap self = this.asUnionMap();
            upwf = upwf.asUnionPwQpolynomialFold();
            res = Impl.isl.isl_union_map_apply_union_pw_qpolynomial_fold(Impl.isl.isl_union_map_copy(self.getPtr()), Impl.isl.isl_union_pw_qpolynomial_fold_copy(upwf.getPtr()), tight);
            Context.checkError(this.ctx);
        }
        return new UnionPwQpolynomialFold(this.ctx, res);
    }

}
