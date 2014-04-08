package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public class BasicMap extends Map {
    public static class Ptr extends Map.Ptr { public Ptr() { super(); } }
    BasicMap() {}
    BasicMap(Context.Ptr ctx, PointerType ptr) {
        assert !ptr.getPointer().equals(Pointer.NULL);
        this.ctx = ctx;
        this.ptr = ptr;
    }
    BasicMap.Ptr getPtr() { return (BasicMap.Ptr)this.ptr; }
    BasicMap.Ptr makePtr0() { 
        BasicMap.Ptr p = (BasicMap.Ptr)this.ptr;
        this.ptr = new BasicMap.Ptr();
        return p;
    }
    // isl_basic_map_from_constraint
    public BasicMap(Constraint constraint) {
        this.ctx = constraint.ctx;
        synchronized(this.ctx) {
            constraint = constraint.asConstraint();
            this.ptr = Impl.isl.isl_basic_map_from_constraint(Impl.isl.isl_constraint_copy(constraint.getPtr()));
            Context.checkError(this.ctx);
        }
    }
    public static BasicMap identity(Space dim) {
        BasicMap that = new BasicMap();
        that.ctx = dim.ctx;
        synchronized(that.ctx) {
            dim = dim.asSpace();
            that.ptr = Impl.isl.isl_basic_map_identity(Impl.isl.isl_space_copy(dim.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static BasicMap equal(Space dim, int n_equal) {
        BasicMap that = new BasicMap();
        that.ctx = dim.ctx;
        synchronized(that.ctx) {
            dim = dim.asSpace();
            assert n_equal >= 0;
            that.ptr = Impl.isl.isl_basic_map_equal(Impl.isl.isl_space_copy(dim.getPtr()), n_equal);
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static BasicMap lessAt(Space dim, int pos) {
        BasicMap that = new BasicMap();
        that.ctx = dim.ctx;
        synchronized(that.ctx) {
            dim = dim.asSpace();
            assert pos >= 0;
            that.ptr = Impl.isl.isl_basic_map_less_at(Impl.isl.isl_space_copy(dim.getPtr()), pos);
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static BasicMap moreAt(Space dim, int pos) {
        BasicMap that = new BasicMap();
        that.ctx = dim.ctx;
        synchronized(that.ctx) {
            dim = dim.asSpace();
            assert pos >= 0;
            that.ptr = Impl.isl.isl_basic_map_more_at(Impl.isl.isl_space_copy(dim.getPtr()), pos);
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static BasicMap empty(Space dim) {
        BasicMap that = new BasicMap();
        that.ctx = dim.ctx;
        synchronized(that.ctx) {
            dim = dim.asSpace();
            that.ptr = Impl.isl.isl_basic_map_empty(Impl.isl.isl_space_copy(dim.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static BasicMap universe(Space dim) {
        BasicMap that = new BasicMap();
        that.ctx = dim.ctx;
        synchronized(that.ctx) {
            dim = dim.asSpace();
            that.ptr = Impl.isl.isl_basic_map_universe(Impl.isl.isl_space_copy(dim.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static BasicMap natUniverse(Space dim) {
        BasicMap that = new BasicMap();
        that.ctx = dim.ctx;
        synchronized(that.ctx) {
            dim = dim.asSpace();
            that.ptr = Impl.isl.isl_basic_map_nat_universe(Impl.isl.isl_space_copy(dim.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static BasicMap universeLike(BasicMap bmap) {
        BasicMap that = new BasicMap();
        that.ctx = bmap.ctx;
        synchronized(that.ctx) {
            bmap = bmap.asBasicMap();
            that.ptr = Impl.isl.isl_basic_map_universe_like(bmap.getPtr());
            Context.checkError(that.ctx);
        }
        return that;
    }
    // isl_basic_map_from_basic_set
    public BasicMap(BasicSet bset, Space dim) {
        this.ctx = dim.ctx;
        synchronized(this.ctx) {
            bset = bset.asBasicSet();
            dim = dim.asSpace();
            this.ptr = Impl.isl.isl_basic_map_from_basic_set(Impl.isl.isl_basic_set_copy(bset.getPtr()), Impl.isl.isl_space_copy(dim.getPtr()));
            Context.checkError(this.ctx);
        }
    }
    // isl_basic_map_read_from_str
    public BasicMap(String str) {
        this.ctx = Context.getDefaultInstance();
        synchronized(this.ctx) {
            this.ptr = Impl.isl.isl_basic_map_read_from_str(this.ctx, str);
            Context.checkError(this.ctx);
        }
    }
    public static BasicMap fromDomain(BasicSet bset) {
        BasicMap that = new BasicMap();
        that.ctx = bset.ctx;
        synchronized(that.ctx) {
            bset = bset.asBasicSet();
            that.ptr = Impl.isl.isl_basic_map_from_domain(Impl.isl.isl_basic_set_copy(bset.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static BasicMap fromRange(BasicSet bset) {
        BasicMap that = new BasicMap();
        that.ctx = bset.ctx;
        synchronized(that.ctx) {
            bset = bset.asBasicSet();
            that.ptr = Impl.isl.isl_basic_map_from_range(Impl.isl.isl_basic_set_copy(bset.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static BasicMap fromDomainAndRange(BasicSet domain, BasicSet range) {
        BasicMap that = new BasicMap();
        that.ctx = range.ctx;
        synchronized(that.ctx) {
            domain = domain.asBasicSet();
            range = range.asBasicSet();
            that.ptr = Impl.isl.isl_basic_map_from_domain_and_range(Impl.isl.isl_basic_set_copy(domain.getPtr()), Impl.isl.isl_basic_set_copy(range.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static BasicMap fromConstraintMatrices(Space dim, Mat eq, Mat ineq, DimType c1, DimType c2, DimType c3, DimType c4, DimType c5) {
        BasicMap that = new BasicMap();
        that.ctx = ineq.ctx;
        synchronized(that.ctx) {
            dim = dim.asSpace();
            eq = eq.asMat();
            ineq = ineq.asMat();
            that.ptr = Impl.isl.isl_basic_map_from_constraint_matrices(Impl.isl.isl_space_copy(dim.getPtr()), Impl.isl.isl_mat_copy(eq.getPtr()), Impl.isl.isl_mat_copy(ineq.getPtr()), c1.value, c2.value, c3.value, c4.value, c5.value);
            Context.checkError(that.ctx);
        }
        return that;
    }
    // isl_basic_map_from_aff
    public BasicMap(Aff aff) {
        this.ctx = aff.ctx;
        synchronized(this.ctx) {
            aff = aff.asAff();
            this.ptr = Impl.isl.isl_basic_map_from_aff(Impl.isl.isl_aff_copy(aff.getPtr()));
            Context.checkError(this.ctx);
        }
    }
    // isl_basic_map_from_multi_aff
    public BasicMap(MultiAff maff) {
        this.ctx = maff.ctx;
        synchronized(this.ctx) {
            maff = maff.asMultiAff();
            this.ptr = Impl.isl.isl_basic_map_from_multi_aff(Impl.isl.isl_multi_aff_copy(maff.getPtr()));
            Context.checkError(this.ctx);
        }
    }
    // isl_basic_map_from_qpolynomial
    public BasicMap(Qpolynomial qp) {
        this.ctx = qp.ctx;
        synchronized(this.ctx) {
            qp = qp.asQpolynomial();
            this.ptr = Impl.isl.isl_basic_map_from_qpolynomial(Impl.isl.isl_qpolynomial_copy(qp.getPtr()));
            Context.checkError(this.ctx);
        }
    }
    protected void finalize() {
        synchronized(this.ctx) {
            Impl.isl.isl_basic_map_free(getPtr());
        }
    }
    public String toString() {
        Printer p = Printer.toStr();
        p = p.printBasicMap(this);
        return p.getStr();
    }
    BasicMap asBasicMap() {
        Class clazz = this.getClass();
        if (clazz.equals(BasicMap.class))
            return this;
        try {
            Constructor<BasicMap> c = BasicMap.class.getConstructor(clazz);
            return c.newInstance(this);
        } catch (Exception e) {
           throw new IllegalStateException("Cannot construct BasicMap from " +
               clazz.getName() + " ?", e);
        }
    }
    public PwMultiAff lexminPwMultiAff() {
        PwMultiAff.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_lexmin_pw_multi_aff(Impl.isl.isl_basic_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwMultiAff(this.ctx, res);
    }
    public boolean isUniverse() {
        boolean res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_is_universe(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isEmpty() {
        boolean res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_is_empty(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isSubset(BasicMap bmap2) {
        boolean res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            bmap2 = bmap2.asBasicMap();
            res = Impl.isl.isl_basic_map_is_subset(self.getPtr(), bmap2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isStrictSubset(BasicMap bmap2) {
        boolean res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            bmap2 = bmap2.asBasicMap();
            res = Impl.isl.isl_basic_map_is_strict_subset(self.getPtr(), bmap2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public BasicMap product(BasicMap bmap2) {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            bmap2 = bmap2.asBasicMap();
            res = Impl.isl.isl_basic_map_product(Impl.isl.isl_basic_map_copy(self.getPtr()), Impl.isl.isl_basic_map_copy(bmap2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public BasicMap domainProduct(BasicMap bmap2) {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            bmap2 = bmap2.asBasicMap();
            res = Impl.isl.isl_basic_map_domain_product(Impl.isl.isl_basic_map_copy(self.getPtr()), Impl.isl.isl_basic_map_copy(bmap2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public BasicMap rangeProduct(BasicMap bmap2) {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            bmap2 = bmap2.asBasicMap();
            res = Impl.isl.isl_basic_map_range_product(Impl.isl.isl_basic_map_copy(self.getPtr()), Impl.isl.isl_basic_map_copy(bmap2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public BasicMap flatProduct(BasicMap bmap2) {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            bmap2 = bmap2.asBasicMap();
            res = Impl.isl.isl_basic_map_flat_product(Impl.isl.isl_basic_map_copy(self.getPtr()), Impl.isl.isl_basic_map_copy(bmap2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public BasicMap flatRangeProduct(BasicMap bmap2) {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            bmap2 = bmap2.asBasicMap();
            res = Impl.isl.isl_basic_map_flat_range_product(Impl.isl.isl_basic_map_copy(self.getPtr()), Impl.isl.isl_basic_map_copy(bmap2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public <ExceptionTy extends Exception> void foreachConstraint(final XCallback1<Constraint,ExceptionTy> fn) throws ExceptionTy {
        int res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            final Context.Ptr _ctx = this.ctx;
            final Throwable[] exc_info = new Throwable[1];
            Callback cb = new Callback() {
                public int callback(Constraint.Ptr cb_arg0, Pointer _user) {
                    try {
                    fn.apply(new Constraint(_ctx, cb_arg0));
                    } catch (Throwable e) {
                        exc_info[0] = e;
                        return -1;
                    }
                    return 0;
                }
            };
            res = Impl.isl.isl_basic_map_foreach_constraint(self.getPtr(), cb, Pointer.NULL);
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
    public BasicMap addConstraint(Constraint constraint) {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            constraint = constraint.asConstraint();
            res = Impl.isl.isl_basic_map_add_constraint(Impl.isl.isl_basic_map_copy(self.getPtr()), Impl.isl.isl_constraint_copy(constraint.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public int nIn() {
        int res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_n_in(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public int nOut() {
        int res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_n_out(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public int nParam() {
        int res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_n_param(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public int nDiv() {
        int res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_n_div(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public int totalDim() {
        int res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_total_dim(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public int dim(DimType type) {
        int res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_dim(self.getPtr(), type.value);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Space getSpace() {
        Space.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_get_space(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public Aff getDiv(int pos) {
        Aff.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_get_div(self.getPtr(), pos);
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public LocalSpace getLocalSpace() {
        LocalSpace.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_get_local_space(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new LocalSpace(this.ctx, res);
    }
    public BasicMap setTupleName(DimType type, String s) {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_set_tuple_name(Impl.isl.isl_basic_map_copy(self.getPtr()), type.value, s);
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public String getTupleName(DimType type) {
        String res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_get_tuple_name(self.getPtr(), type.value);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public String getDimName(DimType type, int pos) {
        String res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            assert pos >= 0;
            res = Impl.isl.isl_basic_map_get_dim_name(self.getPtr(), type.value, pos);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public BasicMap setDimName(DimType type, int pos, String s) {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            assert pos >= 0;
            res = Impl.isl.isl_basic_map_set_dim_name(Impl.isl.isl_basic_map_copy(self.getPtr()), type.value, pos, s);
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public BasicMap setTupleId(DimType type, Id id) {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            id = id.asId();
            res = Impl.isl.isl_basic_map_set_tuple_id(Impl.isl.isl_basic_map_copy(self.getPtr()), type.value, Impl.isl.isl_id_copy(id.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public boolean hasDimId(DimType type, int pos) {
        boolean res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            assert pos >= 0;
            res = Impl.isl.isl_basic_map_has_dim_id(self.getPtr(), type.value, pos);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isRational() {
        boolean res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_is_rational(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public BasicMap removeRedundancies() {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_remove_redundancies(Impl.isl.isl_basic_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public BasicMap intersectDomain(BasicSet bset) {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            bset = bset.asBasicSet();
            res = Impl.isl.isl_basic_map_intersect_domain(Impl.isl.isl_basic_map_copy(self.getPtr()), Impl.isl.isl_basic_set_copy(bset.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public BasicMap intersectRange(BasicSet bset) {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            bset = bset.asBasicSet();
            res = Impl.isl.isl_basic_map_intersect_range(Impl.isl.isl_basic_map_copy(self.getPtr()), Impl.isl.isl_basic_set_copy(bset.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public BasicMap intersect(BasicMap bmap2) {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            bmap2 = bmap2.asBasicMap();
            res = Impl.isl.isl_basic_map_intersect(Impl.isl.isl_basic_map_copy(self.getPtr()), Impl.isl.isl_basic_map_copy(bmap2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public Map union(BasicMap bmap2) {
        Map.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            bmap2 = bmap2.asBasicMap();
            res = Impl.isl.isl_basic_map_union(Impl.isl.isl_basic_map_copy(self.getPtr()), Impl.isl.isl_basic_map_copy(bmap2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public BasicMap applyDomain(BasicMap bmap2) {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            bmap2 = bmap2.asBasicMap();
            res = Impl.isl.isl_basic_map_apply_domain(Impl.isl.isl_basic_map_copy(self.getPtr()), Impl.isl.isl_basic_map_copy(bmap2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public BasicMap applyRange(BasicMap bmap2) {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            bmap2 = bmap2.asBasicMap();
            res = Impl.isl.isl_basic_map_apply_range(Impl.isl.isl_basic_map_copy(self.getPtr()), Impl.isl.isl_basic_map_copy(bmap2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public BasicMap affineHull() {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_affine_hull(Impl.isl.isl_basic_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public BasicMap reverse() {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_reverse(Impl.isl.isl_basic_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public BasicSet domain() {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_domain(Impl.isl.isl_basic_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public BasicSet range() {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_range(Impl.isl.isl_basic_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public BasicMap domainMap() {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_domain_map(Impl.isl.isl_basic_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public BasicMap rangeMap() {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_range_map(Impl.isl.isl_basic_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public BasicMap removeDims(DimType type, int first, int n) {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_basic_map_remove_dims(Impl.isl.isl_basic_map_copy(self.getPtr()), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public BasicMap eliminate(DimType type, int first, int n) {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_basic_map_eliminate(Impl.isl.isl_basic_map_copy(self.getPtr()), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public BasicMap sample() {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_sample(Impl.isl.isl_basic_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public BasicMap detectEqualities() {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_detect_equalities(Impl.isl.isl_basic_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public BasicMap fixVal(DimType type, int pos, Val v) {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            assert pos >= 0;
            v = v.asVal();
            res = Impl.isl.isl_basic_map_fix_val(Impl.isl.isl_basic_map_copy(self.getPtr()), type.value, pos, Impl.isl.isl_val_copy(v.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public boolean isEqual(BasicMap bmap2) {
        boolean res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            bmap2 = bmap2.asBasicMap();
            res = Impl.isl.isl_basic_map_is_equal(self.getPtr(), bmap2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isDisjoint(BasicMap bmap2) {
        boolean res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            bmap2 = bmap2.asBasicMap();
            res = Impl.isl.isl_basic_map_is_disjoint(self.getPtr(), bmap2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Map lexmin() {
        Map.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_lexmin(Impl.isl.isl_basic_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map lexmax() {
        Map.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_lexmax(Impl.isl.isl_basic_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public BasicSet deltas() {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_deltas(Impl.isl.isl_basic_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public BasicMap deltasMap() {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_deltas_map(Impl.isl.isl_basic_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public BasicMap add(DimType type, int n) {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            assert n >= 0;
            res = Impl.isl.isl_basic_map_add(Impl.isl.isl_basic_map_copy(self.getPtr()), type.value, n);
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public BasicMap insertDims(DimType type, int pos, int n) {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            assert pos >= 0;
            assert n >= 0;
            res = Impl.isl.isl_basic_map_insert_dims(Impl.isl.isl_basic_map_copy(self.getPtr()), type.value, pos, n);
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public BasicMap moveDims(DimType dst_type, int dst_pos, DimType src_type, int src_pos, int n) {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            assert dst_pos >= 0;
            assert src_pos >= 0;
            assert n >= 0;
            res = Impl.isl.isl_basic_map_move_dims(Impl.isl.isl_basic_map_copy(self.getPtr()), dst_type.value, dst_pos, src_type.value, src_pos, n);
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public BasicMap projectOut(DimType type, int first, int n) {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_basic_map_project_out(Impl.isl.isl_basic_map_copy(self.getPtr()), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public BasicMap removeDivs() {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_remove_divs(Impl.isl.isl_basic_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public BasicMap removeDivsInvolvingDims(DimType type, int first, int n) {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_basic_map_remove_divs_involving_dims(Impl.isl.isl_basic_map_copy(self.getPtr()), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public BasicMap equate(DimType type1, int pos1, DimType type2, int pos2) {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_equate(Impl.isl.isl_basic_map_copy(self.getPtr()), type1.value, pos1, type2.value, pos2);
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public BasicMap orderGe(DimType type1, int pos1, DimType type2, int pos2) {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_order_ge(Impl.isl.isl_basic_map_copy(self.getPtr()), type1.value, pos1, type2.value, pos2);
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public BasicMap orderGt(DimType type1, int pos1, DimType type2, int pos2) {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_order_gt(Impl.isl.isl_basic_map_copy(self.getPtr()), type1.value, pos1, type2.value, pos2);
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public BasicSet wrap() {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_wrap(Impl.isl.isl_basic_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public BasicMap flatten() {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_flatten(Impl.isl.isl_basic_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public BasicMap flattenDomain() {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_flatten_domain(Impl.isl.isl_basic_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public BasicMap flattenRange() {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_flatten_range(Impl.isl.isl_basic_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public boolean isSingleValued() {
        boolean res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_is_single_valued(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean canZip() {
        boolean res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_can_zip(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public BasicMap zip() {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_zip(Impl.isl.isl_basic_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public boolean canCurry() {
        boolean res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_can_curry(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public BasicMap curry() {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_curry(Impl.isl.isl_basic_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public boolean canUncurry() {
        boolean res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_can_uncurry(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public BasicMap uncurry() {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_uncurry(Impl.isl.isl_basic_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public Map computeDivs() {
        Map.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_compute_divs(Impl.isl.isl_basic_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public BasicMap dropConstraintsInvolvingDims(DimType type, int first, int n) {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_basic_map_drop_constraints_involving_dims(Impl.isl.isl_basic_map_copy(self.getPtr()), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public boolean involvesDims(DimType type, int first, int n) {
        boolean res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_basic_map_involves_dims(self.getPtr(), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public BasicMap gist(BasicMap context) {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            context = context.asBasicMap();
            res = Impl.isl.isl_basic_map_gist(Impl.isl.isl_basic_map_copy(self.getPtr()), Impl.isl.isl_basic_map_copy(context.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public BasicMap alignParams(Space model) {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            model = model.asSpace();
            res = Impl.isl.isl_basic_map_align_params(Impl.isl.isl_basic_map_copy(self.getPtr()), Impl.isl.isl_space_copy(model.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public Mat equalitiesMatrix(DimType c1, DimType c2, DimType c3, DimType c4, DimType c5) {
        Mat.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_equalities_matrix(self.getPtr(), c1.value, c2.value, c3.value, c4.value, c5.value);
            Context.checkError(this.ctx);
        }
        return new Mat(this.ctx, res);
    }
    public Mat inequalitiesMatrix(DimType c1, DimType c2, DimType c3, DimType c4, DimType c5) {
        Mat.Ptr res;
        synchronized(this.ctx) {
            BasicMap self = this.asBasicMap();
            res = Impl.isl.isl_basic_map_inequalities_matrix(self.getPtr(), c1.value, c2.value, c3.value, c4.value, c5.value);
            Context.checkError(this.ctx);
        }
        return new Mat(this.ctx, res);
    }

}
