package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public class BasicSet extends Set {
    public static class Ptr extends Set.Ptr { public Ptr() { super(); } }
    BasicSet() {}
    BasicSet(Context.Ptr ctx, PointerType ptr) {
        assert !ptr.getPointer().equals(Pointer.NULL);
        this.ctx = ctx;
        this.ptr = ptr;
    }
    BasicSet.Ptr getPtr() { return (BasicSet.Ptr)this.ptr; }
    BasicSet.Ptr makePtr0() { 
        BasicSet.Ptr p = (BasicSet.Ptr)this.ptr;
        this.ptr = new BasicSet.Ptr();
        return p;
    }
    // isl_basic_set_from_constraint
    public BasicSet(Constraint constraint) {
        this.ctx = constraint.ctx;
        synchronized(this.ctx) {
            constraint = constraint.asConstraint();
            this.ptr = Impl.isl.isl_basic_set_from_constraint(constraint.getPtr());
            Context.checkError(this.ctx);
        }
    }
    public static BasicSet universe(Space dim) {
        BasicSet that = new BasicSet();
        that.ctx = dim.ctx;
        synchronized(that.ctx) {
            dim = dim.asSpace();
            that.ptr = Impl.isl.isl_basic_set_universe(Impl.isl.isl_space_copy(dim.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    // isl_basic_set_read_from_str
    public BasicSet(String str) {
        this.ctx = Context.getDefaultInstance();
        synchronized(this.ctx) {
            this.ptr = Impl.isl.isl_basic_set_read_from_str(this.ctx, str);
            Context.checkError(this.ctx);
        }
    }
    // isl_basic_set_from_point
    public BasicSet(Point pnt) {
        this.ctx = pnt.ctx;
        synchronized(this.ctx) {
            pnt = pnt.asPoint();
            this.ptr = Impl.isl.isl_basic_set_from_point(Impl.isl.isl_point_copy(pnt.getPtr()));
            Context.checkError(this.ctx);
        }
    }
    public static BasicSet boxFromPoints(Point pnt1, Point pnt2) {
        BasicSet that = new BasicSet();
        that.ctx = pnt2.ctx;
        synchronized(that.ctx) {
            pnt1 = pnt1.asPoint();
            pnt2 = pnt2.asPoint();
            that.ptr = Impl.isl.isl_basic_set_box_from_points(Impl.isl.isl_point_copy(pnt1.getPtr()), Impl.isl.isl_point_copy(pnt2.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static BasicSet fromConstraintMatrices(Space dim, Mat eq, Mat ineq, DimType c1, DimType c2, DimType c3, DimType c4) {
        BasicSet that = new BasicSet();
        that.ctx = ineq.ctx;
        synchronized(that.ctx) {
            dim = dim.asSpace();
            eq = eq.asMat();
            ineq = ineq.asMat();
            that.ptr = Impl.isl.isl_basic_set_from_constraint_matrices(Impl.isl.isl_space_copy(dim.getPtr()), Impl.isl.isl_mat_copy(eq.getPtr()), Impl.isl.isl_mat_copy(ineq.getPtr()), c1.value, c2.value, c3.value, c4.value);
            Context.checkError(that.ctx);
        }
        return that;
    }
    protected void finalize() {
        synchronized(this.ctx) {
            Impl.isl.isl_basic_set_free(getPtr());
        }
    }
    public String toString() {
        Printer p = Printer.toStr();
        p = p.printBasicSet(this);
        return p.getStr();
    }
    BasicSet asBasicSet() {
        Class clazz = this.getClass();
        if (clazz.equals(BasicSet.class))
            return this;
        try {
            Constructor<BasicSet> c = BasicSet.class.getConstructor(clazz);
            return c.newInstance(this);
        } catch (Exception e) {
           throw new IllegalStateException("Cannot construct BasicSet from " +
               clazz.getName() + " ?", e);
        }
    }
    public int nConstraint() {
        int res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            res = Impl.isl.isl_basic_set_n_constraint(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public <ExceptionTy extends Exception> void foreachConstraint(final XCallback1<Constraint,ExceptionTy> fn) throws ExceptionTy {
        int res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
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
            res = Impl.isl.isl_basic_set_foreach_constraint(self.getPtr(), cb, Pointer.NULL);
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
    public <ExceptionTy extends Exception> void foreachBoundPair(DimType type, int pos, final XCallback3<Constraint,Constraint,BasicSet,ExceptionTy> fn) throws ExceptionTy {
        int res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            assert pos >= 0;
            final Context.Ptr _ctx = this.ctx;
            final Throwable[] exc_info = new Throwable[1];
            Callback cb = new Callback() {
                public int callback(Constraint.Ptr cb_arg0, Constraint.Ptr cb_arg1, BasicSet.Ptr cb_arg2, Pointer _user) {
                    try {
                    fn.apply(new Constraint(_ctx, cb_arg0), new Constraint(_ctx, cb_arg1), new BasicSet(_ctx, cb_arg2));
                    } catch (Throwable e) {
                        exc_info[0] = e;
                        return -1;
                    }
                    return 0;
                }
            };
            res = Impl.isl.isl_basic_set_foreach_bound_pair(self.getPtr(), type.value, pos, cb, Pointer.NULL);
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
    public BasicSet addConstraint(Constraint constraint) {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            constraint = constraint.asConstraint();
            res = Impl.isl.isl_basic_set_add_constraint(Impl.isl.isl_basic_set_copy(self.getPtr()), Impl.isl.isl_constraint_copy(constraint.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public int dim(DimType type) {
        int res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            res = Impl.isl.isl_basic_set_dim(self.getPtr(), type.value);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public LocalSpace getLocalSpace() {
        LocalSpace.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            res = Impl.isl.isl_basic_set_get_local_space(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new LocalSpace(this.ctx, res);
    }
    public String getTupleName() {
        String res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            res = Impl.isl.isl_basic_set_get_tuple_name(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public BasicSet setTupleName(String s) {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            res = Impl.isl.isl_basic_set_set_tuple_name(Impl.isl.isl_basic_set_copy(self.getPtr()), s);
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public String getDimName(DimType type, int pos) {
        String res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            assert pos >= 0;
            res = Impl.isl.isl_basic_set_get_dim_name(self.getPtr(), type.value, pos);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public BasicSet setDimName(DimType type, int pos, String s) {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            assert pos >= 0;
            res = Impl.isl.isl_basic_set_set_dim_name(Impl.isl.isl_basic_set_copy(self.getPtr()), type.value, pos, s);
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public BasicSet intersect(BasicSet bset2) {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            bset2 = bset2.asBasicSet();
            res = Impl.isl.isl_basic_set_intersect(Impl.isl.isl_basic_set_copy(self.getPtr()), Impl.isl.isl_basic_set_copy(bset2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public BasicSet intersectParams(BasicSet bset2) {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            bset2 = bset2.asBasicSet();
            res = Impl.isl.isl_basic_set_intersect_params(Impl.isl.isl_basic_set_copy(self.getPtr()), Impl.isl.isl_basic_set_copy(bset2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public BasicSet apply(BasicMap bmap) {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            bmap = bmap.asBasicMap();
            res = Impl.isl.isl_basic_set_apply(Impl.isl.isl_basic_set_copy(self.getPtr()), Impl.isl.isl_basic_map_copy(bmap.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public BasicSet affineHull() {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            res = Impl.isl.isl_basic_set_affine_hull(Impl.isl.isl_basic_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public BasicSet sample() {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            res = Impl.isl.isl_basic_set_sample(Impl.isl.isl_basic_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public BasicSet detectEqualities() {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            res = Impl.isl.isl_basic_set_detect_equalities(Impl.isl.isl_basic_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public boolean isEqual(BasicSet bset2) {
        boolean res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            bset2 = bset2.asBasicSet();
            res = Impl.isl.isl_basic_set_is_equal(self.getPtr(), bset2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Set lexmin() {
        Set.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            res = Impl.isl.isl_basic_set_lexmin(Impl.isl.isl_basic_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set lexmax() {
        Set.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            res = Impl.isl.isl_basic_set_lexmax(Impl.isl.isl_basic_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set union(BasicSet bset2) {
        Set.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            bset2 = bset2.asBasicSet();
            res = Impl.isl.isl_basic_set_union(Impl.isl.isl_basic_set_copy(self.getPtr()), Impl.isl.isl_basic_set_copy(bset2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public BasicSet params() {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            res = Impl.isl.isl_basic_set_params(Impl.isl.isl_basic_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public boolean isEmpty() {
        boolean res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            res = Impl.isl.isl_basic_set_is_empty(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isSubset(BasicSet bset2) {
        boolean res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            bset2 = bset2.asBasicSet();
            res = Impl.isl.isl_basic_set_is_subset(self.getPtr(), bset2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Point samplePoint() {
        Point.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            res = Impl.isl.isl_basic_set_sample_point(Impl.isl.isl_basic_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Point(this.ctx, res);
    }
    public BasicSet flatProduct(BasicSet bset2) {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            bset2 = bset2.asBasicSet();
            res = Impl.isl.isl_basic_set_flat_product(Impl.isl.isl_basic_set_copy(self.getPtr()), Impl.isl.isl_basic_set_copy(bset2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public BasicSet insertDims(DimType type, int pos, int n) {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            assert pos >= 0;
            assert n >= 0;
            res = Impl.isl.isl_basic_set_insert_dims(Impl.isl.isl_basic_set_copy(self.getPtr()), type.value, pos, n);
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public BasicSet addDims(DimType type, int n) {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            assert n >= 0;
            res = Impl.isl.isl_basic_set_add_dims(Impl.isl.isl_basic_set_copy(self.getPtr()), type.value, n);
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public BasicSet moveDims(DimType dst_type, int dst_pos, DimType src_type, int src_pos, int n) {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            assert dst_pos >= 0;
            assert src_pos >= 0;
            assert n >= 0;
            res = Impl.isl.isl_basic_set_move_dims(Impl.isl.isl_basic_set_copy(self.getPtr()), dst_type.value, dst_pos, src_type.value, src_pos, n);
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public BasicSet projectOut(DimType type, int first, int n) {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_basic_set_project_out(Impl.isl.isl_basic_set_copy(self.getPtr()), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public BasicSet removeDivs() {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            res = Impl.isl.isl_basic_set_remove_divs(Impl.isl.isl_basic_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public BasicSet eliminate(DimType type, int first, int n) {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_basic_set_eliminate(Impl.isl.isl_basic_set_copy(self.getPtr()), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public BasicSet removeDivsInvolvingDims(DimType type, int first, int n) {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_basic_set_remove_divs_involving_dims(Impl.isl.isl_basic_set_copy(self.getPtr()), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public BasicSet removeUnknownDivs() {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            res = Impl.isl.isl_basic_set_remove_unknown_divs(Impl.isl.isl_basic_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public BasicSet dropConstraintsInvolvingDims(DimType type, int first, int n) {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_basic_set_drop_constraints_involving_dims(Impl.isl.isl_basic_set_copy(self.getPtr()), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public BasicSet dropConstraintsNotInvolvingDims(DimType type, int first, int n) {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_basic_set_drop_constraints_not_involving_dims(Impl.isl.isl_basic_set_copy(self.getPtr()), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public boolean involvesDims(DimType type, int first, int n) {
        boolean res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_basic_set_involves_dims(self.getPtr(), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public BasicSet neg() {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            res = Impl.isl.isl_basic_set_neg(Impl.isl.isl_basic_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public BasicSet gist(BasicSet context) {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            context = context.asBasicSet();
            res = Impl.isl.isl_basic_set_gist(Impl.isl.isl_basic_set_copy(self.getPtr()), Impl.isl.isl_basic_set_copy(context.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public BasicSet lift() {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            res = Impl.isl.isl_basic_set_lift(Impl.isl.isl_basic_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public BasicSet alignParams(Space model) {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            model = model.asSpace();
            res = Impl.isl.isl_basic_set_align_params(Impl.isl.isl_basic_set_copy(self.getPtr()), Impl.isl.isl_space_copy(model.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public Mat equalitiesMatrix(DimType c1, DimType c2, DimType c3, DimType c4) {
        Mat.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            res = Impl.isl.isl_basic_set_equalities_matrix(self.getPtr(), c1.value, c2.value, c3.value, c4.value);
            Context.checkError(this.ctx);
        }
        return new Mat(this.ctx, res);
    }
    public Mat inequalitiesMatrix(DimType c1, DimType c2, DimType c3, DimType c4) {
        Mat.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            res = Impl.isl.isl_basic_set_inequalities_matrix(self.getPtr(), c1.value, c2.value, c3.value, c4.value);
            Context.checkError(this.ctx);
        }
        return new Mat(this.ctx, res);
    }
    public Mat reducedBasis() {
        Mat.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            res = Impl.isl.isl_basic_set_reduced_basis(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new Mat(this.ctx, res);
    }
    public BasicSet coefficients() {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            res = Impl.isl.isl_basic_set_coefficients(Impl.isl.isl_basic_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public BasicSet solutions() {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            res = Impl.isl.isl_basic_set_solutions(Impl.isl.isl_basic_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public boolean isWrapping() {
        boolean res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            res = Impl.isl.isl_basic_set_is_wrapping(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public BasicMap unwrap() {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            res = Impl.isl.isl_basic_set_unwrap(Impl.isl.isl_basic_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public BasicSet flatten() {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            BasicSet self = this.asBasicSet();
            res = Impl.isl.isl_basic_set_flatten(Impl.isl.isl_basic_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }

}
