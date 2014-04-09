package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public class Set extends UnionSet {
    public static class Ptr extends UnionSet.Ptr { public Ptr() { super(); } }
    Set() {}
    Set(Context.Ptr ctx, PointerType ptr) {
        assert !ptr.getPointer().equals(Pointer.NULL);
        this.ctx = ctx;
        this.ptr = ptr;
    }
    Set.Ptr getPtr() { return (Set.Ptr)this.ptr; }
    Set.Ptr makePtr0() { 
        Set.Ptr p = (Set.Ptr)this.ptr;
        this.ptr = new Set.Ptr();
        return p;
    }
    // isl_set_from_pw_aff
    public Set(PwAff pwaff) {
        this.ctx = pwaff.ctx;
        synchronized(this.ctx) {
            pwaff = pwaff.asPwAff();
            this.ptr = Impl.isl.isl_set_from_pw_aff(Impl.isl.isl_pw_aff_copy(pwaff.getPtr()));
            Context.checkError(this.ctx);
        }
    }
    // isl_set_from_union_set
    public Set(UnionSet uset) {
        this.ctx = uset.ctx;
        synchronized(this.ctx) {
            uset = uset.asUnionSet();
            this.ptr = Impl.isl.isl_set_from_union_set(Impl.isl.isl_union_set_copy(uset.getPtr()));
            Context.checkError(this.ctx);
        }
    }
    // isl_set_read_from_str
    public Set(String str) {
        this.ctx = Context.getDefaultInstance();
        synchronized(this.ctx) {
            this.ptr = Impl.isl.isl_set_read_from_str(this.ctx, str);
            Context.checkError(this.ctx);
        }
    }
    // isl_set_from_basic_set
    public Set(BasicSet bset) {
        this.ctx = bset.ctx;
        synchronized(this.ctx) {
            bset = bset.asBasicSet();
            this.ptr = Impl.isl.isl_set_from_basic_set(Impl.isl.isl_basic_set_copy(bset.getPtr()));
            Context.checkError(this.ctx);
        }
    }
    // isl_set_from_point
    public Set(Point pnt) {
        this.ctx = pnt.ctx;
        synchronized(this.ctx) {
            pnt = pnt.asPoint();
            this.ptr = Impl.isl.isl_set_from_point(Impl.isl.isl_point_copy(pnt.getPtr()));
            Context.checkError(this.ctx);
        }
    }
    public static Set boxFromPoints(Point pnt1, Point pnt2) {
        Set that = new Set();
        that.ctx = pnt2.ctx;
        synchronized(that.ctx) {
            pnt1 = pnt1.asPoint();
            pnt2 = pnt2.asPoint();
            that.ptr = Impl.isl.isl_set_box_from_points(Impl.isl.isl_point_copy(pnt1.getPtr()), Impl.isl.isl_point_copy(pnt2.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    protected void finalize() {
        synchronized(this.ctx) {
            Impl.isl.isl_set_free(getPtr());
        }
    }
    public String toString() {
        Printer p = Printer.toStr();
        p = p.printSet(this);
        return p.getStr();
    }
    Set asSet() {
        Class clazz = this.getClass();
        if (clazz.equals(Set.class))
            return this;
        try {
            Constructor<Set> c = Set.class.getConstructor(clazz);
            return c.newInstance(this);
        } catch (Exception e) {
           throw new IllegalStateException("Cannot construct Set from " +
               clazz.getName() + " ?", e);
        }
    }
    public PwAff indicatorFunction() {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_indicator_function(Impl.isl.isl_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public Set addConstraint(Constraint constraint) {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            constraint = constraint.asConstraint();
            res = Impl.isl.isl_set_add_constraint(Impl.isl.isl_set_copy(self.getPtr()), Impl.isl.isl_constraint_copy(constraint.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public int dim(DimType type) {
        int res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_dim(self.getPtr(), type.value);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean hasTupleName() {
        boolean res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_has_tuple_name(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public String getTupleName() {
        String res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_get_tuple_name(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Set setTupleName(String s) {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_set_tuple_name(Impl.isl.isl_set_copy(self.getPtr()), s);
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public boolean hasDimName(DimType type, int pos) {
        boolean res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            assert pos >= 0;
            res = Impl.isl.isl_set_has_dim_name(self.getPtr(), type.value, pos);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public String getDimName(DimType type, int pos) {
        String res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            assert pos >= 0;
            res = Impl.isl.isl_set_get_dim_name(self.getPtr(), type.value, pos);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Set setDimName(DimType type, int pos, String s) {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            assert pos >= 0;
            res = Impl.isl.isl_set_set_dim_name(Impl.isl.isl_set_copy(self.getPtr()), type.value, pos, s);
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set lexmin() {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_lexmin(Impl.isl.isl_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set lexmax() {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_lexmax(Impl.isl.isl_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set params() {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_params(Impl.isl.isl_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public BasicSet sample() {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_sample(Impl.isl.isl_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public Point samplePoint() {
        Point.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_sample_point(Impl.isl.isl_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Point(this.ctx, res);
    }
    public Set detectEqualities() {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_detect_equalities(Impl.isl.isl_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public BasicSet affineHull() {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_affine_hull(Impl.isl.isl_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public BasicSet convexHull() {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_convex_hull(Impl.isl.isl_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public BasicSet polyhedralHull() {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_polyhedral_hull(Impl.isl.isl_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public BasicSet simpleHull() {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_simple_hull(Impl.isl.isl_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public BasicSet unshiftedSimpleHull() {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_unshifted_simple_hull(Impl.isl.isl_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public Set recessionCone() {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_recession_cone(Impl.isl.isl_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set union(Set set2) {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            set2 = set2.asSet();
            res = Impl.isl.isl_set_union(Impl.isl.isl_set_copy(self.getPtr()), Impl.isl.isl_set_copy(set2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set product(Set set2) {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            set2 = set2.asSet();
            res = Impl.isl.isl_set_product(Impl.isl.isl_set_copy(self.getPtr()), Impl.isl.isl_set_copy(set2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set flatProduct(Set set2) {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            set2 = set2.asSet();
            res = Impl.isl.isl_set_flat_product(Impl.isl.isl_set_copy(self.getPtr()), Impl.isl.isl_set_copy(set2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set intersect(Set set2) {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            set2 = set2.asSet();
            res = Impl.isl.isl_set_intersect(Impl.isl.isl_set_copy(self.getPtr()), Impl.isl.isl_set_copy(set2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set intersectParams(Set params) {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            params = params.asSet();
            res = Impl.isl.isl_set_intersect_params(Impl.isl.isl_set_copy(self.getPtr()), Impl.isl.isl_set_copy(params.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set subtract(Set set2) {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            set2 = set2.asSet();
            res = Impl.isl.isl_set_subtract(Impl.isl.isl_set_copy(self.getPtr()), Impl.isl.isl_set_copy(set2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set complement() {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_complement(Impl.isl.isl_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set apply(Map map) {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            map = map.asMap();
            res = Impl.isl.isl_set_apply(Impl.isl.isl_set_copy(self.getPtr()), Impl.isl.isl_map_copy(map.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set preimageMultiAff(MultiAff ma) {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            ma = ma.asMultiAff();
            res = Impl.isl.isl_set_preimage_multi_aff(Impl.isl.isl_set_copy(self.getPtr()), Impl.isl.isl_multi_aff_copy(ma.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set preimagePwMultiAff(PwMultiAff pma) {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            pma = pma.asPwMultiAff();
            res = Impl.isl.isl_set_preimage_pw_multi_aff(Impl.isl.isl_set_copy(self.getPtr()), Impl.isl.isl_pw_multi_aff_copy(pma.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set preimageMultiPwAff(MultiPwAff mpa) {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            mpa = mpa.asMultiPwAff();
            res = Impl.isl.isl_set_preimage_multi_pw_aff(Impl.isl.isl_set_copy(self.getPtr()), Impl.isl.isl_multi_pw_aff_copy(mpa.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set insertDims(DimType type, int pos, int n) {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            assert pos >= 0;
            assert n >= 0;
            res = Impl.isl.isl_set_insert_dims(Impl.isl.isl_set_copy(self.getPtr()), type.value, pos, n);
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set addDims(DimType type, int n) {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            assert n >= 0;
            res = Impl.isl.isl_set_add_dims(Impl.isl.isl_set_copy(self.getPtr()), type.value, n);
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set moveDims(DimType dst_type, int dst_pos, DimType src_type, int src_pos, int n) {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            assert dst_pos >= 0;
            assert src_pos >= 0;
            assert n >= 0;
            res = Impl.isl.isl_set_move_dims(Impl.isl.isl_set_copy(self.getPtr()), dst_type.value, dst_pos, src_type.value, src_pos, n);
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set projectOut(DimType type, int first, int n) {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_set_project_out(Impl.isl.isl_set_copy(self.getPtr()), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set removeDims(DimType type, int first, int n) {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_set_remove_dims(Impl.isl.isl_set_copy(self.getPtr()), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set removeDivsInvolvingDims(DimType type, int first, int n) {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_set_remove_divs_involving_dims(Impl.isl.isl_set_copy(self.getPtr()), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set removeUnknownDivs() {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_remove_unknown_divs(Impl.isl.isl_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set removeDivs() {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_remove_divs(Impl.isl.isl_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set splitDims(DimType type, int first, int n) {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_set_split_dims(Impl.isl.isl_set_copy(self.getPtr()), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set dropConstraintsInvolvingDims(DimType type, int first, int n) {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_set_drop_constraints_involving_dims(Impl.isl.isl_set_copy(self.getPtr()), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public boolean involvesDims(DimType type, int first, int n) {
        boolean res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_set_involves_dims(self.getPtr(), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean plainIsEmpty() {
        boolean res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_plain_is_empty(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public int fastIsEmpty() {
        int res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_fast_is_empty(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean plainIsUniverse() {
        boolean res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_plain_is_universe(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public int fastIsUniverse() {
        int res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_fast_is_universe(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isParams() {
        boolean res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_is_params(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isEmpty() {
        boolean res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_is_empty(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isBounded() {
        boolean res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_is_bounded(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isSubset(Set set2) {
        boolean res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            set2 = set2.asSet();
            res = Impl.isl.isl_set_is_subset(self.getPtr(), set2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isStrictSubset(Set set2) {
        boolean res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            set2 = set2.asSet();
            res = Impl.isl.isl_set_is_strict_subset(self.getPtr(), set2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isEqual(Set set2) {
        boolean res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            set2 = set2.asSet();
            res = Impl.isl.isl_set_is_equal(self.getPtr(), set2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isDisjoint(Set set2) {
        boolean res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            set2 = set2.asSet();
            res = Impl.isl.isl_set_is_disjoint(self.getPtr(), set2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isSingleton() {
        boolean res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_is_singleton(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isBox() {
        boolean res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_is_box(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean hasEqualSpace(Set set2) {
        boolean res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            set2 = set2.asSet();
            res = Impl.isl.isl_set_has_equal_space(self.getPtr(), set2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Set sum(Set set2) {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            set2 = set2.asSet();
            res = Impl.isl.isl_set_sum(Impl.isl.isl_set_copy(self.getPtr()), Impl.isl.isl_set_copy(set2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set neg() {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_neg(Impl.isl.isl_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set makeDisjoint() {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_make_disjoint(Impl.isl.isl_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set computeDivs() {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_compute_divs(Impl.isl.isl_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set alignDivs() {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_align_divs(Impl.isl.isl_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public int dimIsBounded(DimType type, int pos) {
        int res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            assert pos >= 0;
            res = Impl.isl.isl_set_dim_is_bounded(self.getPtr(), type.value, pos);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public int dimHasLowerBound(DimType type, int pos) {
        int res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            assert pos >= 0;
            res = Impl.isl.isl_set_dim_has_lower_bound(self.getPtr(), type.value, pos);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public int dimHasUpperBound(DimType type, int pos) {
        int res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            assert pos >= 0;
            res = Impl.isl.isl_set_dim_has_upper_bound(self.getPtr(), type.value, pos);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public int dimHasAnyLowerBound(DimType type, int pos) {
        int res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            assert pos >= 0;
            res = Impl.isl.isl_set_dim_has_any_lower_bound(self.getPtr(), type.value, pos);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public int dimHasAnyUpperBound(DimType type, int pos) {
        int res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            assert pos >= 0;
            res = Impl.isl.isl_set_dim_has_any_upper_bound(self.getPtr(), type.value, pos);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Set gist(Set context) {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            context = context.asSet();
            res = Impl.isl.isl_set_gist(Impl.isl.isl_set_copy(self.getPtr()), Impl.isl.isl_set_copy(context.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set coalesce() {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_coalesce(Impl.isl.isl_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public int nBasicSet() {
        int res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_n_basic_set(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public <ExceptionTy extends Exception> void foreachBasicSet(final XCallback1<BasicSet,ExceptionTy> fn) throws ExceptionTy {
        int res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            final Context.Ptr _ctx = this.ctx;
            final Throwable[] exc_info = new Throwable[1];
            Callback cb = new Callback() {
                public int callback(BasicSet.Ptr cb_arg0, Pointer _user) {
                    try {
                    fn.apply(new BasicSet(_ctx, cb_arg0));
                    } catch (Throwable e) {
                        exc_info[0] = e;
                        return -1;
                    }
                    return 0;
                }
            };
            res = Impl.isl.isl_set_foreach_basic_set(self.getPtr(), cb, Pointer.NULL);
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
    public <ExceptionTy extends Exception> void foreachPoint(final XCallback1<Point,ExceptionTy> fn) throws ExceptionTy {
        int res;
        synchronized(this.ctx) {
            Set self = this.asSet();
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
            res = Impl.isl.isl_set_foreach_point(self.getPtr(), cb, Pointer.NULL);
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
    public Val countVal() {
        Val.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_count_val(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new Val(this.ctx, res);
    }
    public Set lift() {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_lift(Impl.isl.isl_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Map lexLeSet(Set set2) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            set2 = set2.asSet();
            res = Impl.isl.isl_set_lex_le_set(Impl.isl.isl_set_copy(self.getPtr()), Impl.isl.isl_set_copy(set2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map lexLtSet(Set set2) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            set2 = set2.asSet();
            res = Impl.isl.isl_set_lex_lt_set(Impl.isl.isl_set_copy(self.getPtr()), Impl.isl.isl_set_copy(set2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map lexGeSet(Set set2) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            set2 = set2.asSet();
            res = Impl.isl.isl_set_lex_ge_set(Impl.isl.isl_set_copy(self.getPtr()), Impl.isl.isl_set_copy(set2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map lexGtSet(Set set2) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            set2 = set2.asSet();
            res = Impl.isl.isl_set_lex_gt_set(Impl.isl.isl_set_copy(self.getPtr()), Impl.isl.isl_set_copy(set2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public int size() {
        int res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_size(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Set alignParams(Space model) {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            model = model.asSpace();
            res = Impl.isl.isl_set_align_params(Impl.isl.isl_set_copy(self.getPtr()), Impl.isl.isl_space_copy(model.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public BasicSet coefficients() {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_coefficients(Impl.isl.isl_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public BasicSet solutions() {
        BasicSet.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_solutions(Impl.isl.isl_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicSet(this.ctx, res);
    }
    public PwAff dimMax(int pos) {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_dim_max(Impl.isl.isl_set_copy(self.getPtr()), pos);
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public PwAff dimMin(int pos) {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_dim_min(Impl.isl.isl_set_copy(self.getPtr()), pos);
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public Map identity() {
        Map.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_identity(Impl.isl.isl_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public boolean isWrapping() {
        boolean res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_is_wrapping(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Map unwrap() {
        Map.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_unwrap(Impl.isl.isl_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Set flatten() {
        Set.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_flatten(Impl.isl.isl_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Map flattenMap() {
        Map.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_flatten_map(Impl.isl.isl_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map lifting() {
        Map.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            res = Impl.isl.isl_set_lifting(Impl.isl.isl_set_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public PwQpolynomialFold applyPwQpolynomialFold(PwQpolynomialFold pwf, boolean[] tight) {
        PwQpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            Set self = this.asSet();
            pwf = pwf.asPwQpolynomialFold();
            res = Impl.isl.isl_set_apply_pw_qpolynomial_fold(Impl.isl.isl_set_copy(self.getPtr()), Impl.isl.isl_pw_qpolynomial_fold_copy(pwf.getPtr()), tight);
            Context.checkError(this.ctx);
        }
        return new PwQpolynomialFold(this.ctx, res);
    }

}
