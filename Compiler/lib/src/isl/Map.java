package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public class Map extends UnionMap {
    public static class Ptr extends UnionMap.Ptr { public Ptr() { super(); } }
    Map() {}
    Map(Context.Ptr ctx, PointerType ptr) {
        assert !ptr.getPointer().equals(Pointer.NULL);
        this.ctx = ctx;
        this.ptr = ptr;
    }
    Map.Ptr getPtr() { return (Map.Ptr)this.ptr; }
    Map.Ptr makePtr0() { 
        Map.Ptr p = (Map.Ptr)this.ptr;
        this.ptr = new Map.Ptr();
        return p;
    }
    // isl_map_from_pw_aff
    public Map(PwAff pwaff) {
        this.ctx = pwaff.ctx;
        synchronized(this.ctx) {
            pwaff = pwaff.asPwAff();
            this.ptr = Impl.isl.isl_map_from_pw_aff(Impl.isl.isl_pw_aff_copy(pwaff.getPtr()));
            Context.checkError(this.ctx);
        }
    }
    public static Map universe(Space dim) {
        Map that = new Map();
        that.ctx = dim.ctx;
        synchronized(that.ctx) {
            dim = dim.asSpace();
            that.ptr = Impl.isl.isl_map_universe(Impl.isl.isl_space_copy(dim.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static Map natUniverse(Space dim) {
        Map that = new Map();
        that.ctx = dim.ctx;
        synchronized(that.ctx) {
            dim = dim.asSpace();
            that.ptr = Impl.isl.isl_map_nat_universe(Impl.isl.isl_space_copy(dim.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static Map empty(Space dim) {
        Map that = new Map();
        that.ctx = dim.ctx;
        synchronized(that.ctx) {
            dim = dim.asSpace();
            that.ptr = Impl.isl.isl_map_empty(Impl.isl.isl_space_copy(dim.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static Map identity(Space dim) {
        Map that = new Map();
        that.ctx = dim.ctx;
        synchronized(that.ctx) {
            dim = dim.asSpace();
            that.ptr = Impl.isl.isl_map_identity(Impl.isl.isl_space_copy(dim.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static Map lexLtFirst(Space dim, int n) {
        Map that = new Map();
        that.ctx = dim.ctx;
        synchronized(that.ctx) {
            dim = dim.asSpace();
            assert n >= 0;
            that.ptr = Impl.isl.isl_map_lex_lt_first(Impl.isl.isl_space_copy(dim.getPtr()), n);
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static Map lexLeFirst(Space dim, int n) {
        Map that = new Map();
        that.ctx = dim.ctx;
        synchronized(that.ctx) {
            dim = dim.asSpace();
            assert n >= 0;
            that.ptr = Impl.isl.isl_map_lex_le_first(Impl.isl.isl_space_copy(dim.getPtr()), n);
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static Map lexLt(Space set_dim) {
        Map that = new Map();
        that.ctx = set_dim.ctx;
        synchronized(that.ctx) {
            set_dim = set_dim.asSpace();
            that.ptr = Impl.isl.isl_map_lex_lt(Impl.isl.isl_space_copy(set_dim.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static Map lexLe(Space set_dim) {
        Map that = new Map();
        that.ctx = set_dim.ctx;
        synchronized(that.ctx) {
            set_dim = set_dim.asSpace();
            that.ptr = Impl.isl.isl_map_lex_le(Impl.isl.isl_space_copy(set_dim.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static Map lexGtFirst(Space dim, int n) {
        Map that = new Map();
        that.ctx = dim.ctx;
        synchronized(that.ctx) {
            dim = dim.asSpace();
            assert n >= 0;
            that.ptr = Impl.isl.isl_map_lex_gt_first(Impl.isl.isl_space_copy(dim.getPtr()), n);
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static Map lexGeFirst(Space dim, int n) {
        Map that = new Map();
        that.ctx = dim.ctx;
        synchronized(that.ctx) {
            dim = dim.asSpace();
            assert n >= 0;
            that.ptr = Impl.isl.isl_map_lex_ge_first(Impl.isl.isl_space_copy(dim.getPtr()), n);
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static Map lexGt(Space set_dim) {
        Map that = new Map();
        that.ctx = set_dim.ctx;
        synchronized(that.ctx) {
            set_dim = set_dim.asSpace();
            that.ptr = Impl.isl.isl_map_lex_gt(Impl.isl.isl_space_copy(set_dim.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static Map lexGe(Space set_dim) {
        Map that = new Map();
        that.ctx = set_dim.ctx;
        synchronized(that.ctx) {
            set_dim = set_dim.asSpace();
            that.ptr = Impl.isl.isl_map_lex_ge(Impl.isl.isl_space_copy(set_dim.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    // isl_map_from_union_map
    public Map(UnionMap umap) {
        this.ctx = umap.ctx;
        synchronized(this.ctx) {
            umap = umap.asUnionMap();
            this.ptr = Impl.isl.isl_map_from_union_map(Impl.isl.isl_union_map_copy(umap.getPtr()));
            Context.checkError(this.ctx);
        }
    }
    // isl_map_read_from_str
    public Map(String str) {
        this.ctx = Context.getDefaultInstance();
        synchronized(this.ctx) {
            this.ptr = Impl.isl.isl_map_read_from_str(this.ctx, str);
            Context.checkError(this.ctx);
        }
    }
    // isl_map_from_basic_map
    public Map(BasicMap bmap) {
        this.ctx = bmap.ctx;
        synchronized(this.ctx) {
            bmap = bmap.asBasicMap();
            this.ptr = Impl.isl.isl_map_from_basic_map(Impl.isl.isl_basic_map_copy(bmap.getPtr()));
            Context.checkError(this.ctx);
        }
    }
    public static Map fromDomain(Set set) {
        Map that = new Map();
        that.ctx = set.ctx;
        synchronized(that.ctx) {
            set = set.asSet();
            that.ptr = Impl.isl.isl_map_from_domain(Impl.isl.isl_set_copy(set.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static Map fromDomainAndRange(Set domain, Set range) {
        Map that = new Map();
        that.ctx = range.ctx;
        synchronized(that.ctx) {
            domain = domain.asSet();
            range = range.asSet();
            that.ptr = Impl.isl.isl_map_from_domain_and_range(Impl.isl.isl_set_copy(domain.getPtr()), Impl.isl.isl_set_copy(range.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    // isl_map_from_set
    public Map(Set set, Space dim) {
        this.ctx = dim.ctx;
        synchronized(this.ctx) {
            set = set.asSet();
            dim = dim.asSpace();
            this.ptr = Impl.isl.isl_map_from_set(Impl.isl.isl_set_copy(set.getPtr()), Impl.isl.isl_space_copy(dim.getPtr()));
            Context.checkError(this.ctx);
        }
    }
    // isl_map_from_aff
    public Map(Aff aff) {
        this.ctx = aff.ctx;
        synchronized(this.ctx) {
            aff = aff.asAff();
            this.ptr = Impl.isl.isl_map_from_aff(Impl.isl.isl_aff_copy(aff.getPtr()));
            Context.checkError(this.ctx);
        }
    }
    // isl_map_from_multi_aff
    public Map(MultiAff maff) {
        this.ctx = maff.ctx;
        synchronized(this.ctx) {
            maff = maff.asMultiAff();
            this.ptr = Impl.isl.isl_map_from_multi_aff(Impl.isl.isl_multi_aff_copy(maff.getPtr()));
            Context.checkError(this.ctx);
        }
    }
    protected void finalize() {
        synchronized(this.ctx) {
            Impl.isl.isl_map_free(getPtr());
        }
    }
    public String toString() {
        Printer p = Printer.toStr();
        p = p.printMap(this);
        return p.getStr();
    }
    Map asMap() {
        Class clazz = this.getClass();
        if (clazz.equals(Map.class))
            return this;
        try {
            Constructor<Map> c = Map.class.getConstructor(clazz);
            return c.newInstance(this);
        } catch (Exception e) {
           throw new IllegalStateException("Cannot construct Map from " +
               clazz.getName() + " ?", e);
        }
    }
    public PwMultiAff lexminPwMultiAff() {
        PwMultiAff.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_lexmin_pw_multi_aff(Impl.isl.isl_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new PwMultiAff(this.ctx, res);
    }
    public Map addBasicMap(BasicMap bmap) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            bmap = bmap.asBasicMap();
            res = Impl.isl.isl_map_add_basic_map(Impl.isl.isl_map_copy(self.getPtr()), Impl.isl.isl_basic_map_copy(bmap.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map reverse() {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_reverse(Impl.isl.isl_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map union(Map map2) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            map2 = map2.asMap();
            res = Impl.isl.isl_map_union(Impl.isl.isl_map_copy(self.getPtr()), Impl.isl.isl_map_copy(map2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map intersectDomain(Set set) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            set = set.asSet();
            res = Impl.isl.isl_map_intersect_domain(Impl.isl.isl_map_copy(self.getPtr()), Impl.isl.isl_set_copy(set.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map intersectRange(Set set) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            set = set.asSet();
            res = Impl.isl.isl_map_intersect_range(Impl.isl.isl_map_copy(self.getPtr()), Impl.isl.isl_set_copy(set.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map applyDomain(Map map2) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            map2 = map2.asMap();
            res = Impl.isl.isl_map_apply_domain(Impl.isl.isl_map_copy(self.getPtr()), Impl.isl.isl_map_copy(map2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map applyRange(Map map2) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            map2 = map2.asMap();
            res = Impl.isl.isl_map_apply_range(Impl.isl.isl_map_copy(self.getPtr()), Impl.isl.isl_map_copy(map2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map preimageDomainMultiAff(MultiAff ma) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            ma = ma.asMultiAff();
            res = Impl.isl.isl_map_preimage_domain_multi_aff(Impl.isl.isl_map_copy(self.getPtr()), Impl.isl.isl_multi_aff_copy(ma.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map preimageDomainPwMultiAff(PwMultiAff pma) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            pma = pma.asPwMultiAff();
            res = Impl.isl.isl_map_preimage_domain_pw_multi_aff(Impl.isl.isl_map_copy(self.getPtr()), Impl.isl.isl_pw_multi_aff_copy(pma.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map preimageDomainMultiPwAff(MultiPwAff mpa) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            mpa = mpa.asMultiPwAff();
            res = Impl.isl.isl_map_preimage_domain_multi_pw_aff(Impl.isl.isl_map_copy(self.getPtr()), Impl.isl.isl_multi_pw_aff_copy(mpa.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map product(Map map2) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            map2 = map2.asMap();
            res = Impl.isl.isl_map_product(Impl.isl.isl_map_copy(self.getPtr()), Impl.isl.isl_map_copy(map2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map domainProduct(Map map2) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            map2 = map2.asMap();
            res = Impl.isl.isl_map_domain_product(Impl.isl.isl_map_copy(self.getPtr()), Impl.isl.isl_map_copy(map2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map rangeProduct(Map map2) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            map2 = map2.asMap();
            res = Impl.isl.isl_map_range_product(Impl.isl.isl_map_copy(self.getPtr()), Impl.isl.isl_map_copy(map2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map flatProduct(Map map2) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            map2 = map2.asMap();
            res = Impl.isl.isl_map_flat_product(Impl.isl.isl_map_copy(self.getPtr()), Impl.isl.isl_map_copy(map2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map flatDomainProduct(Map map2) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            map2 = map2.asMap();
            res = Impl.isl.isl_map_flat_domain_product(Impl.isl.isl_map_copy(self.getPtr()), Impl.isl.isl_map_copy(map2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map flatRangeProduct(Map map2) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            map2 = map2.asMap();
            res = Impl.isl.isl_map_flat_range_product(Impl.isl.isl_map_copy(self.getPtr()), Impl.isl.isl_map_copy(map2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map addConstraint(Constraint constraint) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            constraint = constraint.asConstraint();
            res = Impl.isl.isl_map_add_constraint(Impl.isl.isl_map_copy(self.getPtr()), Impl.isl.isl_constraint_copy(constraint.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public int nIn() {
        int res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_n_in(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public int nOut() {
        int res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_n_out(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public int nParam() {
        int res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_n_param(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public int dim(DimType type) {
        int res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_dim(self.getPtr(), type.value);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Space getSpace() {
        Space.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_get_space(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public boolean hasTupleName(DimType type) {
        boolean res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_has_tuple_name(self.getPtr(), type.value);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public String getTupleName(DimType type) {
        String res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_get_tuple_name(self.getPtr(), type.value);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Map setTupleName(DimType type, String s) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_set_tuple_name(Impl.isl.isl_map_copy(self.getPtr()), type.value, s);
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public boolean hasDimName(DimType type, int pos) {
        boolean res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            assert pos >= 0;
            res = Impl.isl.isl_map_has_dim_name(self.getPtr(), type.value, pos);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public String getDimName(DimType type, int pos) {
        String res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            assert pos >= 0;
            res = Impl.isl.isl_map_get_dim_name(self.getPtr(), type.value, pos);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Map setDimName(DimType type, int pos, String s) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            assert pos >= 0;
            res = Impl.isl.isl_map_set_dim_name(Impl.isl.isl_map_copy(self.getPtr()), type.value, pos, s);
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map setDimId(DimType type, int pos, Id id) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            assert pos >= 0;
            id = id.asId();
            res = Impl.isl.isl_map_set_dim_id(Impl.isl.isl_map_copy(self.getPtr()), type.value, pos, Impl.isl.isl_id_copy(id.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public boolean hasDimId(DimType type, int pos) {
        boolean res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            assert pos >= 0;
            res = Impl.isl.isl_map_has_dim_id(self.getPtr(), type.value, pos);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Id getDimId(DimType type, int pos) {
        Id.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            assert pos >= 0;
            res = Impl.isl.isl_map_get_dim_id(self.getPtr(), type.value, pos);
            Context.checkError(this.ctx);
        }
        return new Id(this.ctx, res);
    }
    public Map setTupleId(DimType type, Id id) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            id = id.asId();
            res = Impl.isl.isl_map_set_tuple_id(Impl.isl.isl_map_copy(self.getPtr()), type.value, Impl.isl.isl_id_copy(id.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map resetTupleId(DimType type) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_reset_tuple_id(Impl.isl.isl_map_copy(self.getPtr()), type.value);
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public boolean hasTupleId(DimType type) {
        boolean res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_has_tuple_id(self.getPtr(), type.value);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Id getTupleId(DimType type) {
        Id.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_get_tuple_id(self.getPtr(), type.value);
            Context.checkError(this.ctx);
        }
        return new Id(this.ctx, res);
    }
    public int findDimById(DimType type, Id id) {
        int res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            id = id.asId();
            res = Impl.isl.isl_map_find_dim_by_id(self.getPtr(), type.value, id.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public int findDimByName(DimType type, String name) {
        int res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_find_dim_by_name(self.getPtr(), type.value, name);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Map removeRedundancies() {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_remove_redundancies(Impl.isl.isl_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public BasicMap simpleHull() {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_simple_hull(Impl.isl.isl_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public BasicMap unshiftedSimpleHull() {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_unshifted_simple_hull(Impl.isl.isl_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public Map floordivVal(Val d) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            d = d.asVal();
            res = Impl.isl.isl_map_floordiv_val(Impl.isl.isl_map_copy(self.getPtr()), Impl.isl.isl_val_copy(d.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map lexmin() {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_lexmin(Impl.isl.isl_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map lexmax() {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_lexmax(Impl.isl.isl_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map intersect(Map map2) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            map2 = map2.asMap();
            res = Impl.isl.isl_map_intersect(Impl.isl.isl_map_copy(self.getPtr()), Impl.isl.isl_map_copy(map2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map intersectParams(Set params) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            params = params.asSet();
            res = Impl.isl.isl_map_intersect_params(Impl.isl.isl_map_copy(self.getPtr()), Impl.isl.isl_set_copy(params.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map subtract(Map map2) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            map2 = map2.asMap();
            res = Impl.isl.isl_map_subtract(Impl.isl.isl_map_copy(self.getPtr()), Impl.isl.isl_map_copy(map2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map subtractDomain(Set dom) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            dom = dom.asSet();
            res = Impl.isl.isl_map_subtract_domain(Impl.isl.isl_map_copy(self.getPtr()), Impl.isl.isl_set_copy(dom.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map subtractRange(Set dom) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            dom = dom.asSet();
            res = Impl.isl.isl_map_subtract_range(Impl.isl.isl_map_copy(self.getPtr()), Impl.isl.isl_set_copy(dom.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map complement() {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_complement(Impl.isl.isl_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map fixVal(DimType type, int pos, Val v) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            assert pos >= 0;
            v = v.asVal();
            res = Impl.isl.isl_map_fix_val(Impl.isl.isl_map_copy(self.getPtr()), type.value, pos, Impl.isl.isl_val_copy(v.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Set deltas() {
        Set.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_deltas(Impl.isl.isl_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Map deltasMap() {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_deltas_map(Impl.isl.isl_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map detectEqualities() {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_detect_equalities(Impl.isl.isl_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public BasicMap affineHull() {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_affine_hull(Impl.isl.isl_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public BasicMap convexHull() {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_convex_hull(Impl.isl.isl_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public BasicMap polyhedralHull() {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_polyhedral_hull(Impl.isl.isl_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public Map addDims(DimType type, int n) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            assert n >= 0;
            res = Impl.isl.isl_map_add_dims(Impl.isl.isl_map_copy(self.getPtr()), type.value, n);
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map insertDims(DimType type, int pos, int n) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            assert pos >= 0;
            assert n >= 0;
            res = Impl.isl.isl_map_insert_dims(Impl.isl.isl_map_copy(self.getPtr()), type.value, pos, n);
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map moveDims(DimType dst_type, int dst_pos, DimType src_type, int src_pos, int n) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            assert dst_pos >= 0;
            assert src_pos >= 0;
            assert n >= 0;
            res = Impl.isl.isl_map_move_dims(Impl.isl.isl_map_copy(self.getPtr()), dst_type.value, dst_pos, src_type.value, src_pos, n);
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map projectOut(DimType type, int first, int n) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_map_project_out(Impl.isl.isl_map_copy(self.getPtr()), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map removeUnknownDivs() {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_remove_unknown_divs(Impl.isl.isl_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map removeDivs() {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_remove_divs(Impl.isl.isl_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map eliminate(DimType type, int first, int n) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_map_eliminate(Impl.isl.isl_map_copy(self.getPtr()), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map removeDims(DimType type, int first, int n) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_map_remove_dims(Impl.isl.isl_map_copy(self.getPtr()), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map removeDivsInvolvingDims(DimType type, int first, int n) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_map_remove_divs_involving_dims(Impl.isl.isl_map_copy(self.getPtr()), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map equate(DimType type1, int pos1, DimType type2, int pos2) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_equate(Impl.isl.isl_map_copy(self.getPtr()), type1.value, pos1, type2.value, pos2);
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map oppose(DimType type1, int pos1, DimType type2, int pos2) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_oppose(Impl.isl.isl_map_copy(self.getPtr()), type1.value, pos1, type2.value, pos2);
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map orderLt(DimType type1, int pos1, DimType type2, int pos2) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_order_lt(Impl.isl.isl_map_copy(self.getPtr()), type1.value, pos1, type2.value, pos2);
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map orderGt(DimType type1, int pos1, DimType type2, int pos2) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_order_gt(Impl.isl.isl_map_copy(self.getPtr()), type1.value, pos1, type2.value, pos2);
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Set wrap() {
        Set.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_wrap(Impl.isl.isl_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Map flatten() {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_flatten(Impl.isl.isl_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map flattenDomain() {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_flatten_domain(Impl.isl.isl_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map flattenRange() {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_flatten_range(Impl.isl.isl_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Set params() {
        Set.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_params(Impl.isl.isl_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set domain() {
        Set.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_domain(Impl.isl.isl_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Set range() {
        Set.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_range(Impl.isl.isl_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Set(this.ctx, res);
    }
    public Map domainMap() {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_domain_map(Impl.isl.isl_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map rangeMap() {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_range_map(Impl.isl.isl_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public BasicMap sample() {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_sample(Impl.isl.isl_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }
    public boolean plainIsEmpty() {
        boolean res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_plain_is_empty(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public int fastIsEmpty() {
        int res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_fast_is_empty(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean plainIsUniverse() {
        boolean res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_plain_is_universe(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isEmpty() {
        boolean res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_is_empty(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isSubset(Map map2) {
        boolean res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            map2 = map2.asMap();
            res = Impl.isl.isl_map_is_subset(self.getPtr(), map2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isStrictSubset(Map map2) {
        boolean res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            map2 = map2.asMap();
            res = Impl.isl.isl_map_is_strict_subset(self.getPtr(), map2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isEqual(Map map2) {
        boolean res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            map2 = map2.asMap();
            res = Impl.isl.isl_map_is_equal(self.getPtr(), map2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isDisjoint(Map map2) {
        boolean res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            map2 = map2.asMap();
            res = Impl.isl.isl_map_is_disjoint(self.getPtr(), map2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean plainIsSingleValued() {
        boolean res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_plain_is_single_valued(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isSingleValued() {
        boolean res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_is_single_valued(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean plainIsInjective() {
        boolean res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_plain_is_injective(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isInjective() {
        boolean res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_is_injective(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isBijective() {
        boolean res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_is_bijective(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isTranslation() {
        boolean res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_is_translation(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean hasEqualSpace(Map map2) {
        boolean res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            map2 = map2.asMap();
            res = Impl.isl.isl_map_has_equal_space(self.getPtr(), map2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean canZip() {
        boolean res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_can_zip(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Map zip() {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_zip(Impl.isl.isl_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public boolean canCurry() {
        boolean res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_can_curry(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Map curry() {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_curry(Impl.isl.isl_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public boolean canUncurry() {
        boolean res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_can_uncurry(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Map uncurry() {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_uncurry(Impl.isl.isl_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map makeDisjoint() {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_make_disjoint(Impl.isl.isl_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map computeDivs() {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_compute_divs(Impl.isl.isl_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map alignDivs() {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_align_divs(Impl.isl.isl_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map dropConstraintsInvolvingDims(DimType type, int first, int n) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_map_drop_constraints_involving_dims(Impl.isl.isl_map_copy(self.getPtr()), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public boolean involvesDims(DimType type, int first, int n) {
        boolean res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_map_involves_dims(self.getPtr(), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Map gist(Map context) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            context = context.asMap();
            res = Impl.isl.isl_map_gist(Impl.isl.isl_map_copy(self.getPtr()), Impl.isl.isl_map_copy(context.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map gistDomain(Set context) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            context = context.asSet();
            res = Impl.isl.isl_map_gist_domain(Impl.isl.isl_map_copy(self.getPtr()), Impl.isl.isl_set_copy(context.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map gistRange(Set context) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            context = context.asSet();
            res = Impl.isl.isl_map_gist_range(Impl.isl.isl_map_copy(self.getPtr()), Impl.isl.isl_set_copy(context.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map gistParams(Set context) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            context = context.asSet();
            res = Impl.isl.isl_map_gist_params(Impl.isl.isl_map_copy(self.getPtr()), Impl.isl.isl_set_copy(context.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map gistBasicMap(BasicMap context) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            context = context.asBasicMap();
            res = Impl.isl.isl_map_gist_basic_map(Impl.isl.isl_map_copy(self.getPtr()), Impl.isl.isl_basic_map_copy(context.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map coalesce() {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_coalesce(Impl.isl.isl_map_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public boolean plainIsEqual(Map map2) {
        boolean res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            map2 = map2.asMap();
            res = Impl.isl.isl_map_plain_is_equal(self.getPtr(), map2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public int fastIsEqual(Map map2) {
        int res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            map2 = map2.asMap();
            res = Impl.isl.isl_map_fast_is_equal(self.getPtr(), map2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public <ExceptionTy extends Exception> void foreachBasicMap(final XCallback1<BasicMap,ExceptionTy> fn) throws ExceptionTy {
        int res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            final Context.Ptr _ctx = this.ctx;
            final Throwable[] exc_info = new Throwable[1];
            Callback cb = new Callback() {
                public int callback(BasicMap.Ptr cb_arg0, Pointer _user) {
                    try {
                    fn.apply(new BasicMap(_ctx, cb_arg0));
                    } catch (Throwable e) {
                        exc_info[0] = e;
                        return -1;
                    }
                    return 0;
                }
            };
            res = Impl.isl.isl_map_foreach_basic_map(self.getPtr(), cb, Pointer.NULL);
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
    public Map fixedPowerVal(Val exp) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            exp = exp.asVal();
            res = Impl.isl.isl_map_fixed_power_val(Impl.isl.isl_map_copy(self.getPtr()), Impl.isl.isl_val_copy(exp.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map transitiveClosure(boolean[] exact) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_transitive_closure(Impl.isl.isl_map_copy(self.getPtr()), exact);
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map lexLeMap(Map map2) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            map2 = map2.asMap();
            res = Impl.isl.isl_map_lex_le_map(Impl.isl.isl_map_copy(self.getPtr()), Impl.isl.isl_map_copy(map2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map lexLtMap(Map map2) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            map2 = map2.asMap();
            res = Impl.isl.isl_map_lex_lt_map(Impl.isl.isl_map_copy(self.getPtr()), Impl.isl.isl_map_copy(map2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map lexGeMap(Map map2) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            map2 = map2.asMap();
            res = Impl.isl.isl_map_lex_ge_map(Impl.isl.isl_map_copy(self.getPtr()), Impl.isl.isl_map_copy(map2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map lexGtMap(Map map2) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            map2 = map2.asMap();
            res = Impl.isl.isl_map_lex_gt_map(Impl.isl.isl_map_copy(self.getPtr()), Impl.isl.isl_map_copy(map2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public Map alignParams(Space model) {
        Map.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            model = model.asSpace();
            res = Impl.isl.isl_map_align_params(Impl.isl.isl_map_copy(self.getPtr()), Impl.isl.isl_space_copy(model.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Map(this.ctx, res);
    }
    public PwAff dimMax(int pos) {
        PwAff.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            res = Impl.isl.isl_map_dim_max(Impl.isl.isl_map_copy(self.getPtr()), pos);
            Context.checkError(this.ctx);
        }
        return new PwAff(this.ctx, res);
    }
    public PwQpolynomialFold applyPwQpolynomialFold(PwQpolynomialFold pwf, boolean[] tight) {
        PwQpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            Map self = this.asMap();
            pwf = pwf.asPwQpolynomialFold();
            res = Impl.isl.isl_map_apply_pw_qpolynomial_fold(Impl.isl.isl_map_copy(self.getPtr()), Impl.isl.isl_pw_qpolynomial_fold_copy(pwf.getPtr()), tight);
            Context.checkError(this.ctx);
        }
        return new PwQpolynomialFold(this.ctx, res);
    }

}
