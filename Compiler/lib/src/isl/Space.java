package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public class Space {
    public static class Ptr extends PointerType { public Ptr() { super(); } }
    protected Context.Ptr ctx;
    protected PointerType ptr;
    Space() {}
    Space(Context.Ptr ctx, PointerType ptr) {
        assert !ptr.getPointer().equals(Pointer.NULL);
        this.ctx = ctx;
        this.ptr = ptr;
    }
    Space.Ptr getPtr() { return (Space.Ptr)this.ptr; }
    Space.Ptr makePtr0() { 
        Space.Ptr p = (Space.Ptr)this.ptr;
        this.ptr = new Space.Ptr();
        return p;
    }
    public static Space alloc(int nparam, int n_in, int n_out) {
        Space that = new Space();
        that.ctx = Context.getDefaultInstance();
        synchronized(that.ctx) {
            assert nparam >= 0;
            assert n_in >= 0;
            assert n_out >= 0;
            that.ptr = Impl.isl.isl_space_alloc(that.ctx, nparam, n_in, n_out);
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static Space setAlloc(int nparam, int dim) {
        Space that = new Space();
        that.ctx = Context.getDefaultInstance();
        synchronized(that.ctx) {
            assert nparam >= 0;
            assert dim >= 0;
            that.ptr = Impl.isl.isl_space_set_alloc(that.ctx, nparam, dim);
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static Space paramsAlloc(int nparam) {
        Space that = new Space();
        that.ctx = Context.getDefaultInstance();
        synchronized(that.ctx) {
            assert nparam >= 0;
            that.ptr = Impl.isl.isl_space_params_alloc(that.ctx, nparam);
            Context.checkError(that.ctx);
        }
        return that;
    }
    protected void finalize() {
        synchronized(this.ctx) {
            Impl.isl.isl_space_free(getPtr());
        }
    }
    public String toString() {
        Printer p = Printer.toStr();
        p = p.printSpace(this);
        return p.getStr();
    }
    Space asSpace() {
        Class clazz = this.getClass();
        if (clazz.equals(Space.class))
            return this;
        try {
            Constructor<Space> c = Space.class.getConstructor(clazz);
            return c.newInstance(this);
        } catch (Exception e) {
           throw new IllegalStateException("Cannot construct Space from " +
               clazz.getName() + " ?", e);
        }
    }
    public Id getDimId(DimType type, int pos) {
        Id.Ptr res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            assert pos >= 0;
            res = Impl.isl.isl_space_get_dim_id(self.getPtr(), type.value, pos);
            Context.checkError(this.ctx);
        }
        return new Id(this.ctx, res);
    }
    public int findDimById(DimType type, Id id) {
        int res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            id = id.asId();
            res = Impl.isl.isl_space_find_dim_by_id(self.getPtr(), type.value, id.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public int findDimByName(DimType type, String name) {
        int res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            res = Impl.isl.isl_space_find_dim_by_name(self.getPtr(), type.value, name);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isParams() {
        boolean res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            res = Impl.isl.isl_space_is_params(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isSet() {
        boolean res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            res = Impl.isl.isl_space_is_set(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isMap() {
        boolean res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            res = Impl.isl.isl_space_is_map(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Space setTupleName(DimType type, String s) {
        Space.Ptr res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            res = Impl.isl.isl_space_set_tuple_name(Impl.isl.isl_space_copy(self.getPtr()), type.value, s);
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public String getTupleName(DimType type) {
        String res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            res = Impl.isl.isl_space_get_tuple_name(self.getPtr(), type.value);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Space setTupleId(DimType type, Id id) {
        Space.Ptr res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            id = id.asId();
            res = Impl.isl.isl_space_set_tuple_id(Impl.isl.isl_space_copy(self.getPtr()), type.value, Impl.isl.isl_id_copy(id.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public Space resetTupleId(DimType type) {
        Space.Ptr res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            res = Impl.isl.isl_space_reset_tuple_id(Impl.isl.isl_space_copy(self.getPtr()), type.value);
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public boolean hasTupleId(DimType type) {
        boolean res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            res = Impl.isl.isl_space_has_tuple_id(self.getPtr(), type.value);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Space setDimId(DimType type, int pos, Id id) {
        Space.Ptr res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            assert pos >= 0;
            id = id.asId();
            res = Impl.isl.isl_space_set_dim_id(Impl.isl.isl_space_copy(self.getPtr()), type.value, pos, Impl.isl.isl_id_copy(id.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public boolean hasDimId(DimType type, int pos) {
        boolean res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            assert pos >= 0;
            res = Impl.isl.isl_space_has_dim_id(self.getPtr(), type.value, pos);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean hasDimName(DimType type, int pos) {
        boolean res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            assert pos >= 0;
            res = Impl.isl.isl_space_has_dim_name(self.getPtr(), type.value, pos);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Space setDimName(DimType type, int pos, String name) {
        Space.Ptr res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            assert pos >= 0;
            res = Impl.isl.isl_space_set_dim_name(Impl.isl.isl_space_copy(self.getPtr()), type.value, pos, name);
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public String getDimName(DimType type, int pos) {
        String res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            assert pos >= 0;
            res = Impl.isl.isl_space_get_dim_name(self.getPtr(), type.value, pos);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Space extend(int nparam, int n_in, int n_out) {
        Space.Ptr res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            assert nparam >= 0;
            assert n_in >= 0;
            assert n_out >= 0;
            res = Impl.isl.isl_space_extend(Impl.isl.isl_space_copy(self.getPtr()), nparam, n_in, n_out);
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public Space addDims(DimType type, int n) {
        Space.Ptr res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            assert n >= 0;
            res = Impl.isl.isl_space_add_dims(Impl.isl.isl_space_copy(self.getPtr()), type.value, n);
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public Space moveDims(DimType dst_type, int dst_pos, DimType src_type, int src_pos, int n) {
        Space.Ptr res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            assert dst_pos >= 0;
            assert src_pos >= 0;
            assert n >= 0;
            res = Impl.isl.isl_space_move_dims(Impl.isl.isl_space_copy(self.getPtr()), dst_type.value, dst_pos, src_type.value, src_pos, n);
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public Space insertDims(DimType type, int pos, int n) {
        Space.Ptr res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            assert pos >= 0;
            assert n >= 0;
            res = Impl.isl.isl_space_insert_dims(Impl.isl.isl_space_copy(self.getPtr()), type.value, pos, n);
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public Space join(Space right) {
        Space.Ptr res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            right = right.asSpace();
            res = Impl.isl.isl_space_join(Impl.isl.isl_space_copy(self.getPtr()), Impl.isl.isl_space_copy(right.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public Space product(Space right) {
        Space.Ptr res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            right = right.asSpace();
            res = Impl.isl.isl_space_product(Impl.isl.isl_space_copy(self.getPtr()), Impl.isl.isl_space_copy(right.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public Space domainProduct(Space right) {
        Space.Ptr res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            right = right.asSpace();
            res = Impl.isl.isl_space_domain_product(Impl.isl.isl_space_copy(self.getPtr()), Impl.isl.isl_space_copy(right.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public Space rangeProduct(Space right) {
        Space.Ptr res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            right = right.asSpace();
            res = Impl.isl.isl_space_range_product(Impl.isl.isl_space_copy(self.getPtr()), Impl.isl.isl_space_copy(right.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public Space mapFromSet() {
        Space.Ptr res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            res = Impl.isl.isl_space_map_from_set(Impl.isl.isl_space_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public Space mapFromDomainAndRange(Space range) {
        Space.Ptr res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            range = range.asSpace();
            res = Impl.isl.isl_space_map_from_domain_and_range(Impl.isl.isl_space_copy(self.getPtr()), Impl.isl.isl_space_copy(range.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public Space reverse() {
        Space.Ptr res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            res = Impl.isl.isl_space_reverse(Impl.isl.isl_space_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public Space dropDims(DimType type, int first, int num) {
        Space.Ptr res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            assert first >= 0;
            assert num >= 0;
            res = Impl.isl.isl_space_drop_dims(Impl.isl.isl_space_copy(self.getPtr()), type.value, first, num);
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public Space dropInputs(int first, int n) {
        Space.Ptr res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_space_drop_inputs(Impl.isl.isl_space_copy(self.getPtr()), first, n);
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public Space dropOutputs(int first, int n) {
        Space.Ptr res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_space_drop_outputs(Impl.isl.isl_space_copy(self.getPtr()), first, n);
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public Space domain() {
        Space.Ptr res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            res = Impl.isl.isl_space_domain(Impl.isl.isl_space_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public Space fromDomain() {
        Space.Ptr res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            res = Impl.isl.isl_space_from_domain(Impl.isl.isl_space_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public Space range() {
        Space.Ptr res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            res = Impl.isl.isl_space_range(Impl.isl.isl_space_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public Space fromRange() {
        Space.Ptr res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            res = Impl.isl.isl_space_from_range(Impl.isl.isl_space_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public Space domainMap() {
        Space.Ptr res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            res = Impl.isl.isl_space_domain_map(Impl.isl.isl_space_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public Space rangeMap() {
        Space.Ptr res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            res = Impl.isl.isl_space_range_map(Impl.isl.isl_space_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public Space params() {
        Space.Ptr res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            res = Impl.isl.isl_space_params(Impl.isl.isl_space_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public Space setFromParams() {
        Space.Ptr res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            res = Impl.isl.isl_space_set_from_params(Impl.isl.isl_space_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public Space alignParams(Space dim2) {
        Space.Ptr res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            dim2 = dim2.asSpace();
            res = Impl.isl.isl_space_align_params(Impl.isl.isl_space_copy(self.getPtr()), Impl.isl.isl_space_copy(dim2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public boolean isWrapping() {
        boolean res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            res = Impl.isl.isl_space_is_wrapping(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Space wrap() {
        Space.Ptr res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            res = Impl.isl.isl_space_wrap(Impl.isl.isl_space_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public Space unwrap() {
        Space.Ptr res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            res = Impl.isl.isl_space_unwrap(Impl.isl.isl_space_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public boolean canZip() {
        boolean res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            res = Impl.isl.isl_space_can_zip(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Space zip() {
        Space.Ptr res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            res = Impl.isl.isl_space_zip(Impl.isl.isl_space_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public boolean canCurry() {
        boolean res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            res = Impl.isl.isl_space_can_curry(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Space curry() {
        Space.Ptr res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            res = Impl.isl.isl_space_curry(Impl.isl.isl_space_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public boolean canUncurry() {
        boolean res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            res = Impl.isl.isl_space_can_uncurry(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Space uncurry() {
        Space.Ptr res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            res = Impl.isl.isl_space_uncurry(Impl.isl.isl_space_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public boolean isDomain(Space space2) {
        boolean res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            space2 = space2.asSpace();
            res = Impl.isl.isl_space_is_domain(self.getPtr(), space2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isRange(Space space2) {
        boolean res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            space2 = space2.asSpace();
            res = Impl.isl.isl_space_is_range(self.getPtr(), space2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isEqual(Space space2) {
        boolean res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            space2 = space2.asSpace();
            res = Impl.isl.isl_space_is_equal(self.getPtr(), space2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public int match(DimType dim1_type, Space dim2, DimType dim2_type) {
        int res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            dim2 = dim2.asSpace();
            res = Impl.isl.isl_space_match(self.getPtr(), dim1_type.value, dim2.getPtr(), dim2_type.value);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public int tupleMatch(DimType dim1_type, Space dim2, DimType dim2_type) {
        int res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            dim2 = dim2.asSpace();
            res = Impl.isl.isl_space_tuple_match(self.getPtr(), dim1_type.value, dim2.getPtr(), dim2_type.value);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public int compatible(Space dim2) {
        int res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            dim2 = dim2.asSpace();
            res = Impl.isl.isl_space_compatible(self.getPtr(), dim2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public int dim(DimType type) {
        int res;
        synchronized(this.ctx) {
            Space self = this.asSpace();
            res = Impl.isl.isl_space_dim(self.getPtr(), type.value);
            Context.checkError(this.ctx);
        }
        return res;
    }

}
