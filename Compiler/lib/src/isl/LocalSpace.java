package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public class LocalSpace {
    public static class Ptr extends PointerType { public Ptr() { super(); } }
    protected Context.Ptr ctx;
    protected PointerType ptr;
    LocalSpace() {}
    LocalSpace(Context.Ptr ctx, PointerType ptr) {
        assert !ptr.getPointer().equals(Pointer.NULL);
        this.ctx = ctx;
        this.ptr = ptr;
    }
    LocalSpace.Ptr getPtr() { return (LocalSpace.Ptr)this.ptr; }
    LocalSpace.Ptr makePtr0() { 
        LocalSpace.Ptr p = (LocalSpace.Ptr)this.ptr;
        this.ptr = new LocalSpace.Ptr();
        return p;
    }
    // isl_local_space_from_space
    public LocalSpace(Space dim) {
        this.ctx = dim.ctx;
        synchronized(this.ctx) {
            dim = dim.asSpace();
            this.ptr = Impl.isl.isl_local_space_from_space(Impl.isl.isl_space_copy(dim.getPtr()));
            Context.checkError(this.ctx);
        }
    }
    protected void finalize() {
        synchronized(this.ctx) {
            Impl.isl.isl_local_space_free(getPtr());
        }
    }
    public String toString() {
        Printer p = Printer.toStr();
        p = p.printLocalSpace(this);
        return p.getStr();
    }
    LocalSpace asLocalSpace() {
        Class clazz = this.getClass();
        if (clazz.equals(LocalSpace.class))
            return this;
        try {
            Constructor<LocalSpace> c = LocalSpace.class.getConstructor(clazz);
            return c.newInstance(this);
        } catch (Exception e) {
           throw new IllegalStateException("Cannot construct LocalSpace from " +
               clazz.getName() + " ?", e);
        }
    }
    public boolean isSet() {
        boolean res;
        synchronized(this.ctx) {
            LocalSpace self = this.asLocalSpace();
            res = Impl.isl.isl_local_space_is_set(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public LocalSpace setTupleId(DimType type, Id id) {
        LocalSpace.Ptr res;
        synchronized(this.ctx) {
            LocalSpace self = this.asLocalSpace();
            id = id.asId();
            res = Impl.isl.isl_local_space_set_tuple_id(Impl.isl.isl_local_space_copy(self.getPtr()), type.value, Impl.isl.isl_id_copy(id.getPtr()));
            Context.checkError(this.ctx);
        }
        return new LocalSpace(this.ctx, res);
    }
    public int dim(DimType type) {
        int res;
        synchronized(this.ctx) {
            LocalSpace self = this.asLocalSpace();
            res = Impl.isl.isl_local_space_dim(self.getPtr(), type.value);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean hasDimName(DimType type, int pos) {
        boolean res;
        synchronized(this.ctx) {
            LocalSpace self = this.asLocalSpace();
            assert pos >= 0;
            res = Impl.isl.isl_local_space_has_dim_name(self.getPtr(), type.value, pos);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public String getDimName(DimType type, int pos) {
        String res;
        synchronized(this.ctx) {
            LocalSpace self = this.asLocalSpace();
            assert pos >= 0;
            res = Impl.isl.isl_local_space_get_dim_name(self.getPtr(), type.value, pos);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public LocalSpace setDimName(DimType type, int pos, String s) {
        LocalSpace.Ptr res;
        synchronized(this.ctx) {
            LocalSpace self = this.asLocalSpace();
            assert pos >= 0;
            res = Impl.isl.isl_local_space_set_dim_name(Impl.isl.isl_local_space_copy(self.getPtr()), type.value, pos, s);
            Context.checkError(this.ctx);
        }
        return new LocalSpace(this.ctx, res);
    }
    public boolean hasDimId(DimType type, int pos) {
        boolean res;
        synchronized(this.ctx) {
            LocalSpace self = this.asLocalSpace();
            assert pos >= 0;
            res = Impl.isl.isl_local_space_has_dim_id(self.getPtr(), type.value, pos);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Id getDimId(DimType type, int pos) {
        Id.Ptr res;
        synchronized(this.ctx) {
            LocalSpace self = this.asLocalSpace();
            assert pos >= 0;
            res = Impl.isl.isl_local_space_get_dim_id(self.getPtr(), type.value, pos);
            Context.checkError(this.ctx);
        }
        return new Id(this.ctx, res);
    }
    public LocalSpace setDimId(DimType type, int pos, Id id) {
        LocalSpace.Ptr res;
        synchronized(this.ctx) {
            LocalSpace self = this.asLocalSpace();
            assert pos >= 0;
            id = id.asId();
            res = Impl.isl.isl_local_space_set_dim_id(Impl.isl.isl_local_space_copy(self.getPtr()), type.value, pos, Impl.isl.isl_id_copy(id.getPtr()));
            Context.checkError(this.ctx);
        }
        return new LocalSpace(this.ctx, res);
    }
    public Space getSpace() {
        Space.Ptr res;
        synchronized(this.ctx) {
            LocalSpace self = this.asLocalSpace();
            res = Impl.isl.isl_local_space_get_space(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public Aff getDiv(int pos) {
        Aff.Ptr res;
        synchronized(this.ctx) {
            LocalSpace self = this.asLocalSpace();
            res = Impl.isl.isl_local_space_get_div(self.getPtr(), pos);
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public LocalSpace domain() {
        LocalSpace.Ptr res;
        synchronized(this.ctx) {
            LocalSpace self = this.asLocalSpace();
            res = Impl.isl.isl_local_space_domain(Impl.isl.isl_local_space_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new LocalSpace(this.ctx, res);
    }
    public LocalSpace range() {
        LocalSpace.Ptr res;
        synchronized(this.ctx) {
            LocalSpace self = this.asLocalSpace();
            res = Impl.isl.isl_local_space_range(Impl.isl.isl_local_space_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new LocalSpace(this.ctx, res);
    }
    public LocalSpace fromDomain() {
        LocalSpace.Ptr res;
        synchronized(this.ctx) {
            LocalSpace self = this.asLocalSpace();
            res = Impl.isl.isl_local_space_from_domain(Impl.isl.isl_local_space_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new LocalSpace(this.ctx, res);
    }
    public LocalSpace addDims(DimType type, int n) {
        LocalSpace.Ptr res;
        synchronized(this.ctx) {
            LocalSpace self = this.asLocalSpace();
            assert n >= 0;
            res = Impl.isl.isl_local_space_add_dims(Impl.isl.isl_local_space_copy(self.getPtr()), type.value, n);
            Context.checkError(this.ctx);
        }
        return new LocalSpace(this.ctx, res);
    }
    public LocalSpace dropDims(DimType type, int first, int n) {
        LocalSpace.Ptr res;
        synchronized(this.ctx) {
            LocalSpace self = this.asLocalSpace();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_local_space_drop_dims(Impl.isl.isl_local_space_copy(self.getPtr()), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return new LocalSpace(this.ctx, res);
    }
    public LocalSpace insertDims(DimType type, int first, int n) {
        LocalSpace.Ptr res;
        synchronized(this.ctx) {
            LocalSpace self = this.asLocalSpace();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_local_space_insert_dims(Impl.isl.isl_local_space_copy(self.getPtr()), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return new LocalSpace(this.ctx, res);
    }
    public LocalSpace intersect(LocalSpace ls2) {
        LocalSpace.Ptr res;
        synchronized(this.ctx) {
            LocalSpace self = this.asLocalSpace();
            ls2 = ls2.asLocalSpace();
            res = Impl.isl.isl_local_space_intersect(Impl.isl.isl_local_space_copy(self.getPtr()), Impl.isl.isl_local_space_copy(ls2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new LocalSpace(this.ctx, res);
    }
    public boolean isEqual(LocalSpace ls2) {
        boolean res;
        synchronized(this.ctx) {
            LocalSpace self = this.asLocalSpace();
            ls2 = ls2.asLocalSpace();
            res = Impl.isl.isl_local_space_is_equal(self.getPtr(), ls2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public BasicMap lifting() {
        BasicMap.Ptr res;
        synchronized(this.ctx) {
            LocalSpace self = this.asLocalSpace();
            res = Impl.isl.isl_local_space_lifting(Impl.isl.isl_local_space_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new BasicMap(this.ctx, res);
    }

}
