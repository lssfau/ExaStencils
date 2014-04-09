package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public class Constraint {
    public static class Ptr extends PointerType { public Ptr() { super(); } }
    protected Context.Ptr ctx;
    protected PointerType ptr;
    Constraint() {}
    Constraint(Context.Ptr ctx, PointerType ptr) {
        assert !ptr.getPointer().equals(Pointer.NULL);
        this.ctx = ctx;
        this.ptr = ptr;
    }
    Constraint.Ptr getPtr() { return (Constraint.Ptr)this.ptr; }
    Constraint.Ptr makePtr0() { 
        Constraint.Ptr p = (Constraint.Ptr)this.ptr;
        this.ptr = new Constraint.Ptr();
        return p;
    }
    public static Constraint equalityAlloc(LocalSpace ls) {
        Constraint that = new Constraint();
        that.ctx = ls.ctx;
        synchronized(that.ctx) {
            ls = ls.asLocalSpace();
            that.ptr = Impl.isl.isl_equality_alloc(Impl.isl.isl_local_space_copy(ls.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static Constraint inequalityAlloc(LocalSpace ls) {
        Constraint that = new Constraint();
        that.ctx = ls.ctx;
        synchronized(that.ctx) {
            ls = ls.asLocalSpace();
            that.ptr = Impl.isl.isl_inequality_alloc(Impl.isl.isl_local_space_copy(ls.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    protected void finalize() {
        synchronized(this.ctx) {
            Impl.isl.isl_constraint_free(getPtr());
        }
    }
    public String toString() {
        Printer p = Printer.toStr();
        p = p.printConstraint(this);
        return p.getStr();
    }
    Constraint asConstraint() {
        Class clazz = this.getClass();
        if (clazz.equals(Constraint.class))
            return this;
        try {
            Constructor<Constraint> c = Constraint.class.getConstructor(clazz);
            return c.newInstance(this);
        } catch (Exception e) {
           throw new IllegalStateException("Cannot construct Constraint from " +
               clazz.getName() + " ?", e);
        }
    }
    public Space getSpace() {
        Space.Ptr res;
        synchronized(this.ctx) {
            Constraint self = this.asConstraint();
            res = Impl.isl.isl_constraint_get_space(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public LocalSpace getLocalSpace() {
        LocalSpace.Ptr res;
        synchronized(this.ctx) {
            Constraint self = this.asConstraint();
            res = Impl.isl.isl_constraint_get_local_space(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new LocalSpace(this.ctx, res);
    }
    public int dim(DimType type) {
        int res;
        synchronized(this.ctx) {
            Constraint self = this.asConstraint();
            res = Impl.isl.isl_constraint_dim(self.getPtr(), type.value);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean involvesDims(DimType type, int first, int n) {
        boolean res;
        synchronized(this.ctx) {
            Constraint self = this.asConstraint();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_constraint_involves_dims(self.getPtr(), type.value, first, n);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public String getDimName(DimType type, int pos) {
        String res;
        synchronized(this.ctx) {
            Constraint self = this.asConstraint();
            assert pos >= 0;
            res = Impl.isl.isl_constraint_get_dim_name(self.getPtr(), type.value, pos);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Val getConstantVal() {
        Val.Ptr res;
        synchronized(this.ctx) {
            Constraint self = this.asConstraint();
            res = Impl.isl.isl_constraint_get_constant_val(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new Val(this.ctx, res);
    }
    public Val getCoefficientVal(DimType type, int pos) {
        Val.Ptr res;
        synchronized(this.ctx) {
            Constraint self = this.asConstraint();
            res = Impl.isl.isl_constraint_get_coefficient_val(self.getPtr(), type.value, pos);
            Context.checkError(this.ctx);
        }
        return new Val(this.ctx, res);
    }
    public Constraint setConstantVal(Val v) {
        Constraint.Ptr res;
        synchronized(this.ctx) {
            Constraint self = this.asConstraint();
            v = v.asVal();
            res = Impl.isl.isl_constraint_set_constant_val(Impl.isl.isl_constraint_copy(self.getPtr()), Impl.isl.isl_val_copy(v.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Constraint(this.ctx, res);
    }
    public Constraint setCoefficientVal(DimType type, int pos, Val v) {
        Constraint.Ptr res;
        synchronized(this.ctx) {
            Constraint self = this.asConstraint();
            v = v.asVal();
            res = Impl.isl.isl_constraint_set_coefficient_val(Impl.isl.isl_constraint_copy(self.getPtr()), type.value, pos, Impl.isl.isl_val_copy(v.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Constraint(this.ctx, res);
    }
    public Aff getDiv(int pos) {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Constraint self = this.asConstraint();
            res = Impl.isl.isl_constraint_get_div(self.getPtr(), pos);
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public Constraint negate() {
        Constraint.Ptr res;
        synchronized(this.ctx) {
            Constraint self = this.asConstraint();
            res = Impl.isl.isl_constraint_negate(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new Constraint(this.ctx, res);
    }
    public boolean isEquality() {
        boolean res;
        synchronized(this.ctx) {
            Constraint self = this.asConstraint();
            res = Impl.isl.isl_constraint_is_equality(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isDivConstraint() {
        boolean res;
        synchronized(this.ctx) {
            Constraint self = this.asConstraint();
            res = Impl.isl.isl_constraint_is_div_constraint(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isLowerBound(DimType type, int pos) {
        boolean res;
        synchronized(this.ctx) {
            Constraint self = this.asConstraint();
            assert pos >= 0;
            res = Impl.isl.isl_constraint_is_lower_bound(self.getPtr(), type.value, pos);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isUpperBound(DimType type, int pos) {
        boolean res;
        synchronized(this.ctx) {
            Constraint self = this.asConstraint();
            assert pos >= 0;
            res = Impl.isl.isl_constraint_is_upper_bound(self.getPtr(), type.value, pos);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Aff getBound(DimType type, int pos) {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Constraint self = this.asConstraint();
            res = Impl.isl.isl_constraint_get_bound(self.getPtr(), type.value, pos);
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }
    public Aff getAff() {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Constraint self = this.asConstraint();
            res = Impl.isl.isl_constraint_get_aff(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }

}
