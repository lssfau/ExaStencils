package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public class Val {
    public static class Ptr extends PointerType { public Ptr() { super(); } }
    protected Context.Ptr ctx;
    protected PointerType ptr;
    Val() {}
    Val(Context.Ptr ctx, PointerType ptr) {
        assert !ptr.getPointer().equals(Pointer.NULL);
        this.ctx = ctx;
        this.ptr = ptr;
    }
    Val.Ptr getPtr() { return (Val.Ptr)this.ptr; }
    Val.Ptr makePtr0() { 
        Val.Ptr p = (Val.Ptr)this.ptr;
        this.ptr = new Val.Ptr();
        return p;
    }
    public static Val zero() {
        Val that = new Val();
        that.ctx = Context.getDefaultInstance();
        synchronized(that.ctx) {
            that.ptr = Impl.isl.isl_val_zero(that.ctx);
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static Val one() {
        Val that = new Val();
        that.ctx = Context.getDefaultInstance();
        synchronized(that.ctx) {
            that.ptr = Impl.isl.isl_val_one(that.ctx);
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static Val nan() {
        Val that = new Val();
        that.ctx = Context.getDefaultInstance();
        synchronized(that.ctx) {
            that.ptr = Impl.isl.isl_val_nan(that.ctx);
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static Val infty() {
        Val that = new Val();
        that.ctx = Context.getDefaultInstance();
        synchronized(that.ctx) {
            that.ptr = Impl.isl.isl_val_infty(that.ctx);
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static Val neginfty() {
        Val that = new Val();
        that.ctx = Context.getDefaultInstance();
        synchronized(that.ctx) {
            that.ptr = Impl.isl.isl_val_neginfty(that.ctx);
            Context.checkError(that.ctx);
        }
        return that;
    }
    // isl_val_read_from_str
    public Val(String str) {
        this.ctx = Context.getDefaultInstance();
        synchronized(this.ctx) {
            this.ptr = Impl.isl.isl_val_read_from_str(this.ctx, str);
            Context.checkError(this.ctx);
        }
    }
    protected void finalize() {
        synchronized(this.ctx) {
            Impl.isl.isl_val_free(getPtr());
        }
    }
    public String toString() {
        Printer p = Printer.toStr();
        p = p.printVal(this);
        return p.getStr();
    }
    Val asVal() {
        Class clazz = this.getClass();
        if (clazz.equals(Val.class))
            return this;
        try {
            Constructor<Val> c = Val.class.getConstructor(clazz);
            return c.newInstance(this);
        } catch (Exception e) {
           throw new IllegalStateException("Cannot construct Val from " +
               clazz.getName() + " ?", e);
        }
    }
    public Val abs() {
        Val.Ptr res;
        synchronized(this.ctx) {
            Val self = this.asVal();
            res = Impl.isl.isl_val_abs(Impl.isl.isl_val_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Val(this.ctx, res);
    }
    public Val neg() {
        Val.Ptr res;
        synchronized(this.ctx) {
            Val self = this.asVal();
            res = Impl.isl.isl_val_neg(Impl.isl.isl_val_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Val(this.ctx, res);
    }
    public Val floor() {
        Val.Ptr res;
        synchronized(this.ctx) {
            Val self = this.asVal();
            res = Impl.isl.isl_val_floor(Impl.isl.isl_val_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Val(this.ctx, res);
    }
    public Val ceil() {
        Val.Ptr res;
        synchronized(this.ctx) {
            Val self = this.asVal();
            res = Impl.isl.isl_val_ceil(Impl.isl.isl_val_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Val(this.ctx, res);
    }
    public Val trunc() {
        Val.Ptr res;
        synchronized(this.ctx) {
            Val self = this.asVal();
            res = Impl.isl.isl_val_trunc(Impl.isl.isl_val_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Val(this.ctx, res);
    }
    public Val min(Val v2) {
        Val.Ptr res;
        synchronized(this.ctx) {
            Val self = this.asVal();
            v2 = v2.asVal();
            res = Impl.isl.isl_val_min(Impl.isl.isl_val_copy(self.getPtr()), Impl.isl.isl_val_copy(v2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Val(this.ctx, res);
    }
    public Val max(Val v2) {
        Val.Ptr res;
        synchronized(this.ctx) {
            Val self = this.asVal();
            v2 = v2.asVal();
            res = Impl.isl.isl_val_max(Impl.isl.isl_val_copy(self.getPtr()), Impl.isl.isl_val_copy(v2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Val(this.ctx, res);
    }
    public Val add(Val v2) {
        Val.Ptr res;
        synchronized(this.ctx) {
            Val self = this.asVal();
            v2 = v2.asVal();
            res = Impl.isl.isl_val_add(Impl.isl.isl_val_copy(self.getPtr()), Impl.isl.isl_val_copy(v2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Val(this.ctx, res);
    }
    public Val sub(Val v2) {
        Val.Ptr res;
        synchronized(this.ctx) {
            Val self = this.asVal();
            v2 = v2.asVal();
            res = Impl.isl.isl_val_sub(Impl.isl.isl_val_copy(self.getPtr()), Impl.isl.isl_val_copy(v2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Val(this.ctx, res);
    }
    public Val mul(Val v2) {
        Val.Ptr res;
        synchronized(this.ctx) {
            Val self = this.asVal();
            v2 = v2.asVal();
            res = Impl.isl.isl_val_mul(Impl.isl.isl_val_copy(self.getPtr()), Impl.isl.isl_val_copy(v2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Val(this.ctx, res);
    }
    public Val mulUi(int v2) {
        Val.Ptr res;
        synchronized(this.ctx) {
            Val self = this.asVal();
            assert v2 >= 0;
            res = Impl.isl.isl_val_mul_ui(Impl.isl.isl_val_copy(self.getPtr()), v2);
            Context.checkError(this.ctx);
        }
        return new Val(this.ctx, res);
    }
    public Val div(Val v2) {
        Val.Ptr res;
        synchronized(this.ctx) {
            Val self = this.asVal();
            v2 = v2.asVal();
            res = Impl.isl.isl_val_div(Impl.isl.isl_val_copy(self.getPtr()), Impl.isl.isl_val_copy(v2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Val(this.ctx, res);
    }
    public Val mod(Val v2) {
        Val.Ptr res;
        synchronized(this.ctx) {
            Val self = this.asVal();
            v2 = v2.asVal();
            res = Impl.isl.isl_val_mod(Impl.isl.isl_val_copy(self.getPtr()), Impl.isl.isl_val_copy(v2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Val(this.ctx, res);
    }
    public Val gcd(Val v2) {
        Val.Ptr res;
        synchronized(this.ctx) {
            Val self = this.asVal();
            v2 = v2.asVal();
            res = Impl.isl.isl_val_gcd(Impl.isl.isl_val_copy(self.getPtr()), Impl.isl.isl_val_copy(v2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Val(this.ctx, res);
    }
    public int sgn() {
        int res;
        synchronized(this.ctx) {
            Val self = this.asVal();
            res = Impl.isl.isl_val_sgn(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isZero() {
        boolean res;
        synchronized(this.ctx) {
            Val self = this.asVal();
            res = Impl.isl.isl_val_is_zero(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isOne() {
        boolean res;
        synchronized(this.ctx) {
            Val self = this.asVal();
            res = Impl.isl.isl_val_is_one(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isNegone() {
        boolean res;
        synchronized(this.ctx) {
            Val self = this.asVal();
            res = Impl.isl.isl_val_is_negone(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isNonneg() {
        boolean res;
        synchronized(this.ctx) {
            Val self = this.asVal();
            res = Impl.isl.isl_val_is_nonneg(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isNonpos() {
        boolean res;
        synchronized(this.ctx) {
            Val self = this.asVal();
            res = Impl.isl.isl_val_is_nonpos(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isPos() {
        boolean res;
        synchronized(this.ctx) {
            Val self = this.asVal();
            res = Impl.isl.isl_val_is_pos(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isNeg() {
        boolean res;
        synchronized(this.ctx) {
            Val self = this.asVal();
            res = Impl.isl.isl_val_is_neg(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isInt() {
        boolean res;
        synchronized(this.ctx) {
            Val self = this.asVal();
            res = Impl.isl.isl_val_is_int(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isRat() {
        boolean res;
        synchronized(this.ctx) {
            Val self = this.asVal();
            res = Impl.isl.isl_val_is_rat(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isNan() {
        boolean res;
        synchronized(this.ctx) {
            Val self = this.asVal();
            res = Impl.isl.isl_val_is_nan(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isInfty() {
        boolean res;
        synchronized(this.ctx) {
            Val self = this.asVal();
            res = Impl.isl.isl_val_is_infty(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isNeginfty() {
        boolean res;
        synchronized(this.ctx) {
            Val self = this.asVal();
            res = Impl.isl.isl_val_is_neginfty(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public int lt(Val v2) {
        int res;
        synchronized(this.ctx) {
            Val self = this.asVal();
            v2 = v2.asVal();
            res = Impl.isl.isl_val_lt(self.getPtr(), v2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public int le(Val v2) {
        int res;
        synchronized(this.ctx) {
            Val self = this.asVal();
            v2 = v2.asVal();
            res = Impl.isl.isl_val_le(self.getPtr(), v2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public int gt(Val v2) {
        int res;
        synchronized(this.ctx) {
            Val self = this.asVal();
            v2 = v2.asVal();
            res = Impl.isl.isl_val_gt(self.getPtr(), v2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public int ge(Val v2) {
        int res;
        synchronized(this.ctx) {
            Val self = this.asVal();
            v2 = v2.asVal();
            res = Impl.isl.isl_val_ge(self.getPtr(), v2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public int eq(Val v2) {
        int res;
        synchronized(this.ctx) {
            Val self = this.asVal();
            v2 = v2.asVal();
            res = Impl.isl.isl_val_eq(self.getPtr(), v2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public int ne(Val v2) {
        int res;
        synchronized(this.ctx) {
            Val self = this.asVal();
            v2 = v2.asVal();
            res = Impl.isl.isl_val_ne(self.getPtr(), v2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean isDivisibleBy(Val v2) {
        boolean res;
        synchronized(this.ctx) {
            Val self = this.asVal();
            v2 = v2.asVal();
            res = Impl.isl.isl_val_is_divisible_by(self.getPtr(), v2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }

    // Additional convenience methods
    public Val(java.math.BigInteger i) { this(i.toString()); }
    public Val(long i) { this(Long.toString(i)); }
    public java.math.BigInteger getNum() {
        return new java.math.BigInteger(toString().split("/")[0]);
    }
    public java.math.BigInteger getDen() {
       String[] s = toString().split("/");
       return new java.math.BigInteger(s.length == 2 ? s[1] : "1");
    }
}
