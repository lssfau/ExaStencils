package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public class Term {
    public static class Ptr extends PointerType { public Ptr() { super(); } }
    protected Context.Ptr ctx;
    protected PointerType ptr;
    Term() {}
    Term(Context.Ptr ctx, PointerType ptr) {
        assert !ptr.getPointer().equals(Pointer.NULL);
        this.ctx = ctx;
        this.ptr = ptr;
    }
    Term.Ptr getPtr() { return (Term.Ptr)this.ptr; }
    Term.Ptr makePtr0() { 
        Term.Ptr p = (Term.Ptr)this.ptr;
        this.ptr = new Term.Ptr();
        return p;
    }
    protected void finalize() {
        synchronized(this.ctx) {
            Impl.isl.isl_term_free(getPtr());
        }
    }
    Term asTerm() {
        Class clazz = this.getClass();
        if (clazz.equals(Term.class))
            return this;
        try {
            Constructor<Term> c = Term.class.getConstructor(clazz);
            return c.newInstance(this);
        } catch (Exception e) {
           throw new IllegalStateException("Cannot construct Term from " +
               clazz.getName() + " ?", e);
        }
    }
    public int dim(DimType type) {
        int res;
        synchronized(this.ctx) {
            Term self = this.asTerm();
            res = Impl.isl.isl_term_dim(self.getPtr(), type.value);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public int getExp(DimType type, int pos) {
        int res;
        synchronized(this.ctx) {
            Term self = this.asTerm();
            assert pos >= 0;
            res = Impl.isl.isl_term_get_exp(self.getPtr(), type.value, pos);
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Aff getDiv(int pos) {
        Aff.Ptr res;
        synchronized(this.ctx) {
            Term self = this.asTerm();
            assert pos >= 0;
            res = Impl.isl.isl_term_get_div(self.getPtr(), pos);
            Context.checkError(this.ctx);
        }
        return new Aff(this.ctx, res);
    }

}
