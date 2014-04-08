package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public class QpolynomialFold {
    public static class Ptr extends PointerType { public Ptr() { super(); } }
    protected Context.Ptr ctx;
    protected PointerType ptr;
    QpolynomialFold() {}
    QpolynomialFold(Context.Ptr ctx, PointerType ptr) {
        assert !ptr.getPointer().equals(Pointer.NULL);
        this.ctx = ctx;
        this.ptr = ptr;
    }
    QpolynomialFold.Ptr getPtr() { return (QpolynomialFold.Ptr)this.ptr; }
    QpolynomialFold.Ptr makePtr0() { 
        QpolynomialFold.Ptr p = (QpolynomialFold.Ptr)this.ptr;
        this.ptr = new QpolynomialFold.Ptr();
        return p;
    }
    public static QpolynomialFold empty(Fold type, Space dim) {
        QpolynomialFold that = new QpolynomialFold();
        that.ctx = dim.ctx;
        synchronized(that.ctx) {
            dim = dim.asSpace();
            that.ptr = Impl.isl.isl_qpolynomial_fold_empty(type.value, Impl.isl.isl_space_copy(dim.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static QpolynomialFold alloc(Fold type, Qpolynomial qp) {
        QpolynomialFold that = new QpolynomialFold();
        that.ctx = qp.ctx;
        synchronized(that.ctx) {
            qp = qp.asQpolynomial();
            that.ptr = Impl.isl.isl_qpolynomial_fold_alloc(type.value, Impl.isl.isl_qpolynomial_copy(qp.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    protected void finalize() {
        synchronized(this.ctx) {
            Impl.isl.isl_qpolynomial_fold_free(getPtr());
        }
    }
    public String toString() {
        Printer p = Printer.toStr();
        p = p.printQpolynomialFold(this);
        return p.getStr();
    }
    QpolynomialFold asQpolynomialFold() {
        Class clazz = this.getClass();
        if (clazz.equals(QpolynomialFold.class))
            return this;
        try {
            Constructor<QpolynomialFold> c = QpolynomialFold.class.getConstructor(clazz);
            return c.newInstance(this);
        } catch (Exception e) {
           throw new IllegalStateException("Cannot construct QpolynomialFold from " +
               clazz.getName() + " ?", e);
        }
    }
    public Fold getType() {
        int res;
        synchronized(this.ctx) {
            QpolynomialFold self = this.asQpolynomialFold();
            res = Impl.isl.isl_qpolynomial_fold_get_type(self.getPtr());
            Context.checkError(this.ctx);
        }
        switch(res) {
        case 2: return Fold.List;
        case 1: return Fold.Max;
        case 0: return Fold.Min;
        default: throw new IllegalStateException("No enum constant in Fold for value " + (res) + "?");
        }
    }
    public boolean isEmpty() {
        boolean res;
        synchronized(this.ctx) {
            QpolynomialFold self = this.asQpolynomialFold();
            res = Impl.isl.isl_qpolynomial_fold_is_empty(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public boolean plainIsEqual(QpolynomialFold fold2) {
        boolean res;
        synchronized(this.ctx) {
            QpolynomialFold self = this.asQpolynomialFold();
            fold2 = fold2.asQpolynomialFold();
            res = Impl.isl.isl_qpolynomial_fold_plain_is_equal(self.getPtr(), fold2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Space getSpace() {
        Space.Ptr res;
        synchronized(this.ctx) {
            QpolynomialFold self = this.asQpolynomialFold();
            res = Impl.isl.isl_qpolynomial_fold_get_space(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public QpolynomialFold fold(QpolynomialFold fold2) {
        QpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            QpolynomialFold self = this.asQpolynomialFold();
            fold2 = fold2.asQpolynomialFold();
            res = Impl.isl.isl_qpolynomial_fold_fold(Impl.isl.isl_qpolynomial_fold_copy(self.getPtr()), Impl.isl.isl_qpolynomial_fold_copy(fold2.getPtr()));
            Context.checkError(this.ctx);
        }
        return new QpolynomialFold(this.ctx, res);
    }
    public QpolynomialFold scaleVal(Val v) {
        QpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            QpolynomialFold self = this.asQpolynomialFold();
            v = v.asVal();
            res = Impl.isl.isl_qpolynomial_fold_scale_val(Impl.isl.isl_qpolynomial_fold_copy(self.getPtr()), Impl.isl.isl_val_copy(v.getPtr()));
            Context.checkError(this.ctx);
        }
        return new QpolynomialFold(this.ctx, res);
    }
    public QpolynomialFold moveDims(DimType dst_type, int dst_pos, DimType src_type, int src_pos, int n) {
        QpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            QpolynomialFold self = this.asQpolynomialFold();
            assert dst_pos >= 0;
            assert src_pos >= 0;
            assert n >= 0;
            res = Impl.isl.isl_qpolynomial_fold_move_dims(Impl.isl.isl_qpolynomial_fold_copy(self.getPtr()), dst_type.value, dst_pos, src_type.value, src_pos, n);
            Context.checkError(this.ctx);
        }
        return new QpolynomialFold(this.ctx, res);
    }
    public QpolynomialFold substitute(DimType type, int first, int n, Qpolynomial[] subs) {
        QpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            QpolynomialFold self = this.asQpolynomialFold();
            assert first >= 0;
            assert n >= 0;
            assert subs == null || subs.length == 1;
        Qpolynomial.Ptr[] _subs = subs != null ? new Qpolynomial.Ptr[1] : null;
            res = Impl.isl.isl_qpolynomial_fold_substitute(Impl.isl.isl_qpolynomial_fold_copy(self.getPtr()), type.value, first, n, _subs);
        if (subs != null)
            subs[0] = new Qpolynomial(this.ctx, _subs[0]);
            Context.checkError(this.ctx);
        }
        return new QpolynomialFold(this.ctx, res);
    }
    public Val eval(Point pnt) {
        Val.Ptr res;
        synchronized(this.ctx) {
            QpolynomialFold self = this.asQpolynomialFold();
            pnt = pnt.asPoint();
            res = Impl.isl.isl_qpolynomial_fold_eval(Impl.isl.isl_qpolynomial_fold_copy(self.getPtr()), Impl.isl.isl_point_copy(pnt.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Val(this.ctx, res);
    }
    public QpolynomialFold gistParams(Set context) {
        QpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            QpolynomialFold self = this.asQpolynomialFold();
            context = context.asSet();
            res = Impl.isl.isl_qpolynomial_fold_gist_params(Impl.isl.isl_qpolynomial_fold_copy(self.getPtr()), Impl.isl.isl_set_copy(context.getPtr()));
            Context.checkError(this.ctx);
        }
        return new QpolynomialFold(this.ctx, res);
    }
    public QpolynomialFold gist(Set context) {
        QpolynomialFold.Ptr res;
        synchronized(this.ctx) {
            QpolynomialFold self = this.asQpolynomialFold();
            context = context.asSet();
            res = Impl.isl.isl_qpolynomial_fold_gist(Impl.isl.isl_qpolynomial_fold_copy(self.getPtr()), Impl.isl.isl_set_copy(context.getPtr()));
            Context.checkError(this.ctx);
        }
        return new QpolynomialFold(this.ctx, res);
    }
    public <ExceptionTy extends Exception> void foreachQpolynomial(final XCallback1<Qpolynomial,ExceptionTy> fn) throws ExceptionTy {
        int res;
        synchronized(this.ctx) {
            QpolynomialFold self = this.asQpolynomialFold();
            final Context.Ptr _ctx = this.ctx;
            final Throwable[] exc_info = new Throwable[1];
            Callback cb = new Callback() {
                public int callback(Qpolynomial.Ptr cb_arg0, Pointer _user) {
                    try {
                    fn.apply(new Qpolynomial(_ctx, cb_arg0));
                    } catch (Throwable e) {
                        exc_info[0] = e;
                        return -1;
                    }
                    return 0;
                }
            };
            res = Impl.isl.isl_qpolynomial_fold_foreach_qpolynomial(self.getPtr(), cb, Pointer.NULL);
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

}
