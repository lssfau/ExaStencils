package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public class Printer {
    public static class Ptr extends PointerType { public Ptr() { super(); } }
    protected Context.Ptr ctx;
    protected PointerType ptr;
    Printer() {}
    Printer(Context.Ptr ctx, PointerType ptr) {
        assert !ptr.getPointer().equals(Pointer.NULL);
        this.ctx = ctx;
        this.ptr = ptr;
    }
    Printer.Ptr getPtr() { return (Printer.Ptr)this.ptr; }
    Printer.Ptr makePtr0() { 
        Printer.Ptr p = (Printer.Ptr)this.ptr;
        this.ptr = new Printer.Ptr();
        return p;
    }
    public static Printer toStr() {
        Printer that = new Printer();
        that.ctx = Context.getDefaultInstance();
        synchronized(that.ctx) {
            that.ptr = Impl.isl.isl_printer_to_str(that.ctx);
            Context.checkError(that.ctx);
        }
        return that;
    }
    protected void finalize() {
        synchronized(this.ctx) {
            Impl.isl.isl_printer_free(getPtr());
        }
    }
    Printer asPrinter() {
        Class clazz = this.getClass();
        if (clazz.equals(Printer.class))
            return this;
        try {
            Constructor<Printer> c = Printer.class.getConstructor(clazz);
            return c.newInstance(this);
        } catch (Exception e) {
           throw new IllegalStateException("Cannot construct Printer from " +
               clazz.getName() + " ?", e);
        }
    }
    public String getStr() {
        String res;
        synchronized(this.ctx) {
            Printer self = this.asPrinter();
            res = Impl.isl.isl_printer_get_str(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Printer printId(Id id) {
        Printer.Ptr res;
        synchronized(this.ctx) {
            Printer self = this.asPrinter();
            id = id.asId();
            res = Impl.isl.isl_printer_print_id(self.makePtr0(), id.getPtr());
            Context.checkError(this.ctx);
        }
        return new Printer(this.ctx, res);
    }
    public Printer printSpace(Space dim) {
        Printer.Ptr res;
        synchronized(this.ctx) {
            Printer self = this.asPrinter();
            dim = dim.asSpace();
            res = Impl.isl.isl_printer_print_space(self.makePtr0(), dim.getPtr());
            Context.checkError(this.ctx);
        }
        return new Printer(this.ctx, res);
    }
    public Printer printLocalSpace(LocalSpace ls) {
        Printer.Ptr res;
        synchronized(this.ctx) {
            Printer self = this.asPrinter();
            ls = ls.asLocalSpace();
            res = Impl.isl.isl_printer_print_local_space(self.makePtr0(), ls.getPtr());
            Context.checkError(this.ctx);
        }
        return new Printer(this.ctx, res);
    }
    public Printer printConstraint(Constraint c) {
        Printer.Ptr res;
        synchronized(this.ctx) {
            Printer self = this.asPrinter();
            c = c.asConstraint();
            res = Impl.isl.isl_printer_print_constraint(self.makePtr0(), c.getPtr());
            Context.checkError(this.ctx);
        }
        return new Printer(this.ctx, res);
    }
    public Printer printPoint(Point pnt) {
        Printer.Ptr res;
        synchronized(this.ctx) {
            Printer self = this.asPrinter();
            pnt = pnt.asPoint();
            res = Impl.isl.isl_printer_print_point(self.makePtr0(), pnt.getPtr());
            Context.checkError(this.ctx);
        }
        return new Printer(this.ctx, res);
    }
    public Printer printPwMultiAff(PwMultiAff pma) {
        Printer.Ptr res;
        synchronized(this.ctx) {
            Printer self = this.asPrinter();
            pma = pma.asPwMultiAff();
            res = Impl.isl.isl_printer_print_pw_multi_aff(self.makePtr0(), pma.getPtr());
            Context.checkError(this.ctx);
        }
        return new Printer(this.ctx, res);
    }
    public Printer printVal(Val v) {
        Printer.Ptr res;
        synchronized(this.ctx) {
            Printer self = this.asPrinter();
            v = v.asVal();
            res = Impl.isl.isl_printer_print_val(self.makePtr0(), v.getPtr());
            Context.checkError(this.ctx);
        }
        return new Printer(this.ctx, res);
    }
    public Printer printAff(Aff aff) {
        Printer.Ptr res;
        synchronized(this.ctx) {
            Printer self = this.asPrinter();
            aff = aff.asAff();
            res = Impl.isl.isl_printer_print_aff(self.makePtr0(), aff.getPtr());
            Context.checkError(this.ctx);
        }
        return new Printer(this.ctx, res);
    }
    public Printer printPwAff(PwAff pwaff) {
        Printer.Ptr res;
        synchronized(this.ctx) {
            Printer self = this.asPrinter();
            pwaff = pwaff.asPwAff();
            res = Impl.isl.isl_printer_print_pw_aff(self.makePtr0(), pwaff.getPtr());
            Context.checkError(this.ctx);
        }
        return new Printer(this.ctx, res);
    }
    public Printer printMultiAff(MultiAff maff) {
        Printer.Ptr res;
        synchronized(this.ctx) {
            Printer self = this.asPrinter();
            maff = maff.asMultiAff();
            res = Impl.isl.isl_printer_print_multi_aff(self.makePtr0(), maff.getPtr());
            Context.checkError(this.ctx);
        }
        return new Printer(this.ctx, res);
    }
    public Printer printUnionPwMultiAff(UnionPwMultiAff upma) {
        Printer.Ptr res;
        synchronized(this.ctx) {
            Printer self = this.asPrinter();
            upma = upma.asUnionPwMultiAff();
            res = Impl.isl.isl_printer_print_union_pw_multi_aff(self.makePtr0(), upma.getPtr());
            Context.checkError(this.ctx);
        }
        return new Printer(this.ctx, res);
    }
    public Printer printMultiPwAff(MultiPwAff mpa) {
        Printer.Ptr res;
        synchronized(this.ctx) {
            Printer self = this.asPrinter();
            mpa = mpa.asMultiPwAff();
            res = Impl.isl.isl_printer_print_multi_pw_aff(self.makePtr0(), mpa.getPtr());
            Context.checkError(this.ctx);
        }
        return new Printer(this.ctx, res);
    }
    public Printer printUnionMap(UnionMap umap) {
        Printer.Ptr res;
        synchronized(this.ctx) {
            Printer self = this.asPrinter();
            umap = umap.asUnionMap();
            res = Impl.isl.isl_printer_print_union_map(self.makePtr0(), umap.getPtr());
            Context.checkError(this.ctx);
        }
        return new Printer(this.ctx, res);
    }
    public Printer printUnionSet(UnionSet uset) {
        Printer.Ptr res;
        synchronized(this.ctx) {
            Printer self = this.asPrinter();
            uset = uset.asUnionSet();
            res = Impl.isl.isl_printer_print_union_set(self.makePtr0(), uset.getPtr());
            Context.checkError(this.ctx);
        }
        return new Printer(this.ctx, res);
    }
    public Printer printVec(Vec vec) {
        Printer.Ptr res;
        synchronized(this.ctx) {
            Printer self = this.asPrinter();
            vec = vec.asVec();
            res = Impl.isl.isl_printer_print_vec(self.makePtr0(), vec.getPtr());
            Context.checkError(this.ctx);
        }
        return new Printer(this.ctx, res);
    }
    public Printer printBasicSet(BasicSet bset) {
        Printer.Ptr res;
        synchronized(this.ctx) {
            Printer self = this.asPrinter();
            bset = bset.asBasicSet();
            res = Impl.isl.isl_printer_print_basic_set(self.makePtr0(), bset.getPtr());
            Context.checkError(this.ctx);
        }
        return new Printer(this.ctx, res);
    }
    public Printer printSet(Set map) {
        Printer.Ptr res;
        synchronized(this.ctx) {
            Printer self = this.asPrinter();
            map = map.asSet();
            res = Impl.isl.isl_printer_print_set(self.makePtr0(), map.getPtr());
            Context.checkError(this.ctx);
        }
        return new Printer(this.ctx, res);
    }
    public Printer printBasicMap(BasicMap bmap) {
        Printer.Ptr res;
        synchronized(this.ctx) {
            Printer self = this.asPrinter();
            bmap = bmap.asBasicMap();
            res = Impl.isl.isl_printer_print_basic_map(self.makePtr0(), bmap.getPtr());
            Context.checkError(this.ctx);
        }
        return new Printer(this.ctx, res);
    }
    public Printer printMap(Map map) {
        Printer.Ptr res;
        synchronized(this.ctx) {
            Printer self = this.asPrinter();
            map = map.asMap();
            res = Impl.isl.isl_printer_print_map(self.makePtr0(), map.getPtr());
            Context.checkError(this.ctx);
        }
        return new Printer(this.ctx, res);
    }
    public Printer printBand(Band band) {
        Printer.Ptr res;
        synchronized(this.ctx) {
            Printer self = this.asPrinter();
            band = band.asBand();
            res = Impl.isl.isl_printer_print_band(self.makePtr0(), band.getPtr());
            Context.checkError(this.ctx);
        }
        return new Printer(this.ctx, res);
    }
    public Printer printSchedule(Schedule schedule) {
        Printer.Ptr res;
        synchronized(this.ctx) {
            Printer self = this.asPrinter();
            schedule = schedule.asSchedule();
            res = Impl.isl.isl_printer_print_schedule(self.makePtr0(), schedule.getPtr());
            Context.checkError(this.ctx);
        }
        return new Printer(this.ctx, res);
    }
    public Printer printQpolynomial(Qpolynomial qp) {
        Printer.Ptr res;
        synchronized(this.ctx) {
            Printer self = this.asPrinter();
            qp = qp.asQpolynomial();
            res = Impl.isl.isl_printer_print_qpolynomial(self.makePtr0(), qp.getPtr());
            Context.checkError(this.ctx);
        }
        return new Printer(this.ctx, res);
    }
    public Printer printPwQpolynomial(PwQpolynomial pwqp) {
        Printer.Ptr res;
        synchronized(this.ctx) {
            Printer self = this.asPrinter();
            pwqp = pwqp.asPwQpolynomial();
            res = Impl.isl.isl_printer_print_pw_qpolynomial(self.makePtr0(), pwqp.getPtr());
            Context.checkError(this.ctx);
        }
        return new Printer(this.ctx, res);
    }
    public Printer printQpolynomialFold(QpolynomialFold fold) {
        Printer.Ptr res;
        synchronized(this.ctx) {
            Printer self = this.asPrinter();
            fold = fold.asQpolynomialFold();
            res = Impl.isl.isl_printer_print_qpolynomial_fold(self.makePtr0(), fold.getPtr());
            Context.checkError(this.ctx);
        }
        return new Printer(this.ctx, res);
    }
    public Printer printPwQpolynomialFold(PwQpolynomialFold pwf) {
        Printer.Ptr res;
        synchronized(this.ctx) {
            Printer self = this.asPrinter();
            pwf = pwf.asPwQpolynomialFold();
            res = Impl.isl.isl_printer_print_pw_qpolynomial_fold(self.makePtr0(), pwf.getPtr());
            Context.checkError(this.ctx);
        }
        return new Printer(this.ctx, res);
    }
    public Printer printUnionPwQpolynomial(UnionPwQpolynomial upwqp) {
        Printer.Ptr res;
        synchronized(this.ctx) {
            Printer self = this.asPrinter();
            upwqp = upwqp.asUnionPwQpolynomial();
            res = Impl.isl.isl_printer_print_union_pw_qpolynomial(self.makePtr0(), upwqp.getPtr());
            Context.checkError(this.ctx);
        }
        return new Printer(this.ctx, res);
    }
    public Printer printUnionPwQpolynomialFold(UnionPwQpolynomialFold upwf) {
        Printer.Ptr res;
        synchronized(this.ctx) {
            Printer self = this.asPrinter();
            upwf = upwf.asUnionPwQpolynomialFold();
            res = Impl.isl.isl_printer_print_union_pw_qpolynomial_fold(self.makePtr0(), upwf.getPtr());
            Context.checkError(this.ctx);
        }
        return new Printer(this.ctx, res);
    }

}
