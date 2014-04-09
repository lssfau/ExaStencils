package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public class Point {
    public static class Ptr extends PointerType { public Ptr() { super(); } }
    protected Context.Ptr ctx;
    protected PointerType ptr;
    Point() {}
    Point(Context.Ptr ctx, PointerType ptr) {
        assert !ptr.getPointer().equals(Pointer.NULL);
        this.ctx = ctx;
        this.ptr = ptr;
    }
    Point.Ptr getPtr() { return (Point.Ptr)this.ptr; }
    Point.Ptr makePtr0() { 
        Point.Ptr p = (Point.Ptr)this.ptr;
        this.ptr = new Point.Ptr();
        return p;
    }
    public static Point zero(Space dim) {
        Point that = new Point();
        that.ctx = dim.ctx;
        synchronized(that.ctx) {
            dim = dim.asSpace();
            that.ptr = Impl.isl.isl_point_zero(Impl.isl.isl_space_copy(dim.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static Point void_(Space dim) {
        Point that = new Point();
        that.ctx = dim.ctx;
        synchronized(that.ctx) {
            dim = dim.asSpace();
            that.ptr = Impl.isl.isl_point_void(Impl.isl.isl_space_copy(dim.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    protected void finalize() {
        synchronized(this.ctx) {
            Impl.isl.isl_point_free(getPtr());
        }
    }
    public String toString() {
        Printer p = Printer.toStr();
        p = p.printPoint(this);
        return p.getStr();
    }
    Point asPoint() {
        Class clazz = this.getClass();
        if (clazz.equals(Point.class))
            return this;
        try {
            Constructor<Point> c = Point.class.getConstructor(clazz);
            return c.newInstance(this);
        } catch (Exception e) {
           throw new IllegalStateException("Cannot construct Point from " +
               clazz.getName() + " ?", e);
        }
    }
    public Space getSpace() {
        Space.Ptr res;
        synchronized(this.ctx) {
            Point self = this.asPoint();
            res = Impl.isl.isl_point_get_space(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new Space(this.ctx, res);
    }
    public Val getCoordinateVal(DimType type, int pos) {
        Val.Ptr res;
        synchronized(this.ctx) {
            Point self = this.asPoint();
            res = Impl.isl.isl_point_get_coordinate_val(self.getPtr(), type.value, pos);
            Context.checkError(this.ctx);
        }
        return new Val(this.ctx, res);
    }
    public Point setCoordinateVal(DimType type, int pos, Val v) {
        Point.Ptr res;
        synchronized(this.ctx) {
            Point self = this.asPoint();
            v = v.asVal();
            res = Impl.isl.isl_point_set_coordinate_val(Impl.isl.isl_point_copy(self.getPtr()), type.value, pos, Impl.isl.isl_val_copy(v.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Point(this.ctx, res);
    }
    public boolean isVoid() {
        boolean res;
        synchronized(this.ctx) {
            Point self = this.asPoint();
            res = Impl.isl.isl_point_is_void(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }

}
