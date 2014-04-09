package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public class Schedule {
    public static class Ptr extends PointerType { public Ptr() { super(); } }
    protected Context.Ptr ctx;
    protected PointerType ptr;
    Schedule() {}
    Schedule(Context.Ptr ctx, PointerType ptr) {
        assert !ptr.getPointer().equals(Pointer.NULL);
        this.ctx = ctx;
        this.ptr = ptr;
    }
    Schedule.Ptr getPtr() { return (Schedule.Ptr)this.ptr; }
    Schedule.Ptr makePtr0() { 
        Schedule.Ptr p = (Schedule.Ptr)this.ptr;
        this.ptr = new Schedule.Ptr();
        return p;
    }
    protected void finalize() {
        synchronized(this.ctx) {
            Impl.isl.isl_schedule_free(getPtr());
        }
    }
    public String toString() {
        Printer p = Printer.toStr();
        p = p.printSchedule(this);
        return p.getStr();
    }
    Schedule asSchedule() {
        Class clazz = this.getClass();
        if (clazz.equals(Schedule.class))
            return this;
        try {
            Constructor<Schedule> c = Schedule.class.getConstructor(clazz);
            return c.newInstance(this);
        } catch (Exception e) {
           throw new IllegalStateException("Cannot construct Schedule from " +
               clazz.getName() + " ?", e);
        }
    }
    public UnionMap getMap() {
        UnionMap.Ptr res;
        synchronized(this.ctx) {
            Schedule self = this.asSchedule();
            res = Impl.isl.isl_schedule_get_map(self.getPtr());
            Context.checkError(this.ctx);
        }
        return new UnionMap(this.ctx, res);
    }
    public <ExceptionTy extends Exception> void foreachBand(final XCallback1<Band,ExceptionTy> fn) throws ExceptionTy {
        int res;
        synchronized(this.ctx) {
            Schedule self = this.asSchedule();
            final Context.Ptr _ctx = this.ctx;
            final Throwable[] exc_info = new Throwable[1];
            Callback cb = new Callback() {
                public int callback(Band.Ptr cb_arg0, Pointer _user) {
                    try {
                    fn.apply(new Band(_ctx, cb_arg0));
                    } catch (Throwable e) {
                        exc_info[0] = e;
                        return -1;
                    }
                    return 0;
                }
            };
            res = Impl.isl.isl_schedule_foreach_band(self.getPtr(), cb, Pointer.NULL);
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
