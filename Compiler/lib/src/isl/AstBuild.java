package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public class AstBuild {
    public static class Ptr extends PointerType { public Ptr() { super(); } }
    protected Context.Ptr ctx;
    protected PointerType ptr;
    AstBuild() {}
    AstBuild(Context.Ptr ctx, PointerType ptr) {
        assert !ptr.getPointer().equals(Pointer.NULL);
        this.ctx = ctx;
        this.ptr = ptr;
    }
    AstBuild.Ptr getPtr() { return (AstBuild.Ptr)this.ptr; }
    AstBuild.Ptr makePtr0() { 
        AstBuild.Ptr p = (AstBuild.Ptr)this.ptr;
        this.ptr = new AstBuild.Ptr();
        return p;
    }
    public static AstBuild fromContext(Set set) {
        AstBuild that = new AstBuild();
        that.ctx = set.ctx;
        synchronized(that.ctx) {
            set = set.asSet();
            that.ptr = Impl.isl.isl_ast_build_from_context(Impl.isl.isl_set_copy(set.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    protected void finalize() {
        synchronized(this.ctx) {
            Impl.isl.isl_ast_build_free(getPtr());
        }
    }
    AstBuild asAstBuild() {
        Class clazz = this.getClass();
        if (clazz.equals(AstBuild.class))
            return this;
        try {
            Constructor<AstBuild> c = AstBuild.class.getConstructor(clazz);
            return c.newInstance(this);
        } catch (Exception e) {
           throw new IllegalStateException("Cannot construct AstBuild from " +
               clazz.getName() + " ?", e);
        }
    }
    public AstNode astFromSchedule(UnionMap schedule) {
        AstNode.Ptr res;
        synchronized(this.ctx) {
            AstBuild self = this.asAstBuild();
            schedule = schedule.asUnionMap();
            res = Impl.isl.isl_ast_build_ast_from_schedule(self.getPtr(), Impl.isl.isl_union_map_copy(schedule.getPtr()));
            Context.checkError(this.ctx);
        }
        return new AstNode(this.ctx, res);
    }

}
