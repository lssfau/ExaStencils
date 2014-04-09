package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
class Context {
    public static class Ptr extends PointerType { public Ptr() { super(); } }
    private static Ptr defaultInstance = null;
    static Ptr getDefaultInstance() {
        if (defaultInstance == null)
            defaultInstance = Impl.isl.isl_ctx_alloc();
        return defaultInstance;
    }
    static void checkError(Ptr ctx) {
        if (Impl.isl.isl_ctx_last_error(ctx) != 0) { // 0 == isl_error_none
            Impl.isl.isl_ctx_reset_error(ctx);
            throw new IslException("isl error");
         }
     }
}
