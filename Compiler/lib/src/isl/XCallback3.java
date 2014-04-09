package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public interface XCallback3<ArgTy1,ArgTy2,ArgTy3,ExTy extends Exception> {
    void apply(ArgTy1 arg1, ArgTy2 arg2, ArgTy3 arg3) throws ExTy;
}
