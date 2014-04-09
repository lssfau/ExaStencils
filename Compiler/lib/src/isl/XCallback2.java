package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public interface XCallback2<ArgTy1,ArgTy2,ExTy extends Exception> {
    void apply(ArgTy1 arg1, ArgTy2 arg2) throws ExTy;
}
