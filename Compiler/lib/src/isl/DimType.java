package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public enum DimType {
    All(5),
    Cst(0),
    Div(4),
    In(2),
    Out(3),
    Param(1),
    Set(3),
    ;
    int value;
    DimType(int value) { this.value = value; }
}
