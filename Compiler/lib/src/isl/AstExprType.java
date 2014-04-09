package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public enum AstExprType {
    ExprError(-1),
    ExprId(1),
    ExprInt(2),
    ExprOp(0),
    ;
    int value;
    AstExprType(int value) { this.value = value; }
}
