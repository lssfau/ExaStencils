package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public enum AstNodeType {
    NodeBlock(3),
    NodeError(-1),
    NodeFor(1),
    NodeIf(2),
    NodeUser(4),
    ;
    int value;
    AstNodeType(int value) { this.value = value; }
}
