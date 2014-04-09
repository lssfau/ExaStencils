package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public enum Fold {
    List(2),
    Max(1),
    Min(0),
    ;
    int value;
    Fold(int value) { this.value = value; }
}
