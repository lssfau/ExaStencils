package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public enum AstOpType {
    OpAccess(22),
    OpAdd(7),
    OpAnd(0),
    OpAndThen(1),
    OpCall(21),
    OpCond(14),
    OpDiv(10),
    OpEq(16),
    OpError(-1),
    OpFdivQ(11),
    OpGe(19),
    OpGt(20),
    OpLe(17),
    OpLt(18),
    OpMax(4),
    OpMember(23),
    OpMin(5),
    OpMinus(6),
    OpMul(9),
    OpOr(2),
    OpOrElse(3),
    OpPdivQ(12),
    OpPdivR(13),
    OpSelect(15),
    OpSub(8),
    ;
    int value;
    AstOpType(int value) { this.value = value; }
}
