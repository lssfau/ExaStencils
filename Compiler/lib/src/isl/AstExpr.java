package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public class AstExpr {
    public static class Ptr extends PointerType { public Ptr() { super(); } }
    protected Context.Ptr ctx;
    protected PointerType ptr;
    AstExpr() {}
    AstExpr(Context.Ptr ctx, PointerType ptr) {
        assert !ptr.getPointer().equals(Pointer.NULL);
        this.ctx = ctx;
        this.ptr = ptr;
    }
    AstExpr.Ptr getPtr() { return (AstExpr.Ptr)this.ptr; }
    AstExpr.Ptr makePtr0() { 
        AstExpr.Ptr p = (AstExpr.Ptr)this.ptr;
        this.ptr = new AstExpr.Ptr();
        return p;
    }
    protected void finalize() {
        synchronized(this.ctx) {
            Impl.isl.isl_ast_expr_free(getPtr());
        }
    }
    AstExpr asAstExpr() {
        Class clazz = this.getClass();
        if (clazz.equals(AstExpr.class))
            return this;
        try {
            Constructor<AstExpr> c = AstExpr.class.getConstructor(clazz);
            return c.newInstance(this);
        } catch (Exception e) {
           throw new IllegalStateException("Cannot construct AstExpr from " +
               clazz.getName() + " ?", e);
        }
    }
    public AstExprType getType() {
        int res;
        synchronized(this.ctx) {
            AstExpr self = this.asAstExpr();
            res = Impl.isl.isl_ast_expr_get_type(self.getPtr());
            Context.checkError(this.ctx);
        }
        switch(res) {
        case -1: return AstExprType.ExprError;
        case 1: return AstExprType.ExprId;
        case 2: return AstExprType.ExprInt;
        case 0: return AstExprType.ExprOp;
        default: throw new IllegalStateException("No enum constant in AstExprType for value " + (res) + "?");
        }
    }
    public AstOpType getOpType() {
        int res;
        synchronized(this.ctx) {
            AstExpr self = this.asAstExpr();
            res = Impl.isl.isl_ast_expr_get_op_type(self.getPtr());
            Context.checkError(this.ctx);
        }
        switch(res) {
        case 22: return AstOpType.OpAccess;
        case 7: return AstOpType.OpAdd;
        case 0: return AstOpType.OpAnd;
        case 1: return AstOpType.OpAndThen;
        case 21: return AstOpType.OpCall;
        case 14: return AstOpType.OpCond;
        case 10: return AstOpType.OpDiv;
        case 16: return AstOpType.OpEq;
        case -1: return AstOpType.OpError;
        case 11: return AstOpType.OpFdivQ;
        case 19: return AstOpType.OpGe;
        case 20: return AstOpType.OpGt;
        case 17: return AstOpType.OpLe;
        case 18: return AstOpType.OpLt;
        case 4: return AstOpType.OpMax;
        case 23: return AstOpType.OpMember;
        case 5: return AstOpType.OpMin;
        case 6: return AstOpType.OpMinus;
        case 9: return AstOpType.OpMul;
        case 2: return AstOpType.OpOr;
        case 3: return AstOpType.OpOrElse;
        case 12: return AstOpType.OpPdivQ;
        case 13: return AstOpType.OpPdivR;
        case 15: return AstOpType.OpSelect;
        case 8: return AstOpType.OpSub;
        default: throw new IllegalStateException("No enum constant in AstOpType for value " + (res) + "?");
        }
    }

}
