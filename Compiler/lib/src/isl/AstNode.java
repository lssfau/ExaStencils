package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public class AstNode {
    public static class Ptr extends PointerType { public Ptr() { super(); } }
    protected Context.Ptr ctx;
    protected PointerType ptr;
    AstNode() {}
    AstNode(Context.Ptr ctx, PointerType ptr) {
        assert !ptr.getPointer().equals(Pointer.NULL);
        this.ctx = ctx;
        this.ptr = ptr;
    }
    AstNode.Ptr getPtr() { return (AstNode.Ptr)this.ptr; }
    AstNode.Ptr makePtr0() { 
        AstNode.Ptr p = (AstNode.Ptr)this.ptr;
        this.ptr = new AstNode.Ptr();
        return p;
    }
    protected void finalize() {
        synchronized(this.ctx) {
            Impl.isl.isl_ast_node_free(getPtr());
        }
    }
    AstNode asAstNode() {
        Class clazz = this.getClass();
        if (clazz.equals(AstNode.class))
            return this;
        try {
            Constructor<AstNode> c = AstNode.class.getConstructor(clazz);
            return c.newInstance(this);
        } catch (Exception e) {
           throw new IllegalStateException("Cannot construct AstNode from " +
               clazz.getName() + " ?", e);
        }
    }
    public AstNodeType getType() {
        int res;
        synchronized(this.ctx) {
            AstNode self = this.asAstNode();
            res = Impl.isl.isl_ast_node_get_type(self.getPtr());
            Context.checkError(this.ctx);
        }
        switch(res) {
        case 3: return AstNodeType.NodeBlock;
        case -1: return AstNodeType.NodeError;
        case 1: return AstNodeType.NodeFor;
        case 2: return AstNodeType.NodeIf;
        case 4: return AstNodeType.NodeUser;
        default: throw new IllegalStateException("No enum constant in AstNodeType for value " + (res) + "?");
        }
    }

}
