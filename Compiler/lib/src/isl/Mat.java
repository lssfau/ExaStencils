package isl;
import isl.*;
import java.lang.reflect.*;
import com.sun.jna.*;
public class Mat {
    public static class Ptr extends PointerType { public Ptr() { super(); } }
    protected Context.Ptr ctx;
    protected PointerType ptr;
    Mat() {}
    Mat(Context.Ptr ctx, PointerType ptr) {
        assert !ptr.getPointer().equals(Pointer.NULL);
        this.ctx = ctx;
        this.ptr = ptr;
    }
    Mat.Ptr getPtr() { return (Mat.Ptr)this.ptr; }
    Mat.Ptr makePtr0() { 
        Mat.Ptr p = (Mat.Ptr)this.ptr;
        this.ptr = new Mat.Ptr();
        return p;
    }
    public static Mat alloc(int n_row, int n_col) {
        Mat that = new Mat();
        that.ctx = Context.getDefaultInstance();
        synchronized(that.ctx) {
            assert n_row >= 0;
            assert n_col >= 0;
            that.ptr = Impl.isl.isl_mat_alloc(that.ctx, n_row, n_col);
            Context.checkError(that.ctx);
        }
        return that;
    }
    public static Mat fromRowVec(Vec vec) {
        Mat that = new Mat();
        that.ctx = vec.ctx;
        synchronized(that.ctx) {
            vec = vec.asVec();
            that.ptr = Impl.isl.isl_mat_from_row_vec(Impl.isl.isl_vec_copy(vec.getPtr()));
            Context.checkError(that.ctx);
        }
        return that;
    }
    protected void finalize() {
        synchronized(this.ctx) {
            Impl.isl.isl_mat_free(getPtr());
        }
    }
    Mat asMat() {
        Class clazz = this.getClass();
        if (clazz.equals(Mat.class))
            return this;
        try {
            Constructor<Mat> c = Mat.class.getConstructor(clazz);
            return c.newInstance(this);
        } catch (Exception e) {
           throw new IllegalStateException("Cannot construct Mat from " +
               clazz.getName() + " ?", e);
        }
    }
    public int rows() {
        int res;
        synchronized(this.ctx) {
            Mat self = this.asMat();
            res = Impl.isl.isl_mat_rows(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public int cols() {
        int res;
        synchronized(this.ctx) {
            Mat self = this.asMat();
            res = Impl.isl.isl_mat_cols(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public Val getElementVal(int row, int col) {
        Val.Ptr res;
        synchronized(this.ctx) {
            Mat self = this.asMat();
            res = Impl.isl.isl_mat_get_element_val(self.getPtr(), row, col);
            Context.checkError(this.ctx);
        }
        return new Val(this.ctx, res);
    }
    public Mat setElementVal(int row, int col, Val v) {
        Mat.Ptr res;
        synchronized(this.ctx) {
            Mat self = this.asMat();
            v = v.asVal();
            res = Impl.isl.isl_mat_set_element_val(Impl.isl.isl_mat_copy(self.getPtr()), row, col, Impl.isl.isl_val_copy(v.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Mat(this.ctx, res);
    }
    public Mat rightInverse() {
        Mat.Ptr res;
        synchronized(this.ctx) {
            Mat self = this.asMat();
            res = Impl.isl.isl_mat_right_inverse(Impl.isl.isl_mat_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Mat(this.ctx, res);
    }
    public Mat rightKernel() {
        Mat.Ptr res;
        synchronized(this.ctx) {
            Mat self = this.asMat();
            res = Impl.isl.isl_mat_right_kernel(Impl.isl.isl_mat_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Mat(this.ctx, res);
    }
    public Mat normalize() {
        Mat.Ptr res;
        synchronized(this.ctx) {
            Mat self = this.asMat();
            res = Impl.isl.isl_mat_normalize(Impl.isl.isl_mat_copy(self.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Mat(this.ctx, res);
    }
    public Mat normalizeRow(int row) {
        Mat.Ptr res;
        synchronized(this.ctx) {
            Mat self = this.asMat();
            res = Impl.isl.isl_mat_normalize_row(Impl.isl.isl_mat_copy(self.getPtr()), row);
            Context.checkError(this.ctx);
        }
        return new Mat(this.ctx, res);
    }
    public Mat insertCols(int col, int n) {
        Mat.Ptr res;
        synchronized(this.ctx) {
            Mat self = this.asMat();
            assert col >= 0;
            assert n >= 0;
            res = Impl.isl.isl_mat_insert_cols(Impl.isl.isl_mat_copy(self.getPtr()), col, n);
            Context.checkError(this.ctx);
        }
        return new Mat(this.ctx, res);
    }
    public Mat insertRows(int row, int n) {
        Mat.Ptr res;
        synchronized(this.ctx) {
            Mat self = this.asMat();
            assert row >= 0;
            assert n >= 0;
            res = Impl.isl.isl_mat_insert_rows(Impl.isl.isl_mat_copy(self.getPtr()), row, n);
            Context.checkError(this.ctx);
        }
        return new Mat(this.ctx, res);
    }
    public Mat moveCols(int dst_col, int src_col, int n) {
        Mat.Ptr res;
        synchronized(this.ctx) {
            Mat self = this.asMat();
            assert dst_col >= 0;
            assert src_col >= 0;
            assert n >= 0;
            res = Impl.isl.isl_mat_move_cols(Impl.isl.isl_mat_copy(self.getPtr()), dst_col, src_col, n);
            Context.checkError(this.ctx);
        }
        return new Mat(this.ctx, res);
    }
    public Mat addRows(int n) {
        Mat.Ptr res;
        synchronized(this.ctx) {
            Mat self = this.asMat();
            assert n >= 0;
            res = Impl.isl.isl_mat_add_rows(Impl.isl.isl_mat_copy(self.getPtr()), n);
            Context.checkError(this.ctx);
        }
        return new Mat(this.ctx, res);
    }
    public Mat insertZeroCols(int first, int n) {
        Mat.Ptr res;
        synchronized(this.ctx) {
            Mat self = this.asMat();
            assert first >= 0;
            assert n >= 0;
            res = Impl.isl.isl_mat_insert_zero_cols(Impl.isl.isl_mat_copy(self.getPtr()), first, n);
            Context.checkError(this.ctx);
        }
        return new Mat(this.ctx, res);
    }
    public Mat addZeroCols(int n) {
        Mat.Ptr res;
        synchronized(this.ctx) {
            Mat self = this.asMat();
            assert n >= 0;
            res = Impl.isl.isl_mat_add_zero_cols(Impl.isl.isl_mat_copy(self.getPtr()), n);
            Context.checkError(this.ctx);
        }
        return new Mat(this.ctx, res);
    }
    public Mat insertZeroRows(int row, int n) {
        Mat.Ptr res;
        synchronized(this.ctx) {
            Mat self = this.asMat();
            assert row >= 0;
            assert n >= 0;
            res = Impl.isl.isl_mat_insert_zero_rows(Impl.isl.isl_mat_copy(self.getPtr()), row, n);
            Context.checkError(this.ctx);
        }
        return new Mat(this.ctx, res);
    }
    public Mat addZeroRows(int n) {
        Mat.Ptr res;
        synchronized(this.ctx) {
            Mat self = this.asMat();
            assert n >= 0;
            res = Impl.isl.isl_mat_add_zero_rows(Impl.isl.isl_mat_copy(self.getPtr()), n);
            Context.checkError(this.ctx);
        }
        return new Mat(this.ctx, res);
    }
    public Mat concat(Mat bot) {
        Mat.Ptr res;
        synchronized(this.ctx) {
            Mat self = this.asMat();
            bot = bot.asMat();
            res = Impl.isl.isl_mat_concat(Impl.isl.isl_mat_copy(self.getPtr()), Impl.isl.isl_mat_copy(bot.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Mat(this.ctx, res);
    }
    public Mat vecConcat(Vec bot) {
        Mat.Ptr res;
        synchronized(this.ctx) {
            Mat self = this.asMat();
            bot = bot.asVec();
            res = Impl.isl.isl_mat_vec_concat(Impl.isl.isl_mat_copy(self.getPtr()), Impl.isl.isl_vec_copy(bot.getPtr()));
            Context.checkError(this.ctx);
        }
        return new Mat(this.ctx, res);
    }
    public boolean isEqual(Mat mat2) {
        boolean res;
        synchronized(this.ctx) {
            Mat self = this.asMat();
            mat2 = mat2.asMat();
            res = Impl.isl.isl_mat_is_equal(self.getPtr(), mat2.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }
    public int initialNonZeroCols() {
        int res;
        synchronized(this.ctx) {
            Mat self = this.asMat();
            res = Impl.isl.isl_mat_initial_non_zero_cols(self.getPtr());
            Context.checkError(this.ctx);
        }
        return res;
    }

}
