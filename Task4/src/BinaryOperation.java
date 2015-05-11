import java.util.HashMap;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

public abstract class BinaryOperation extends Statement {
    private Constructor<?> defaultCtor = null;

    public final String opCode;
    public final Statement left;
    public final Statement right;

    protected BinaryOperation(String opCode, Statement left, Statement right) {
        try {
            defaultCtor = getClass().getConstructor(Statement.class, Statement.class);
        } catch (NoSuchMethodException ignore) {
        }

        this.opCode = opCode;
        this.left = left;
        this.right = right;
    }
    
    protected boolean compareImpl(String[] patterns, Statement other) {
        if (other instanceof BinaryOperation) {
            BinaryOperation bother = (BinaryOperation)other;
            return opCode.equals(bother.opCode)
                && left.compareWithinContext(patterns, bother.left)
                && right.compareWithinContext(patterns, bother.right);
        } else {
            return false;
        }
    }

    public Statement substituteTerm(Expression haystack, Expression needle) {
        Statement result = null;
        try {
            result = (Statement) defaultCtor.newInstance(left.substituteTerm(haystack, needle), right.substituteTerm(haystack, needle));
        } catch (InstantiationException ignore) {
        } catch (IllegalAccessException ignore) {
        } catch (InvocationTargetException ignore) {
        }
        return result;
    }

    public Statement substitutePatterns(Statement[] to) {
        Statement result = null;
        try {
            result = (Statement) defaultCtor.newInstance(left.substitutePatterns(to), right.substitutePatterns(to));
        } catch (InstantiationException ignore) {
        } catch (IllegalAccessException ignore) {
        } catch (InvocationTargetException ignore) {
        }
        return result;
    }

    public boolean estimate(HashMap<String, Boolean> values) {
        return calculate(left.estimate(values), right.estimate(values));
    }

    public String toString() {
        return "(" + left + " " + opCode + " " + right + ")";
    }

    protected abstract boolean calculate(boolean a, boolean b);
}
