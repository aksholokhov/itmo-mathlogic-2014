import java.util.HashMap;

public class Forall extends Statement {
    public final Statement child;
    public final String varName;

    public Forall(String varName, Statement child) {
        this.varName = varName;
        this.child = child;
    }

    protected boolean compareImpl(String[] patterns, Statement other) {
        if (other instanceof Forall && varName.equals(((Forall)other).varName)) {
            return child.compareWithinContext(patterns, ((Forall)other).child);
        } else {
            return false;
        }
    }

    public Statement substituteTerm(Expression where, Expression what) {
        return new Forall(varName, child.substituteTerm(where, what));
    }

    public Statement substitutePatterns(Statement[] to) {
        return new Forall(varName, child.substitutePatterns(to));
    }

    public boolean estimate(HashMap<String, Boolean> values) {
        return false;
    }

    public String toString() {
        return "@" + varName + child;
    }
}
