import java.util.HashMap;

public class Exists extends Statement {
    public final Statement child;
    public final String varName;

    public Exists(String varName, Statement child) {
        this.varName = varName;
        this.child = child;
    }

    protected boolean compareImpl(String[] patterns, Statement other) {
        if (other instanceof Exists && varName.equals(((Exists)other).varName)) {
            return child.compareWithinContext(patterns, ((Exists)other).child);
        } else {
            return false;
        }
    }

    public Statement substituteTerm(Expression haystack, Expression needle) {
        return new Exists(varName, child.substituteTerm(haystack, needle));
    }

    public Statement substitutePatterns(Statement[] to) {
        return new Exists(varName, child.substitutePatterns(to));
    }

    public boolean estimate(HashMap<String, Boolean> values) {
        // undefined in predicate calculus
        return false;
    }

    public String toString() {
        return "?" + varName + child;
    }
}
