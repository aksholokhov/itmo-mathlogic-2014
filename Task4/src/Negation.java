import java.util.HashMap;

public class Negation extends Statement {
    public final Statement child;

    public Negation(Statement child) {
        this.child = child;
    }

    protected boolean compareImpl(String[] patterns, Statement other) {
        if (other instanceof Negation) {
            return child.compareWithinContext(patterns, ((Negation)other).child);
        } else {
            return false;
        }
    }

    public Statement substitutePatterns(Statement[] to) {
        return new Negation(child.substitutePatterns(to));
    }

    public Statement substituteTerm(Expression where, Expression what) {
        return new Negation(child.substituteTerm(where, what));
    }

    public boolean estimate(HashMap<String, Boolean> values) {
        return !child.estimate(values);
    }

    public String toString() {
        return "!(" + child + ")";
    }
}
