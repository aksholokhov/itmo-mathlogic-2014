public class Apostrophe extends Expression {
    public final Expression child;

    public Apostrophe(Expression child) {
        this.child = child;
    }

    public boolean equals(Object other) {
        if (other instanceof Apostrophe) {
            return child.equals(((Apostrophe) other).child);
        } else {
            return false;
        }
    }

    protected Expression substituteExpression(Expression where, Expression what) {
        return new Apostrophe(child.substituteTerm(where, what));
    }

    public String toString() {
        return "" + child + "'";
    }

    public int hashCode() {
        return toString().hashCode();
    }
}
