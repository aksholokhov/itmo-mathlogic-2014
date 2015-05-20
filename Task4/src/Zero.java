public class Zero extends Expression {
    public Zero() {
    }

    public boolean equals(Object other) {
        return other instanceof Zero;
    }

    protected Expression substituteExpression(Expression where, Expression what) {
        return this;
    }

    public String toString() {
        return "0";
    }

    public String toPolishString() {
        return "0";
    }

    public int hashCode() {
        return toPolishString().hashCode();
    }
}
