public class Sum extends Expression {
    public final Expression left;
    public final Expression right;

    public Sum(Expression left, Expression right) {
        this.left = left;
        this.right = right;
    }

    public boolean equals(Object other) {
        if (other instanceof Sum) {
            Sum sOther = (Sum) other;
            return left.equals(sOther.left) && right.equals(sOther.right);
        } else {
            return false;
        }
    }

    protected Expression substituteExpression(Expression where, Expression what) {
        return new Sum(left.substituteTerm(where, what), right.substituteTerm(where, what));
    }

    public String toString() {
        return "(" + left + " + " + right + ")";
    }

    public int hashCode() {
        return toString().hashCode();
    }
}
