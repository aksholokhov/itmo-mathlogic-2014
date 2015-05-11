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

    protected Expression substituteExpression(Expression haystack, Expression needle) {
        return new Sum(left.substituteTerm(haystack, needle), right.substituteTerm(haystack, needle));
    }

    public String toString() {
        return "(" + left + " + " + right + ")";
    }

    public int hashCode() {
        return toString().hashCode();
    }
}
