public class Product extends Expression {
    public final Expression left;
    public final Expression right;

    public Product(Expression left, Expression right) {
        this.left = left;
        this.right = right;
    }

    public boolean equals(Object other) {
        if (other instanceof Product) {
            Product pOther = (Product) other;
            return left.equals(pOther.left) && right.equals(pOther.right);
        } else {
            return false;
        }
    }

    protected Expression substituteExpression(Expression where, Expression what) {
        return new Product(left.substituteTerm(where, what), right.substituteTerm(where, what));
    }

    public String toString() {
        return "(" + left + " * " + right + ")";
    }

    public String toPolishString() {
        return "*" + left + right;
    }

    public int hashCode() {
        return toPolishString().hashCode();
    }
}
