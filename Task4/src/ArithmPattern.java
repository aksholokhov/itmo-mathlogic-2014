public class ArithmPattern extends Expression {
    public int patternId;

    public ArithmPattern(int patternId) {
        this.patternId = patternId;
    }

    public boolean equals(Object other) {
        return other instanceof ArithmPattern && patternId == ((ArithmPattern) other).patternId;
    }

    protected Expression substituteExpression(Expression where, Expression what) {
        return this;
    }

    public String toString() {
        return "#" + patternId;
    }

    public int hashCode() {
        return toString().hashCode();
    }
}
