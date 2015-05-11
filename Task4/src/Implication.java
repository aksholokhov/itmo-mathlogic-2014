public class Implication extends BinaryOperation {
    public Implication(Statement left, Statement right) {
        super("->", left, right);
    }
    protected boolean calculate(boolean a, boolean b) {
        return !a | b;
    }
}
