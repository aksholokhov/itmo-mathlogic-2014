public class Disjunction extends BinaryOperation {
    public Disjunction(Statement left, Statement right) {
        super("|", left, right);
    }
    protected boolean calculate(boolean a, boolean b) {
        return a | b;
    }
}
