public class Conjunction extends BinaryOperation {
    public Conjunction(Statement left, Statement right) {
        super("&", left, right);
    }
    protected boolean calculate(boolean a, boolean b) {
        return a & b;
    }
}
