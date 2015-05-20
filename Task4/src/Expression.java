import java.util.Set;
import java.util.HashSet;

public abstract class Expression {
    public Expression substituteTerm(Expression pool, Expression elem) {
        if (equals(pool)) {
            return elem;
        } else {
            return substituteExpression(pool, elem);
        }
    }

    public Set<String> getAllVariables() {
        Set<String> vars = new HashSet<>();

        if (this instanceof Function) {
            Function ex = (Function) this;
            if (ex.terms.size() == 0) {
                vars.add(ex.name);
            } else {
                for (Expression term : ex.terms) {
                    vars.addAll(term.getAllVariables());
                }
            }
        } else if (this instanceof Product) {
            Product ex = (Product) this;
            vars.addAll(ex.left.getAllVariables());
            vars.addAll(ex.right.getAllVariables());
        } else if (this instanceof Sum) {
            Sum ex = (Sum) this;
            vars.addAll(ex.left.getAllVariables());
            vars.addAll(ex.right.getAllVariables());
        } else if (this instanceof Apostrophe) {
            Apostrophe ex = (Apostrophe) this;
            vars.addAll(ex.child.getAllVariables());
        }

        return vars;
    }

    protected abstract Expression substituteExpression(Expression where, Expression what);

}
