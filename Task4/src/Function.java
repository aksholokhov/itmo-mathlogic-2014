import java.util.ArrayList;

public class Function extends Expression {
    public final String name;
    public final ArrayList<Expression> terms;

    public Function(String name, ArrayList<Expression> terms) {
        this.name = name;
        this.terms = terms;
    }

    public boolean equals(Object other) {
        if (other instanceof Function) {
            Function fOther = (Function) other;
            if (name.equals(fOther.name) && terms.size() == fOther.terms.size()) {
                for (int i = 0; i < terms.size(); i++) {
                    if (!terms.get(i).equals(fOther.terms.get(i))) {
                        return false;
                    }
                }
                return true;
            } else {
                return false;
            }
        } else {
            return false;
        }
    }
    
    protected Expression substituteExpression(Expression where, Expression what) {
        ArrayList<Expression> newTerms = new ArrayList<>();

        for (Expression term : terms) {
            newTerms.add(term.substituteTerm(where, what));
        }

        return new Function(name, newTerms);
    }

    public String toString() {
        String str = name;
        if (terms.size() != 0) {
            str += "(";
            for (int i = 0; i < terms.size(); i++) {
                str += terms.get(i);
                if (i != terms.size() - 1) {
                    str += ", ";
                }
            }
            str += ")";
        }
        return str;
    }

    public int hashCode() {
        return toString().hashCode();
    }
}
