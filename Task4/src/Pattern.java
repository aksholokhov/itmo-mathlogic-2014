import java.util.HashMap;

public class Pattern extends Statement {
    public static final int MAX_PATTERN_COUNT = 10;

    public final int patternId;

    public Pattern(int patternId) {
        this.patternId = patternId;
    }

    protected boolean compareImpl(String[] patterns, Statement other) {
        return other instanceof Pattern && patternId == ((Pattern) other).patternId;
    }

    public Statement substituteTerm(Expression where, Expression what) {
        return this;
    }

    public Statement substitutePatterns(Statement[] to) {
        if (to[patternId] != null) {
            return to[patternId];
        } else {
            return this;
        }
    }

    public boolean estimate(HashMap<String, Boolean> values) {
        return true;
    }

    public String toString() {
        return "$" + patternId;
    }
}
