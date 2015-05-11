import java.util.ArrayList;

public class Helper {
    public static ArrayList<Statement> axioms = parseKnownLines(
        "$1->$2->$1",
        "($1->$2)->($1->$2->$3)->($1->$3)",
        "$1->$2->$1&$2",
        "$1&$2->$1",
        "$1&$2->$2",
        "$1->$1|$2",
        "$2->$1|$2",
        "($1->$3)->($2->$3)->($1|$2->$3)",
        "($1->$2)->($1->!$2)->!$1",
        "!!$1->$1"
    );

    public static ArrayList<Statement> arithmAxioms = parseKnownLines(
        "#1=#2->#1'=#2'",
        "#1=#2->#1=#3->#2=#3",
        "#1'=#2'->#1=#2",
        "!#1'=0",
        "#1+#2'=(#1+#2)'",
        "#1+0=#1",
        "#1*0=0",
        "#1*#2'=#1*#2+#1"
    );

    public static ArrayList<Statement> parseLines(String... lines) throws ParserException {
        PredicateParser parser = new PredicateParser();
        ArrayList<Statement> statements = new ArrayList<Statement>(lines.length);

        for (String line : lines) {
            Statement stmt = parser.parse(line);
            statements.add(stmt);
        }

        return statements;
    }

    public static ArrayList<Statement> parseKnownLines(String... lines) {
        try {
            return parseLines(lines);
        } catch (ParserException ignore) {
            // this is unreachable
            return null;
        }
    }

    private static void optimizeRecursively(boolean[] used, ArrayList<Annotator.AnnotatedStatement> statements, int current) {
        Annotator.AnnotatedStatement annStatement = statements.get(current);
        
        used[current] = true;

        if (annStatement instanceof Annotator.ModusPonens) {
            int alpha = ((Annotator.ModusPonens) annStatement).alpha;
            int beta = ((Annotator.ModusPonens) annStatement).beta;
            
            used[alpha] = true;
            used[beta] = true;
            optimizeRecursively(used, statements, alpha);
            optimizeRecursively(used, statements, beta);
        } else if (annStatement instanceof Annotator.InferenceRule) {
            int lineNo = ((Annotator.InferenceRule) annStatement).lineNo;

            used[lineNo] = true;
            optimizeRecursively(used, statements, lineNo);
        }
    }

    public static ArrayList<Statement> optimizeAnnotated(ArrayList<Annotator.AnnotatedStatement> statements) {
        ArrayList<Statement> optStatements = new ArrayList<>();
        boolean[] used = new boolean[statements.size()];

        for (int i = 0; i < statements.size(); i++) {
            if (statements.get(i) instanceof Annotator.Unannotated) {
                used[i] = true;
            }
        }

        optimizeRecursively(used, statements, statements.size() - 1);

        for (int i = 0; i < statements.size(); i++) {
            if (used[i]) {
                optStatements.add(statements.get(i).statement);
            }
        }

        return optStatements;
    }

    public static ArrayList<Annotator.AnnotatedStatement> annotateInContext(ArrayList<Statement> context, ArrayList<Statement> statements) {
        ArrayList<Annotator.AnnotatedStatement> annStatements = new ArrayList<>(statements.size());

        for (Statement stmt : statements) {
            int hypothesisNum = -1;

            for (int i = 0; i < context.size(); i++) {
                if (stmt.equals(context.get(i))) {
                    hypothesisNum = i;
                    break;
                }
            }

            if (hypothesisNum != -1) {
                annStatements.add(new Annotator.Hypothesis(stmt, hypothesisNum));
            } else {
                annStatements.add(new Annotator.Unannotated(stmt));
            }
        }

        return annStatements;
    }
}
