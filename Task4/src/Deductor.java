import java.util.ArrayList;

public class Deductor {
    private static final ArrayList<Statement> axioms = Helper.axioms;
    private static final ArrayList<Statement> ruleSelf = Helper.parseKnownLines(
        "$1->$1->$1",
        "$1->($1->$1)->$1",
        "($1->$1->$1)->($1->($1->$1)->$1)->($1->$1)",
        "($1->($1->$1)->$1)->($1->$1)",
        "$1->$1"
    );
    private static final ArrayList<Statement> ruleAx = Helper.parseKnownLines(
        "$2",
        "$2->$1->$2",
        "$1->$2"
    );
    private static final ArrayList<Statement> ruleMP = Helper.parseKnownLines(
        "$1->$2",
        "$1->$2->$3",
        "($1->$2)->($1->$2->$3)->($1->$3)",
        "($1->$2->$3)->($1->$3)",
        "$1->$3"
    );

    private static final ArrayList<Statement> ruleIR1a = Helper.parseKnownLines(
        "($1->($2->$3))",
        "(($1->($2->$3))->(($1&$2)->($1->($2->$3))))",
        "(($1&$2)->($1->($2->$3)))",
        "(($1&$2)->$1)",
        "(($1&$2)->$2)",
        "((($1&$2)->$1)->((($1&$2)->($1->($2->$3)))->(($1&$2)->($2->$3))))",
        "((($1&$2)->($1->($2->$3)))->(($1&$2)->($2->$3)))",
        "(($1&$2)->($2->$3))",
        "((($1&$2)->$2)->((($1&$2)->($2->$3))->(($1&$2)->$3)))",
        "((($1&$2)->($2->$3))->(($1&$2)->$3))",
        "(($1&$2)->$3)"
    );

    private static final ArrayList<Statement> ruleIR1b = Helper.parseKnownLines(
        "(($1&$2)->$3)",
        "((($1&$2)->$3)->($1->(($1&$2)->$3)))",
        "($1->(($1&$2)->$3))",
        "((($1&$2)->$3)->($2->(($1&$2)->$3)))",
        "(((($1&$2)->$3)->($2->(($1&$2)->$3)))->($1->((($1&$2)->$3)->($2->(($1&$2)->$3)))))",
        "($1->((($1&$2)->$3)->($2->(($1&$2)->$3))))",
        "(($1->(($1&$2)->$3))->(($1->((($1&$2)->$3)->($2->(($1&$2)->$3))))->($1->($2->(($1&$2)->$3)))))",
        "(($1->((($1&$2)->$3)->($2->(($1&$2)->$3))))->($1->($2->(($1&$2)->$3))))",
        "($1->($2->(($1&$2)->$3)))",
        "($1->($2->($1&$2)))",
        "(($2->($1&$2))->(($2->(($1&$2)->$3))->($2->$3)))",
        "((($2->($1&$2))->(($2->(($1&$2)->$3))->($2->$3)))->($1->(($2->($1&$2))->(($2->(($1&$2)->$3))->($2->$3)))))",
        "($1->(($2->($1&$2))->(($2->(($1&$2)->$3))->($2->$3))))",
        "(($1->($2->($1&$2)))->(($1->(($2->($1&$2))->(($2->(($1&$2)->$3))->($2->$3))))->($1->(($2->(($1&$2)->$3))->($2->$3)))))",
        "(($1->(($2->($1&$2))->(($2->(($1&$2)->$3))->($2->$3))))->($1->(($2->(($1&$2)->$3))->($2->$3))))",
        "($1->(($2->(($1&$2)->$3))->($2->$3)))",
        "(($1->($2->(($1&$2)->$3)))->(($1->(($2->(($1&$2)->$3))->($2->$3)))->($1->($2->$3))))",
        "(($1->(($2->(($1&$2)->$3))->($2->$3)))->($1->($2->$3)))",
        "($1->($2->$3))"
    );

    private static final ArrayList<Statement> ruleIR2 = Helper.parseKnownLines(
        "($1->($2->$3))",
        "(($1->($2->$3))->($2->($1->($2->$3))))",
        "($2->($1->($2->$3)))",
        "($2->($1->$2))",
        "(($1->$2)->(($1->($2->$3))->($1->$3)))",
        "((($1->$2)->(($1->($2->$3))->($1->$3)))->($2->(($1->$2)->(($1->($2->$3))->($1->$3)))))",
        "($2->(($1->$2)->(($1->($2->$3))->($1->$3))))",
        "(($2->($1->$2))->(($2->(($1->$2)->(($1->($2->$3))->($1->$3))))->($2->(($1->($2->$3))->($1->$3)))))",
        "(($2->(($1->$2)->(($1->($2->$3))->($1->$3))))->($2->(($1->($2->$3))->($1->$3))))",
        "($2->(($1->($2->$3))->($1->$3)))",
        "(($2->($1->($2->$3)))->(($2->(($1->($2->$3))->($1->$3)))->($2->($1->$3))))",
        "(($2->(($1->($2->$3))->($1->$3)))->($2->($1->$3)))",
        "($2->($1->$3))"
    );

    private static void processCase(ArrayList<Statement> rule, ArrayList<Annotator.AnnotatedStatement> out, Statement[] to) {
        for (int i = 0; i < rule.size(); i++) {
            out.add(new Annotator.Unannotated(rule.get(i).substitutePatterns(to)));
        }
    }

    public static ArrayList<Annotator.AnnotatedStatement> liberate(ArrayList<Annotator.AnnotatedStatement> proof, Statement hypothesis) {
        Statement[] to = new Statement[]{hypothesis, null, null}; // 3 variables ought to be enough for anybody
        
        ArrayList<Annotator.AnnotatedStatement> liberatedProof = new ArrayList<Annotator.AnnotatedStatement>();

        for (Annotator.AnnotatedStatement annStmt : proof) {
            Statement stmt = annStmt.statement;

            // case 1: A -> A
            if (stmt.equals(hypothesis)) {
                processCase(ruleSelf, liberatedProof, to);
            }

            // case 2: A -> a, a is hypothesis or axiom
            else if (annStmt instanceof Annotator.Hypothesis || annStmt instanceof Annotator.Axiom) {
                to[1] = stmt;
                processCase(ruleAx, liberatedProof, to);
            }

            // case 3: A -> b, b is modus ponens
            else if (annStmt instanceof Annotator.ModusPonens) {
                to[1] = proof.get(((Annotator.ModusPonens)annStmt).alpha).statement;
                to[2] = stmt;
                processCase(ruleMP, liberatedProof, to);
            }

            else if (annStmt instanceof Annotator.InferenceRule) {
                Annotator.InferenceRule irStmt = (Annotator.InferenceRule) annStmt;
                Implication st = (Implication) proof.get(irStmt.lineNo).statement;
                Statement stL = st.left;
                Statement stR = st.right;
                
                // case 4: A -> c, c is inference rule 1
                if (irStmt.n == 1) {
                    String varName = ((Forall) (((Implication) irStmt.statement).right)).varName;
                   
                    to[1] = stL;
                    to[2] = stR;
                    processCase(ruleIR1a, liberatedProof, to);

                    Statement quant = new Forall(varName, stR);
                    Statement newLine = new Implication(new Conjunction(hypothesis, stL), quant);
                    liberatedProof.add(new Annotator.Unannotated(newLine));

                    to[2] = quant;
                    processCase(ruleIR1b, liberatedProof, to);
                }

                // case 5: A -> d, d is inference rule 2
                else if (irStmt.n == 2) {
                    String varName = ((Exists) (((Implication) irStmt.statement).left)).varName;
                    
                    to[1] = stL;
                    to[2] = stR;
                    processCase(ruleIR2, liberatedProof, to);

                    Statement quant = new Exists(varName, stL);
                    Statement newLine = new Implication(quant, new Implication(hypothesis, stR));
                    liberatedProof.add(new Annotator.Unannotated(newLine));

                    to[0] = quant;
                    to[1] = hypothesis;
                    processCase(ruleIR2, liberatedProof, to);
                    to[0] = hypothesis;
                }
            }
        }

        return liberatedProof;
    }
}
