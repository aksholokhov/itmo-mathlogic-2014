import java.util.ArrayList;

public class Proof {
    private final ArrayList<Statement> context;
    private final ArrayList<Annotator.AnnotatedStatement> statements;

    private final Annotator annotator = new Annotator();
    private final Deductor liberator = new Deductor();

    public Proof(ArrayList<Statement> context, ArrayList<Annotator.AnnotatedStatement> statements) {
        this.context = context;
        this.statements = statements;
    }

    public ArrayList<Annotator.AnnotatedStatement> getStatements() {
        return statements;
    }

    public Proof annotate() throws AnnotatorException {
        return new Proof(context, annotator.annotate(context, statements));
    }

    public Proof liberate() throws AnnotatorException {
        ArrayList<Statement> newContext = new ArrayList<Statement>(context);
        Statement hypothesis = newContext.remove(newContext.size() - 1);

        return new Proof(newContext, liberator.liberate(annotator.annotate(context, statements), hypothesis));
    }

    public Proof optimize() throws AnnotatorException {
        ArrayList<Statement> newStatements = Helper.optimizeAnnotated(annotator.annotate(context, statements));

        return new Proof(context, Helper.annotateInContext(context, newStatements));
    }
}
