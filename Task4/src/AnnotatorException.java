public class AnnotatorException extends Exception {
    private final Statement statement;
    private final String message;
    private final int lineNo;

    public AnnotatorException(int lineNo, Statement statement, String message) {
        this.statement = statement;
        this.message = message;
        this.lineNo = lineNo;
    }

    public Statement getStatement() {
        return statement;
    }

    public String getMessage() {
        return message;
    }

    public int getLineNo() {return lineNo;}
}
