
//  The Human Attendant

import java.io.*;
import java.util.*;
import java.net.*;

class Attendant {
    public final static CardSource Source = new CardSource("Attendant", -1);
    private static boolean addComments, writeDown = true;
    private static String numberPicture = null;
    private AnnunciatorPanel panel;
    public Vector cardChain = null;
    private int ncards = 0;
    private boolean errorDetected = false;
    private String libraryTemplate = null;
    private boolean allowFileInclusion = false;
    private OutputApparatus annotationDevice = null;

    //  Constructor

    Attendant(AnnunciatorPanel p) {
        panel = p; 
        reset();
    }

    //  Set output device for annotations

    void setAnnotation(OutputApparatus a) {
        annotationDevice = a;
    }

    //  Reset to starting conditions

    private void reset() {
        newCardChain();
        libraryTemplate = null;
        allowFileInclusion = false;
        annotationDevice = null;
    }

    //  Read line from input card stream

    private String readLine(LineNumberInputStream is, String fileName) {
        StringBuffer s = new StringBuffer(80);
        int i = -1;

        try {
            while ((i = is.read()) != -1) {
                char c = (char) i;

                if (c == '\n') {
                    break;
                }
                s.append(c);
            }
            if (i == -1) {
                if (s.length() > 0) {
                    return s.toString();
                }
                return null;
            }
        } catch (IOException e) {
            panel.attendantLogMessage("I encountered an error reading cards from file " +
                fileName + ": " + e);
            errorDetected = true;
            return null;
        }
        return s.toString();
    }

    //  Set the template used to expand library requests to file names

    public void setLibraryTemplate(String s) {
        libraryTemplate = s;
    }

    //  Set whether inclusion of cards from local files is permitted

    public void setFileInclusion(boolean allow) {
        allowFileInclusion = allow;
    }

    //  Determine if a library name is valid

    static private String libValid = "abcdefghijklmnopqrstuvwxyz-_0123456789";
    private boolean isLibraryNameValid(String s) {
        int i;

        for (i = 0; i < s.length(); i++) {
            if (libValid.indexOf(s.charAt(i)) < 0) {
                return false;
            }
        }
        return true;
    }

    //  Add cards from an input stream to the chain

    public boolean addStream(InputStream istream, String description) {
        LineNumberInputStream s;
        String card;

        CardSource cs = new CardSource(description, ncards);

        s = new LineNumberInputStream(istream);
        while (!errorDetected && (card = readLine(s, description)) != null) {
            boolean shortform;
            int offset;

            /*  We recognise a card which begins with an "@" or the
                more Victorian " A include cards " as an
                instruction to the attendant to locate the chain
                of cards bearing that label and interpolate it
                into the chain, replacing the card requesting it.
                This allows commonly used sequences of calculation
                to be called out without having to physically copy
                them into the original card chain.  */

            if (allowFileInclusion && card.length() > 1 &&
                    ((shortform = (card.charAt(0) == '@')) ||
                    card.toLowerCase().startsWith("a include cards "))) {
                offset = shortform ? 1 : 16;
                cardChain.addElement(new Card(". Begin interpolation of " +
                    card.substring(offset) + " by attendant", -1,
                    Attendant.Source));
                ncards++;
                addFile(card.substring(offset));
                cardChain.addElement(new Card(". End interpolation of " +
                    card.substring(offset) + " by attendant", -1,
                    Attendant.Source));
                ncards++;
            } else if (card.toLowerCase().startsWith("a include from library cards for ")) {

                /*  The analyst can request the inclusion of a set of
                    cards from the machine's library with the request
                    to the attendant:

                        A include from library cards for <whatever>

                    The attendant searches for a set of cards named
                    <whatever> and, if found, they are strung into
                    the chain, replacing the library request.

                    This is implemented by extracting the <whatever>
                    name, using isLibraryNameValid to make sure it
                    contains no characters which might subvert system
                    security, then creating an actual path name by
                    substituting it into the libraryTemplate.  The
                    library facility allows a server which is running
                    emulations to provide a local library accessible
                    by users without requiring them to download the
                    cards themselves, and without compromising the
                    server's own file system security.  */

                String lspec = card.substring(33).trim();
                String lname = lspec.toLowerCase();
                boolean libok = false;

                if ((libraryTemplate != null) && isLibraryNameValid(lname)) {
                    int tidx = libraryTemplate.indexOf("@@", 0);

                    if (tidx >= 0) {
                        String lfname = libraryTemplate.substring(0, tidx) +
                            lname + libraryTemplate.substring(tidx + 2);

                        cardChain.addElement(new Card(". Begin interpolation of " +
                            lspec + " from library by attendant", -1,
                            Attendant.Source));
                        ncards++;
                        addFile(lfname, true, lspec + " [Library]");
                        cardChain.addElement(new Card(". End interpolation of " +
                            lspec + " from library by attendant", -1,
                            Attendant.Source));
                        ncards++;
                        libok = !errorDetected;
                    }
                }
                if (!libok) {
                    panel.attendantLogMessage("I could not find a chain of cards named " + lspec + " in the library\n");
                    errorDetected = true;
                }
            } else {
                cardChain.addElement(new Card(card, ncards, cs));
                ncards++;
            }
        }
        return errorDetected;
    }

    //  Test whether a specification is a URL or file system path

    boolean isURL(String fileName) {
        return fileName.startsWith("http:") || fileName.startsWith("file:") ||
               fileName.startsWith("ftp:");
    }

    //  Add cards from a file to the chain

    public boolean addFile(String fileName, boolean isLib, String useName) {
        if (isURL(fileName)) {
            try {
                URL u = new URL(fileName);

                try {
                    errorDetected = addStream(u.openStream(),
                                              useName == null ? fileName : useName);
                } catch (IOException e) {
                    if (!isLib) {
                        panel.attendantLogMessage("I could not find the file of cards named " + fileName + "\n");
                    }
                    errorDetected = true;
                }
            } catch (MalformedURLException e) {
                if (!isLib) {
                    panel.attendantLogMessage("I could not find the file of cards named " + fileName + "\n");
                }
                errorDetected = true;
            }
        } else {
            try {
                errorDetected = addStream(new FileInputStream(fileName),
                                          useName == null ? fileName : useName);
            } catch (FileNotFoundException e) {
                if (!isLib) {
                    panel.attendantLogMessage("I could not find the file of cards named " + fileName + "\n");
                }
                errorDetected = true;
            }
        }
        return errorDetected;
    }

    public boolean addFile(String fileName) {
        return addFile(fileName, false, null);
    }

    //  Return a string containing cards obtained from a file or URL

    public String obtainCardString(String fileName) {
        InputStream istream = null;
        StringBuffer result = new StringBuffer(2048);

        errorDetected = false;
        if (isURL(fileName)) {
            try {
                URL u = new URL(fileName);

                try {
                    istream = u.openStream();
                } catch (IOException e) {
                }
            } catch (MalformedURLException e) {
            }
        } else {
            try {
                istream = new FileInputStream(fileName);
            } catch (FileNotFoundException e) {
            }
        }
        if (istream == null) {
            panel.attendantLogMessage("I could not find the file of cards named " + fileName + "\n");
            errorDetected = true;
        } else {
            LineNumberInputStream s;
            String card;

            s = new LineNumberInputStream(istream);
            while (!errorDetected && (card = readLine(s, fileName)) != null) {
                result.append(card + "\n");
            }
        }
        return errorDetected ? null : result.toString();
    }

    //  Add cards supplied in a String

    public boolean addString(String theCards) {
        errorDetected |= addStream(new StringBufferInputStream(theCards), "Cards");
        return errorDetected;
    }

    //  Begin a new chain of cards

    public void newCardChain() {
        cardChain = new Vector(100, 100);
        ncards = 0;
        errorDetected = false;
    }

    //  Mount card chain in card reader

    public boolean mountCards(CardReader cr, boolean comments) {
        numberPicture = null;
        writeDown = true;
        examineCards(cardChain, comments);
        if (!errorDetected) {
            cr.reset();
            cr.mountCards(cardChain);
        }
        return errorDetected;
    }

    //  Issue a complaint when an error is found in the card chain

    private final void complain(Card c, String s) {
        panel.attendantLogMessage(c + "\n");
        panel.attendantLogMessage(s + "\n");
        errorDetected = true;
    }

    /*  Test whether this card marks the end of a cycle
        which is to be translated into combinatorial
        cards.  */

    private static final boolean isCycleStart(Card c) {
        String s = c.text;

        return s.length() > 0 && ((s.charAt(0) == '(') ||
                                  (s.charAt(0) == '{'));
    }

    /*  Test whether this card marks the end of a cycle.  */

    private static final boolean isCycleEnd(Card c) {
        String s = c.text;

        return s.length() > 0 && ((s.charAt(0) == ')') ||
                                  (s.charAt(0) == '}'));
    }

    /*  Test whether card is a comment.  */

    private static final boolean isComment(Card c) {
        String s = c.text;

        return (s.length() < 1) || (s.charAt(0) == '.') ||
            (s.charAt(0) == ' ');
    }

    /*  Translate a cycle into equivalent combinatorial
        cards the Mill can process.  If addComments
        is set the original attendant request cards are
        left in place as comments; otherwise they are
        removed.  If the cycle contains a cycle,
        translateCycle calls itself to translate the
        inner cycle.

        Warning: fiddling with the logic that computes
        the number of cards the combinatorial cards skip
        may lead to a skull explosion.  The correct number
        depends on the type of cycle, whether or not an
        else branch exists on a conditional, and whether
        comments are being retained or deleted, and is
        intimately associated with precisely where the
        combinatorial card is inserted when comments are
        being retained.

    */

    private boolean translateCycle(Vector cards, int start) {
        Card c = (Card) cards.elementAt(start);
        String s = c.text;
        char which = s.charAt(0);
        boolean depends = false, error = false;
        int i;

        if (s.length() > 1) {
            depends = s.charAt(1) == '?';
        }
        if (addComments) {
            c.text = ". " + s + " Translated by attendant";
        } else {
            cards.removeElementAt(start);
            start--;
        }

        /*  Search for the end of this cycle.  If a sub-cycle
            is detected, recurse to translate it.  */

        for (i = start + 1; i < cards.size(); i++) {
            Card u = (Card) cards.elementAt(i);


            if (isCycleStart(u)) {
                translateCycle(cards, i);
            }
            if (isCycleEnd(u)) {
                boolean isElse = u.text.startsWith("}{");

                if (u.text.charAt(0) != (which == '(' ? ')': '}')) {
                    complain(u, "End of cycle does not match " + which +
                                " beginning on card " + start);
                    error = true;
                }
                if (addComments) {
                    u.text = ". " + u.text + " Translated by attendant";
                } else {
                    cards.removeElementAt(i);
                }
                if (which == '(') {

                    //  It's a loop

                    cards.insertElementAt(new
                        Card("CB" + 
                                (depends ? "?" : "+") +
                                (i - start),
                                -1, Source), i);
                } else {

                    //  It's a forward skip, possibly with an else clause

                    cards.insertElementAt(new
                        Card("CF" + (depends ? "?" : "+") +
                            ((isElse ? (addComments ? 1 : 0) : (addComments ? 0 : -1)) + Math.abs(i - start)),
                            -1, Source), start + 1);

                    //  Translate else branch of conditional, if present

                    if (isElse) {
                        int j;

                        for (j = i + 1; j < cards.size(); j++) {
                            u = (Card) cards.elementAt(j);

                            if (isCycleStart(u)) {
                                translateCycle(cards, j);
                            }
                            if (isCycleEnd(u)) {
                                if (u.text.charAt(0) != '}') {
                                    complain(u, "End of else cycle does not match " + which +
                                                " beginning on card " + i);
                                    error = true;
                                }
                                if (addComments) {
                                    u.text = ". " + u.text + " Translated by attendant";
                                } else {
                                    cards.removeElementAt(j);
                                }
                                cards.insertElementAt(new
                                    Card("CF+" + (Math.abs(j - i) - (addComments ? 1 : 1)),
                                    -1, Source), i + (addComments ? 2 : 1));
                                return error;
                            }
                        }
                    }
                }
                return error;
            }
        }
        complain(c, "No matching end of cycle.");
        return true;
    }

    private void translateCombinatorics(Vector cards) {
        int i;

        for (i = 0; i < cards.size(); i++) {
            if (isCycleStart((Card) cards.elementAt(i))) {
                translateCycle(cards, i);
            }
        }
    }

    /*  Perform any requested fixed-point expansions.  */

    private void translateFixedPoint(Vector cards, boolean comments) {
        int i;
        int decimalPlace = -1;

        for (i = 0; i < cards.size(); i++) {
            Card thisCard = (Card) cards.elementAt(i);
            String card = thisCard.text.toLowerCase();

            //  A set decimal places to [+/-]n

            if (card.startsWith("a set decimal places to ")) {
                String dspec = card.substring(24).trim();
                int relative = 0;
                boolean dspok = true;

                if (dspec.charAt(0) == '+' || dspec.charAt(0) == '-') {
                    if (decimalPlace == -1) {
                        complain(thisCard, "I cannot accept a relative decimal place setting\nwithout a prior absolute setting.");
                        dspok = false;
                    } else {
                        relative = (dspec.charAt(0) == '+') ? 1 : -1;
                        dspec = dspec.substring(1);
                    }
                }
                if (dspok) {
                    try {
                        int d = Integer.parseInt(dspec);
                        if (relative != 0) {
                            d = decimalPlace + (d * relative);
                        }
                        if (d < 0 || d > 50) {
                            complain(thisCard, "I can only set the decimal place between 0 and 50 digits.");
                        } else {
                            if (comments) {
                                thisCard.text = ". " + thisCard.text;
                            } else {
                                cards.removeElementAt(i);
                                i--;
                            }
                            decimalPlace = d;
                        }
                    } catch (NumberFormatException ne) {
                        complain(thisCard, "I cannot find the number of decimal places you wish to use.");
                    }
                }

            //  Convert "A write numbers with decimal point" to a picture

            } else if (card.startsWith("a write numbers with decimal point")) {
                int dpa;

                if (decimalPlace < 0) {
                    complain(thisCard, "I cannot add the number of decimal places because\n" +
                        "you have not instructed me how many decimal\n" +
                        "places to use in a prior \"A set decimal places to\"\ninstruction.");
                    return;
                }
                thisCard.text = "A write numbers as 9.";
                for (dpa = 0; dpa < decimalPlace; dpa++) {
                    thisCard.text += "9";
                }

            //  Add step up/down to "<" or ">" if not specified

            } else if ((card.startsWith("<") || card.startsWith(">")) &&
                        (card.trim().length() == 1)) {
                if (decimalPlace < 0) {
                    complain(thisCard, "I cannot add the number of decimal places because\n" +
                        "you have not instructed me how many decimal\n" +
                        "places to use in a prior \"A set decimal places\"\ninstruction.");
                    return;
                } else {
                    thisCard.text += String.valueOf(decimalPlace);
                    if (comments) {
                        thisCard.text += " . Step count added by attendant";
                    }
                }

            /*  Replace number cards with decimal points with
                cards scaled to the proper number of digits.  */

            } else if (card.startsWith("n")) {
                StringTokenizer stok = new StringTokenizer(card.substring(1));
                if (stok.countTokens() == 2) {
                    String cn, nspec;

                    cn = stok.nextToken();
                    nspec = stok.nextToken();
                    int dp = nspec.indexOf(".");

                    if (dp >= 0) {
                        if (decimalPlace < 0) {
                            complain(thisCard, "I cannot add the number of decimal places because\n" +
                                "you have not instructed me how many decimal\n" +
                                "places to use in a prior \"A set decimal places\"\ninstruction.");
                            return;
                        } else {
                            String dpart = nspec.substring(dp + 1),
                                   dpnew = "";
                            int j, places = 0;
                            char ch;

                            for (j = 0; j < dpart.length(); j++) {
                                if (Character.isDigit(ch = dpart.charAt(j))) {
                                    dpnew += ch;
                                    places++;
                                }
                            }

                            /* Now adjust the decimal part to the given number
                               of decimal places by trimming excess digits and
                               appending zeroes as required. */

                            if (dpart.length() > decimalPlace) {
                                /* If we're trimming excess digits, round the
                                   remaining digits based on the first digit
                                   of the portion trimmed. */
                                if ((decimalPlace > 0) &&
                                    (Character.digit(
                                        dpart.charAt(decimalPlace), 10) >= 5)) {
                                    dpart = BigInt.valueOf(dpart.substring(0, decimalPlace)).add(BigInt.ONE).toString();
                                } else {
                                    dpart = dpart.substring(0, decimalPlace);
                                }
                            }
                            while (dpart.length() < decimalPlace) {
                                dpart += "0";
                            }

                            //  Append the decimal part to fixed part from card

                            thisCard.text = "N" + cn + " " +
                                nspec.substring(0, dp) + dpart;
                            if (comments) {
                                thisCard.text += " . Decimal expansion by attendant";
                            }
                        }
                    }
                }

            }
        }
    }

    private void examineCards(Vector cards, boolean comments) {
        int i;

        addComments = comments;
        if (!comments) {
            for (i = 0; i < cards.size(); i++) {
                if (isComment((Card) cards.elementAt(i))) {
                    cards.removeElementAt(i);
                    i--;
                }
            }
        }
        translateFixedPoint(cards, comments);
        translateCombinatorics(cards);
    }

    /*  Process request when the mill stops on an attendant action
        card.  */

    public boolean processActionCard(Card c) {
        boolean ok = true;
        String card = c.text;

        /*  "write"  Control the transcription of 
                     output from the printer to the
                     final summary of the computation.  */

        if (card.toLowerCase().startsWith(" write ", 1)) {
            if (card.toLowerCase().startsWith("numbers as ", 8)) {
                setPicture(card.substring(19));
            } else if (card.toLowerCase().startsWith("annotation ", 8)) {
                writeAnnotation(card.substring(19));
            } else if (card.toLowerCase().startsWith("in columns", 8)) {
                setWriteAcross();
            } else if (card.toLowerCase().startsWith("in rows", 8)) {
                setWriteDown();
            } else if (card.toLowerCase().startsWith("new line", 8)) {
                writeNewLine();
            } else {
                ok = false;
            }
        } else {
            ok = false;
        }
        if (!ok) {
            complain(c, "I do not understand this request for attendant action.");
        }
        return ok;
    }

    /*  Inform the attendant when an abnormality occurs in the
        operation of the mill.  */

    private static final String MillAb = "Abnormality in the Mill: ";

    public void millAbnormality(String what, Card c) {
        if (c != null) {
            complain(c, MillAb + what);
        } else {
            panel.attendantLogMessage(MillAb + what + "\n");
            errorDetected = true;
        }
    }

    /*  Inform the attendant of an item to be added to the trace
        log.  */

    public void traceLog(String s) {
        panel.attendantWriteTrace(s + "\n");
    }

    //  Set picture to be used in printing subsequent numbers

    private static void setPicture(String pic) {
        if (pic.length() < 1) {
            pic = null;
        }
        numberPicture = pic;
    }

    public static String editToPicture(BigInt v) {
        String s;
        boolean negative = v.test() < 0, sign = false;

        if (numberPicture != null) {
            s = v.abs().toString();
            int i = numberPicture.length();
            String o = "";

            while (--i >= 0) {
                char c = numberPicture.charAt(i);

                switch (c) {
                    case '9':         //  Digit, unconditionally
                        if (s.length() == 0) {
                            o = "0" + o;
                        } else {
                            o = s.substring(s.length() - 1, s.length()) + o;
                            s = s.substring(0, s.length() - 1);
                        }
                        break;

                    case '#':         // Digit, if number not exhausted
                        if (s.length() > 0) {
                            o = s.substring(s.length() - 1, s.length()) + o;
                            s = s.substring(0, s.length() - 1);
                        }
                        break;

                    case ',':         // Comma if digits remain to output
                        if ((numberPicture.indexOf('9') >= 0) ||
                            s.length() > 0) {
                            o = c + o;
                        }
                        break;

                    case '-':         // Sign if negative
                        if (negative) {
                            o = '-' + o;
                            sign = true;
                        }
                        break;

                    case '±':         // Plus or minus sign
                        o = (negative ? '-' : '+') + o;
                        sign = true;
                        break;

                    case '+':         // Sign if negative, space if positive
                        o = (negative ? '-' : ' ') + o;
                        sign = true;
                        break;

                    default:          // Copy character to output
                        o = c + o;
                        break;
                }
            }
            /*  If there's any number "left over", write it to
                prevent truncation without warning.  */
            if (s.length() > 0) {
                o = s + o;
            }
            /*  If the number is negative and no sign has been output
                so far, prefix it with a sign.  */
            if (negative && !sign) {
                o = '-' + o;
            }
            s = o;
        } else {
            s = v.toString();
        }

        return s;
    }

    //  Write selected sequence at end of item

    void writeEndOfItem() {
        if (writeDown) {
            panel.attendantLogMessage("\n");
        }
    }

    void writeEndOfItem(OutputApparatus a) {
        if (writeDown) {
            a.Output("\n");
        }
    }

    private static void setWriteAcross() {
        writeDown = false;
    }

    private static void setWriteDown() {
        writeDown = true;
    }

    private void writeAnnotation(String s) {
        annotationDevice.Output(s);
        writeEndOfItem(annotationDevice);
    }

    private void writeNewLine() {
        annotationDevice.Output("\n");
    }
}
