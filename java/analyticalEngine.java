import java.util.*;

//  The Analytical Engine

class analyticalEngine {
    private final static BigInt K10e50 =
        BigInt.valueOf("100000000000000000000000000000000000000000000000000");

    AnnunciatorPanel panel = null;
    CardReader cardReader = null;
    Store store = null;
    Mill mill = null;
    Attendant attendant = null;
    CardPunchingApparatus punch = null;
    PrintingApparatus printer = null;
    CurveDrawingApparatus curvedraw = null;
    boolean running = false, trace = false;
    boolean errorDetected = false;

    //  Constructor

    analyticalEngine(AnnunciatorPanel p) {
        panel = p;
        attendant = new Attendant(panel);
        mill = new Mill(panel, attendant);
        store = new Store(panel, attendant);
        cardReader = new CardReader(panel, attendant);
        curvedraw = new CurveDrawingApparatus(panel, attendant);
        punch = new CardPunchingApparatus();
        printer = new PrintingApparatus();
        attendant.setAnnotation(printer);
        reset();
    }

    public void reset() {
        errorDetected = false;
        running = false;
    }

    //  Test if an error has occurred

    public boolean error() {
        return errorDetected;
    }

    //  setTrace  --  Set trace mode

    void setTrace(boolean t) {
        trace = t;
        mill.setTrace(t);
        store.setTrace(t);
    }

    /*  setCurveDrawingApparatus  --  Specify custom curve drawing apparatus

            To avoid forcing the caller to know our private panel
            and attendant objects, the caller creates the
            CurveDrawingApparatus-derived class with the
            default constructor which nulls these values.  After
            we're handed the CurveDrawingApparatus we call
            setPanelAndAttendant to supply them.  */

    void setCurveDrawingApparatus(CurveDrawingApparatus c) {
        c.setPanelAndAttendant(panel, attendant);
        curvedraw = c;
    }

    /*  setCardPunch --  Specify custom card punch  */

    void setCardPunch(CardPunchingApparatus c) {
        c.setPanelAndAttendant(panel, attendant);
        punch = c;
    }

    /*  setPrinter --  Specify custom printer  */

    void setPrinter(PrintingApparatus c) {
        c.setPanelAndAttendant(panel, attendant);
        printer = c;
        attendant.setAnnotation(printer);
    }

    //  setLibraryTemplate  --  Set attendant library template (null if none)

    void setLibraryTemplate(String s) {
        attendant.setLibraryTemplate(s);
    }

    /*  allowFileInclusion  --  Control whether the attendant permits
                                cards to be included from local files.  */

    void allowFileInclusion(boolean permit) {
        attendant.setFileInclusion(permit);
    }

    //  loadNewCards  --  Prepare to load a new chain of cards

    void loadNewCards() {
        errorDetected = false;
        attendant.newCardChain();
    }

    //  loadCardsFromFile  --  Load cards into from file into card chain

    void loadCardsFromFile(String fileName) {
        errorDetected |= attendant.addFile(fileName);
    }

    /*  loadCardsFromString  --  Load cards from string.  Individual
                                 cards in the string are delimited by
                                 the new line character.  */

    void loadCardsFromString(String theCards) {
        errorDetected |= attendant.addString(theCards);
    }

    //  scrutinise  --  Ask attendant to prepare and mount cards

    void scrutinise(boolean comments) {
        errorDetected |= attendant.mountCards(cardReader, comments);
    }

    //  obtainCardString  --  Obtain cards from analyst's file or URL

    String obtainCardString(String fileOrURL) {
        return attendant.obtainCardString(fileOrURL);
    }

    //  dumpCards  --  Dump listing of cards

    void dumpCards(boolean punched) {
        if (punched) {
            cardReader.listAll(punch, true);
        } else {
            cardReader.listAll(printer, false);
        }
    }

    //  Halt  --  Stop the mill

    public void halt() {
        panel.changeMillRunning(running = false);
    }

    /*  errorHalt  --  Notify attendant if an error is detected
                       in the mill.  */

    private void errorHalt(String why, Card perpetrator) {
        attendant.millAbnormality(why, perpetrator);
        errorDetected = true;
        halt();
    }

    //  commence  --  Set up to run new chain of cards

    void commence() {
        store.reset();
        mill.reset();
        cardReader.firstCard();
    }

    //  processCard  --  Process the next card

    boolean processCard() {
        boolean cardAvailable = false, halted = false;
        Card currentCard;

        if ((currentCard = cardReader.nextCard()) != null) {
            String card = currentCard.text;
            StringTokenizer stok;
            char operation = card.length() == 0 ? ' ' : card.charAt(0);
            boolean prime = false;
            int n = 0, cl = 0;
            BigInt v = BigInt.ZERO;

            cardAvailable = true;
            if (trace) {
                attendant.traceLog("Card:  " + currentCard);
            }

            //  Trim possible comment from card

            if ((cl = card.indexOf(". ", 1)) >= 0) {
                /* It's okay to use trim() here since the only
                   situation in which leading space would be
                   dropped is if the card is a comment in the
                   first place.  Since the operation has already
                   been extracted, no harm is done if the leading
                   space on the comment is dropped by the trim()
                   below. */
                card = card.substring(0, cl).trim();
            }

            switch (operation) {

                //  Mill operations (Operation cards)

                case '+':
                case '-':
                case '×': case '*': case 'x':
                case '÷': case '/':
                    mill.setOperation(operation);
                    break;

                case '<':
                case '>':
                    n = -1;
                    if (card.length() > 1) {
                        try {
                            n = Integer.parseInt(card.substring(1));
                        } catch (NumberFormatException ne) {
                        }
                    }
                    if (n < 0 || n > 100) {
                        errorHalt("Bad stepping up/down card", currentCard);
                    }
                    mill.shiftAxes((operation == '<') ? n : -n);
                    break;

                //  Mill to store transfers (Variable cards)

                case 'L':
                case 'Z':
                case 'S':
                    prime = false;
                    try {
                        String sprune = card.substring(1).trim();
                        if (sprune.endsWith("'")) {
                            prime = true;
                            sprune = sprune.substring(0, sprune.length() - 1).trim();
                        }
                        n = Integer.parseInt(sprune);
                    } catch (NumberFormatException ne) {
                        n = -1;
                    }
                    if (n < 0) {
                        errorHalt("Bad variable card", currentCard);
                        break;
                    }

                    switch (operation) {
                        case 'L':
                            mill.transferIn(store.get(n), prime);
                            break;

                        case 'Z':
                            mill.transferIn(store.get(n), prime);
                            store.set(n, BigInt.ZERO);
                            break;

                        case 'S':
                            store.set(n, mill.transferOut(prime));
                            break;
                    }
                    break;

                //  Number cards

                case 'N':
                    stok = new StringTokenizer(card.substring(1));
                    n = -1;
                    if (stok.countTokens() == 2) {
                        try {
                            String vn;

                            n = Integer.parseInt(stok.nextToken());
                            vn = stok.nextToken();
                            if (vn.charAt(0) == '+') {
                                vn = vn.substring(1);
                            }
                            v = BigInt.valueOf(vn);
                        } catch (NumberFormatException ne) {
                        }
                    }
                    if (n < 0 || n > 999 || v.abs().compare(K10e50) >= 0) {
                        errorHalt("Bad number card", currentCard);
                        break;
                    }
                    store.set(n, v);
                    break;

                //  Combinatorial cards

                case 'C':
                    {
                        int howMany;
                        boolean withinChain = true;

                        if (card.length() < 4 ||
                            (card.charAt(1) != 'F' && card.charAt(1) != 'B') ||
                            (card.charAt(2) != '?' &&
                             card.charAt(2) != '1' && card.charAt(2) != '+')) {
                            errorHalt("Bad combinatorial card", currentCard);
                            break;
                        }
                        try {
                            howMany = Integer.parseInt(card.substring(3).trim());
                        } catch (NumberFormatException ne) {
                            howMany = -1;
                        }
                        if (n < 0) {
                            errorHalt("Bad combinatorial card cycle length", currentCard);
                            break;
                        }
                        if (card.charAt(2) == '1' || card.charAt(2) == '+' ||
                            mill.hasRunUp()) {
                            if (card.charAt(1) == 'F') {
                                withinChain = cardReader.advance(howMany);
                            } else {
                                withinChain = cardReader.repeat(howMany);
                            }
                            if (!withinChain) {
                                errorHalt("Card chain fell on floor during", currentCard);
                                break;
                            }
                        }
                    }
                    break;

                //  Control cards

                case 'B':             // Ring Bell
                    panel.ringBell();
                    break;

                case 'P':             // Print
                    mill.print(printer);
                    break;

                case 'H':             // Halt
                    panel.changeMillRunning(running = false, card.substring(1));
                    halted = true;
                    break;

                //  Curve Drawing Apparatus

                case 'D':
                    if (card.length() > 1) {
                        switch (card.charAt(1)) {
                            case 'X':
                                curvedraw.setX(mill.outAxis());
                                break;

                            case 'Y':
                                curvedraw.setY(mill.outAxis());
                                break;

                            case '+':
                                curvedraw.drawTo();
                                break;

                            case '-':
                                curvedraw.moveTo();
                                break;

                            default:
                                errorHalt("Bad curve drawing card", currentCard);
                                break;
                        }
                    }
                    break;

                //  Attendant action cards

                case 'A':
                    if (!attendant.processActionCard(currentCard)) {
                        panel.changeMillRunning(running = false);
                        errorDetected = true;
                        break;
                    }
                    break;

                //  Non-period diagnostic cards

                case 'T':             // Trace
                    setTrace((card.length() > 1) && (card.charAt(1) == '1'));
                    break;

                case ' ':
                case '.':             // Comment
                    break;

                default:
                    errorHalt("Unknown operation", currentCard);
                    break;
            }
        }
        return cardAvailable && !halted && !errorDetected;
    }

    //  run  --  Run until halted by error, halt card, or end of chain

    void run() {
        panel.changeMillRunning(running = true);

        while (running && processCard()) {
        }
        panel.changeMillRunning(running = false);
    }
}
