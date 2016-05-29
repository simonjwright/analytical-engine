/*

                            by John Walker
                       http://www.fourmilab.ch/

                This program is in the public domain.

*/

import java.applet.*;
import java.awt.*;
import java.util.*;
import java.net.*;

//  Utilities

class Util {
    static final String sppad = "                                                             ";

    static final public String padNum(BigInt n, int places) {
        String s = n.toString();

        if (s.length() < places) {
            s = sppad.substring(0, (places - s.length()) + 1) + s;
        }
        return s;
    }

    static final public String padNum(BigInt n) {
        return padNum(n, 51);
    }
}

//  Graphical label component

class graphicalLabel extends Canvas {
    String alternateText = null;
    Image img = null;
    int hpad = 0, vpad = 0;

    graphicalLabel(String imageSource, String altext, int hp, int vp) {
        URL u;

        hpad = hp;
        vpad = vp;
        try {
            u = new URL(imageSource);
        } catch (MalformedURLException e) {
            u = null;
System.out.println("graphicalLabel: Bad URL " + imageSource);
        }
        if (u != null) {
            img = getToolkit().getImage(u);
        }
        if (img == null) {
//  Create replacement image from text
System.out.println("graphicalLabel: Not found " + imageSource);
        } else {
            prepareImage(img, null);
            waitForImage(img);
        }
    }

    graphicalLabel(String imageSource, String altext) {
        this(imageSource, altext, 0, 0);
    }

    //  Wait for image to arrive

    public void waitForImage(Image im) {
        try {
            MediaTracker mt = new MediaTracker(this);

            mt.addImage(im, 0);
            mt.waitForID(0);
        } catch (Exception e) {
        }
    }

    //  Paint the image into the canvas

    public void paint(Graphics g) {
        Dimension size = size();

        g.drawImage(img, (size.width - img.getWidth(null)) / 2, vpad, null);
    }

    //  PREFERREDSIZE  --  Indicate how large we'd like to be, given our druthers

    public Dimension preferredSize() {
        return new Dimension(img.getWidth(null) + hpad * 2,
                             img.getHeight(null) + vpad * 2);
    }
}

//  Mill display component

class millDisplay extends Canvas {
    Font textFont;
    FontMetrics textFontM;
    Graphics gee;
    BigInt[] ingress = new BigInt[3];
    BigInt[] egress = new BigInt[2];
    String operation;
    boolean running, run_up;

    //  Constructor

    millDisplay() {
        textFont = new Font("Courier", Font.PLAIN, 12);
        setFont(textFont);
        textFontM = getFontMetrics(getFont());
    }

    //  print  --  Print string at given character position

    public void print(int y, int x, String s) {
        gee.drawString(s, x * textFontM.getMaxAdvance(),
                          textFontM.getAscent() + (y * textFontM.getHeight()));
    }

    //  biz  --  Return 0 if a BigInt is not defined

    private String biz(BigInt x) {
        BigInt b = x == null ? BigInt.ZERO : x;

        return Util.padNum(b);
    }

    //  PAINT  --  Paint the component window
    
    public void paint(Graphics g) {
        Dimension size = size();

        gee = g;
        g.setColor(Color.lightGray);
        g.fillRect(0, 0, size.width, size.height);
        g.setColor(Color.black);

        print(0, 0, "Operation: " + operation + 
            "   Run-up: " + (run_up ? "Set    " : "Not set") +
            (running ? "   Running" : "   Stopped"));
        print(1, 0, "Ingress 0:  " + biz(ingress[0]));
        print(2, 0, "        0': " + biz(ingress[2]));
        print(3, 0, "        1:  " + biz(ingress[1]));
        print(4, 0, "Egress  0:  " + biz(egress[0]));
        print(5, 0, "        0': " + biz(egress[1]));
    }

    //  PREFERREDSIZE  --  Indicate how large we'd like to be, given our druthers

    public Dimension preferredSize() {
        return new Dimension(textFontM.getMaxAdvance() * 46,
                             (textFontM.getHeight() +
                              textFontM.getLeading()) * 6);
    }

    public void changeIngress(int which, BigInt v) {
        ingress[which] = v;
        repaint();
    }

    public void changeEgress(int which, BigInt v) {
        egress[which] = v;
        repaint();
    }

    //  Change in the run up lever

    public void changeRunUp(boolean runup) {
        run_up = runup;
        repaint();
    }

    //  Change in current operation in  the mill

    public void changeOperation(String op) {
        operation = op;
        repaint();
    }

    //  Change to the run/stop state of the mill

    public void changeMillRunning(boolean run) {
        running = run;
        repaint();
    }                                  
}

//  Store display component

class storeDisplay extends Canvas {
    Font textFont, textFontB;
    FontMetrics textFontM;
    Graphics gee;
    int lastChange = -1;
    Vector storeVector = null;
    String [] cannedStrings = new String[1000];

    //  Constructor

    storeDisplay() {
        textFont = new Font("Courier", Font.PLAIN, 12);
        textFontB = new Font("Courier", Font.BOLD, 12);
        setFont(textFont);
        textFontM = getFontMetrics(getFont());
    }

    //  print  --  Print string at given character position

    public void print(int y, int x, String s) {
        gee.drawString(s, x * textFontM.getMaxAdvance(),
                          textFontM.getAscent() + (y * textFontM.getHeight()));
    }

    //  PAINT  --  Paint the component window
    
    public void paint(Graphics g) {
        Dimension size = size();
        int i, j = 0;

        gee = g;
        if (storeVector != null) {
            gee.setColor(Color.black);

            for (i = 0; i < storeVector.size(); i++) {
                String s = Integer.toString(i), t;
                BigInt b = (BigInt) storeVector.elementAt(i);

                if (b != null) {
                    while (s.length() < 3) {
                        s = "0" + s;
                    }
                    if (i == lastChange) {
                        gee.setFont(textFontB);
                    }
                    if ((t = cannedStrings[i]) == null) {
                        t = cannedStrings[i] = Util.padNum(b);
                    }
                    print(j, 0, s + ": " + t);
                    j++;
                    if (i == lastChange) {
                        gee.setFont(textFont);
                    }
                }
            }
        }
    }

    //  PREFERREDSIZE  --  Indicate how large we'd like to be, given our druthers

    public Dimension preferredSize() {
        return new Dimension(textFontM.getMaxAdvance() * 60,
                             (textFontM.getHeight() + textFontM.getLeading()) * 16);
    }

    //  Change to a column in the Store

    public void changeStoreColumn(int which, Vector v) {
        lastChange = which;
        storeVector = v;
        if (which == -1) {
            cannedStrings = new String[1000];
        } else {
            cannedStrings[which] = null;
        }
        repaint();
    }
}

//  Card reader display

class cardReaderDisplay extends Canvas {
    Font textFont;
    FontMetrics textFontM;
    Vector cardChain = null;
    int currentCard = 0, theight;
    static final int NLINES = 6;

    //  Constructor

    cardReaderDisplay() {
        textFont = new Font("Courier", Font.PLAIN, 12);
        setFont(textFont);
        textFontM = getFontMetrics(getFont());
        theight = textFontM.getHeight();
    }

    //  PAINT  --  Paint the component window
    
    public void paint(Graphics g) {
        Dimension size = size();
        int nl = size.height / theight;
        int l = Math.max(currentCard - (nl / 2), 0), i,
            ncards = cardChain.size();

        g.setColor(Color.blue);
        g.fillRect(0, 0, size.width, size.height);

        for (i = 0; i < nl; i++) {
            if (l >= ncards) {
                break;
            }
            if (l == currentCard) {
                g.setColor(Color.yellow);
                g.fillRect(0, i * theight, size.width, theight);
            }
            g.setColor(l == currentCard ? Color.blue : Color.white);
            g.drawString(((Card) cardChain.elementAt(l)).toString(),
                0, textFontM.getAscent() + (i * theight));
            l++;
        }
    }

    //  PREFERREDSIZE  --  Indicate size we'd like

    public Dimension preferredSize() {
        return new Dimension(textFontM.getMaxAdvance() * 70,
                             (textFontM.getHeight() + textFontM.getLeading()) * NLINES);
    }

    public void newCardChain(Vector v) {
        cardChain = v;
        newCard(0);
    }

    public void newCard(int n) {
        currentCard = n;
        repaint();
    }
}

/*  Annunciator display panel

    This panel includes the Mill, Card Reader, and Store displays.
    It is initially hidden, and usually displayed when the user
    wishes to animate or manually crank the engine.

*/

class annunciatorDisplay extends Frame {
    Component mdc, crc, sdc;

    //  Constructor

    annunciatorDisplay(String where, Component md, Component cr, Component st) {
        super();

        GridBagLayout gribble = new GridBagLayout();
        GridBagConstraints gc = new GridBagConstraints();
        Label lbl;
        graphicalLabel glbl;
        int cy = 0;

        /*  Guess what?  Appletviewer (at least on JDK 1.0 SGI) returns
            the file name in a getCodeBase URL, in blatant violation of
            the API documentation.  So, if the name doesn't end with a
            slash, slash the end off the "where" string.  */

        if (!where.endsWith("/")) {
            where = where.substring(0, where.lastIndexOf('/') + 1);
        }

        setLayout(gribble);
        setResizable(true);
        setTitle("Annunciator Panel");
        mdc = md;
        crc = cr;
        sdc = st;

        //  Mill display

        gc.fill = GridBagConstraints.BOTH;
        gc.insets.right = 4;
        gc.weightx = 1;
        gc.gridx = 0;
        gc.gridy = cy;
        gc.gridwidth = 2;
        glbl = new graphicalLabel(where + "Lmill.gif", "Mill", 0, 5);
        gribble.setConstraints(glbl, gc);
        add(glbl);
        cy++;
        gc.gridy = cy;
        gc.weighty = 0;
        gribble.setConstraints(mdc, gc);
        add(mdc);
        cy++;

        //  Card reader

        gc.gridy = cy;
        gc.weighty = 0;
        glbl = new graphicalLabel(where + "Lcardr.gif", "Card Reader", 0, 5);
        gribble.setConstraints(glbl, gc);
        add(glbl);
        cy++;
        gc.gridy = cy;
        gc.weighty = 1;
        gribble.setConstraints(crc, gc);
        add(crc);
        cy++;

        //  Store

        gc.gridy = cy;
        gc.weighty = 0;
        glbl = new graphicalLabel(where+ "Lstore.gif", "Store", 0, 5);
        gribble.setConstraints(glbl, gc);
        add(glbl);
        cy++;
        gc.gridy = cy;
        gc.weighty = 2;
        gribble.setConstraints(sdc, gc);
        add(sdc);

        pack();
    }
}

//  Printer output display

class printerDisplay extends TextArea {

    //  Constructor

    printerDisplay() {
        super(8, 80);
        setFont(new Font("Courier", Font.PLAIN, 12));
    }

}

//  Attendant remarks display

class attendantDisplay extends TextArea {

    //  Constructor

    attendantDisplay() {
        super(6, 80);
    }
}

//  Subclass AnnunciatorPanel to display events posted there

class AnimatedAnnunciatorPanel extends AnnunciatorPanel {
    annunciatorDisplay annunciatorD = null;
    millDisplay millD = null;
    storeDisplay storeD = null;
    cardReaderDisplay cardD = null;
    printerDisplay printerD = null;
    attendantDisplay attendantD = null;
    boolean tracing = false, animating = false, watch;
    AudioClip sound = null;

    void setDisplayPanels(Component annunciator,
                          Component mill, Component store,
                          Component cards, Component printer,
                          Component attendant) {
        annunciatorD = (annunciatorDisplay) annunciator;
        millD = (millDisplay) mill;
        storeD = (storeDisplay) store;
        cardD = (cardReaderDisplay) cards;
        printerD = (printerDisplay) printer;
        attendantD = (attendantDisplay) attendant;
    }

    public void setBellSound(AudioClip s) {
        sound = s;
    }

    public void attendantLogMessage(String s) {
        attendantD.appendText(s);
    }

    private void watchMan() {
        watch = tracing || animating || annunciatorD.isShowing();
    }

    public void setTrace(boolean t) {
        tracing = t;
        watchMan();
    }

    public void setAnimate(boolean t) {
        animating = t;
        watchMan();
    }

    public void setPanelShowing(boolean t) {
        annunciatorD.show(t);
        watchMan();
    }

    public void mountCardReaderChain(Vector v) {
        if (cardD != null) {
            cardD.newCardChain(v);
        }
    }

    public void changeCardReaderCard(Card c, int cardNumber) {
        if (watch) {
            cardD.newCard(cardNumber);
        }
    }

    public void changeIngress(int which, BigInt v) {
        if (millD != null && watch) {
            millD.changeIngress(which, v);
        }
    }

    public void changeEgress(int which, BigInt v) {
        if (millD != null && watch) {
            millD.changeEgress(which, v);
        }
    }

    //  Change in the run up lever

    public void changeRunUp(boolean run_up) {
        if (millD != null && watch) {
            millD.changeRunUp(run_up);
        }
    }

    //  Change in current operation in the mill

    public void changeOperation(String op) {
        if (millD != null && watch) {
            millD.changeOperation(op);
        }
    }

    //  Change to the run/stop state of the mill

    public void changeMillRunning(boolean running, String message) {
        if (millD != null && watch) {
            millD.changeMillRunning(running);
        }
        if (!running) {
            if (message != null && (message = message.trim()).length() > 0) {
                attendantD.appendText("Halt: " + message + "\n");
            }
        }
    }                                  

    //  Ring the bell

    public void ringBell() {
        if (sound != null) {
            sound.play();
        }
    }

    //  Change to a column in the Store

    public void changeStoreColumn(int which, Vector v) {
        if (storeD != null && watch) {
            storeD.changeStoreColumn(which, v);
        }
    }
}

//  Subclass PrintingApparatus to pass output to printer field

class AnimatedPrintingApparatus extends PrintingApparatus {
    printerDisplay printer;

    //  Constructor

    AnimatedPrintingApparatus(printerDisplay d) {
        super();
        printer = d;
    }

    //  Print a string

    void Output(String s) {
        printer.appendText(s);
    }
}

/*  Special Panel to load on CR in file name/URL field

    This can be used in any situation where you want a
    carriage return input to a text field to press a
    related action button.  The only restriction is that
    only one text field and associated button can be
    defined per panel.  If you need more, simply place
    each in its own loadPanel, and collect the loadPanels
    into a larger composite panel.

*/

class loadPanel extends Panel {
    Component tf, button;
    aeapp a;

    loadPanel(aeapp xa, Component xtf, Component xbutton) {
        super();
        a = xa;
        tf = xtf;
        button = xbutton;
    }

    public boolean keyDown(Event evt, int key) {
        if (evt.target == tf && (key == 10 || key == 13)) {
            evt.target = button;
            a.action(evt, null);
            return true;
        }
        return false;
    }
}

//      The Analytical Engine Animator Applet

public class aeapp extends Applet implements Runnable {
    static String appName = "aeapp";
    static String Version = "Version 1.0";
    final static int Mode_constant = 3;

    /*  If you have a default path for library items, specify it in
        the following declaration (and don't forget to include the
        "@@" where the file name goes).  Set this to null if you
        have no default library path.  If the path begins with
        "http:", it is taken to be the URL from from which library
        items are served.  This allows Web sites to provide libraries
        to clients.  */

    static String defaultLibraryTemplate = "http://www.fourmilab.ch/babbage/Library/@@.ae";

    Thread animator;
    Button reset_engine, start_stop, single_step, clear_printer,
           clear_attendant, load_program, clear_program, load_program_file;
    TextField programFileName;
    Checkbox view_panel,
             tracing,
             animate,
             comments = new Checkbox("Keep comments", null, true);
    boolean error = false, running = false, quitIt = false, animating = false,
            showPanel = false, doTrace = false;
    Component millD, storeD, cardD, printerD, attendantD;
    annunciatorDisplay annunciatorD;
    WebFrameCurveDrawingApparatus curveD;
    TextArea programD;
    String initialCards = null, initialURL = null;
    AnimatedAnnunciatorPanel panel = new AnimatedAnnunciatorPanel();
    AnimatedPrintingApparatus printer;
    analyticalEngine e = new analyticalEngine(panel);
    String audioFile = "gong.au";
    AudioClip sound;

    //  Parameters from applet invocation

    int mode = Mode_constant;
    boolean silent, show_version;

    //  RUN  --  Main thread processing

    public void run() {
        while (true) {
            if (running) {
                if (quitIt) {
                    running = false;
                } else {
                    if (!e.error()) {
                        int snooze = 0;

                        e.setTrace(tracing.getState());
                        ((millDisplay) millD).changeMillRunning(true);
                        while (running && e.processCard() && !e.error()) {
                            if (animating) {
                                try {
                                    animator.sleep(500);
                                } catch (InterruptedException e) {
                                    return;
                                }
                            } else {
                                snooze++;
                                if (snooze > 50) {
                                    snooze = 0;
                                    try {
                                        animator.sleep(10);
                                    } catch (InterruptedException e) {
                                        return;
                                    }
                                }
                            }
                        }
                        ((millDisplay) millD).changeMillRunning(false);
                    }
                    running = false;
                    start_stop.setLabel(" Start ");
                }
            }
            try {
                animator.sleep(250);
            } catch (InterruptedException e) {
                return;
            }
        }
    }

    /*  GETPARAMETERINFO  --  Standard applet method that returns an
                              array of arrays of strings describing the
                              parameters this applet accepts.  We append
                              any parameters accepted by the visual
                              feedback program to the end of the list.
                              In doing so we make the assumption that
                              init() will always be called before
                              getParameterInfo().  */

    public String[][] getParameterInfo() {
        String[][] info = {
            { "Animate", "y/n", "Animate?  Default n" },
            { "Cards", "String", "Initial program cards.  Default none" },
            { "Panel", "y/n", "Show annunciator panel?  Default n" },
            { "Program", "String", "URL for initial program.  Default none" },
//          { "Sound", "-s/-n", "Sound for crank turn?  Default -n" },
            { "Trace", "y/n", "Trace calculation?  Default n" },
        };
        String[][] merged = new String[info.length][3];
        int i;

        System.arraycopy(info, 0, merged, 0, info.length);
        return merged;
    }

    /*  GETAPPLETINFO  --  Return information about this applet.  */

    public String getAppletInfo() {
        return "Analytical Engine Animator " +
               " applet by John Walker.  Public domain.";
    }

    /*  PROCESS_PARAMETERS  --  Process applet parameters.  All parameters
                                are kept in instance variables of the
                                applet.  */

    protected void process_parameters() {
        String s;

        //  Load an initial set of cards ?

        initialCards = getParameter("Cards");

        //  Load initial cards from a URL ?

        initialURL = getParameter("Program");

        //  Animate by default ?

        s = getParameter("Animate");
        if (s != null && s.equalsIgnoreCase("y")) {
            animating = true;
        }

        //  Show Panel by default ?

        s = getParameter("Panel");
        if (s != null && s.equalsIgnoreCase("y")) {
            showPanel = true;
        }

        //  Trace by default ?

        s = getParameter("Trace");
        if (s != null && s.equalsIgnoreCase("y")) {
            doTrace = true;
        }

        //  Generate sound ?

        s = getParameter("Sound");
        if (s == null) {
            s = "Silent";
        }
        silent = !s.equalsIgnoreCase("-s");
    }


    //  INIT  --  Applet is loaded--create our objects and display them

    public void init() {
        int i, cy = 0;
        String s;
        GridBagLayout gribble = new GridBagLayout();
        GridBagConstraints gc = new GridBagConstraints();
        Label lbl;
        Panel p;

        setLayout(gribble);
        process_parameters();         // Parse parameters and set mode variables

        //  Initialise the Engine emulator

        e.setLibraryTemplate(defaultLibraryTemplate);
        e.allowFileInclusion(false);
        e.setCurveDrawingApparatus((CurveDrawingApparatus)
                    (curveD = new WebFrameCurveDrawingApparatus()));

        //  Create display components

        millD = new millDisplay();
        cardD = new cardReaderDisplay();
        storeD = new storeDisplay();
        annunciatorD = new annunciatorDisplay(getCodeBase().toString(), millD, cardD, storeD);

        programD = new TextArea(10, 80);
        programD.setFont(new Font("Courier", Font.PLAIN, 12));
        programD.setText(initialCards);
        printerD = new printerDisplay();
        attendantD = new attendantDisplay();
        printer = new AnimatedPrintingApparatus((printerDisplay) printerD);

        //  Notify Engine of non-default components

        e.setPrinter(printer);

        //  Pass display components to annunciator panel

        panel.setDisplayPanels(annunciatorD, millD, storeD, cardD, printerD, attendantD);
        sound = getAudioClip(getCodeBase(), audioFile);
        panel.setBellSound(sound);

        reset_engine = new Button(" Reset ");
        start_stop = new Button(" Start ");
        single_step = new Button(" Step ");


        //  Add the various components to the applet window

        //  Mill display
        gc.fill = GridBagConstraints.BOTH;
        gc.insets.right = 4;
        gc.weightx = 1;
        gc.gridx = 0;
        gc.gridy = cy;
        gc.gridwidth = 2;

        //  Analyst's program

        p = new Panel();
        p.setLayout(new FlowLayout(FlowLayout.LEFT));
        gc.gridy = cy;
        gc.weighty = 0;
        lbl = new Label("Analyst's Program");
        p.add(lbl);
        load_program = new Button(" Submit ");
        p.add(load_program);
        clear_program = new Button(" Clear ");
        p.add(clear_program);
        p.add(comments);
        gribble.setConstraints(p, gc);
        add(p);
        cy++;
        gc.gridy = cy;
        load_program_file = new Button(" Load ");
        programFileName = new TextField("http://www.fourmilab.ch/babbage/websim/", 65);
        p = new loadPanel(this, programFileName, load_program_file);
        p.setLayout(new FlowLayout(FlowLayout.LEFT));
        p.add(load_program_file);
        p.add(programFileName);
        gribble.setConstraints(p, gc);
        add(p);
        cy++;
        gc.insets.right = 4;
        gc.gridy = cy;
        gc.weighty = 1;
        gribble.setConstraints(programD, gc);
        add(programD);
        cy++;

        //  Printer output

        p = new Panel();
        p.setLayout(new FlowLayout(FlowLayout.LEFT));
        gc.gridy = cy;
        gc.weighty = 0;
        lbl = new Label("Printer");
        p.add(lbl);
        clear_printer = new Button(" Erase ");
        p.add(clear_printer);
        gribble.setConstraints(p, gc);
        add(p);
        cy++;
        gc.insets.right = 4;
        gc.gridy = cy;
        gc.weighty = 1;
        gribble.setConstraints(printerD, gc);
        add(printerD);
        cy++;

        //  Attendant comments output

        p = new Panel();
        p.setLayout(new FlowLayout(FlowLayout.LEFT));
        gc.gridy = cy;
        gc.weighty = 0;
        lbl = new Label("Attendant's Log");
        p.add(lbl);
        clear_attendant = new Button(" Erase ");
        p.add(clear_attendant);
        gribble.setConstraints(p, gc);
        add(p);
        cy++;
        gc.gridy = cy;
        gc.weighty = 1;
        gribble.setConstraints(attendantD, gc);
        add(attendantD);
        cy++;

        //  Simulator control panel

        p = new Panel();

        view_panel = new Checkbox("Panel", null, showPanel);
        panel.setPanelShowing(showPanel);
        p.add(view_panel);
        tracing = new Checkbox("Trace", null, doTrace);
        p.add(tracing);
        animate = new Checkbox("Animate", null, animating);
        panel.setAnimate(animating);
        p.add(animate);
        p.add(reset_engine);
        p.add(single_step);
        p.add(start_stop);

        gc.insets.top = 8;
        gc.gridx = 0;
        gc.gridy = cy;
        gc.weighty = 0;
        gc.anchor = GridBagConstraints.CENTER;
        gc.fill = GridBagConstraints.REMAINDER;
        gribble.setConstraints(p, gc);
        add(p);
        cy++;

        //  Load initial cards if supplied by a parameter

        if (initialCards != null) {
            e.loadCardsFromString(initialCards);
            if (!e.error()) {
                e.scrutinise(true);
            }
            if (!e.error()) {
                e.commence();
            }
        }

        //  Load initial cards if supplied from a URL

        if (initialURL != null) {
            programFileName.setText(initialURL);
/*
            e.loadCardsFromFile(initialURL);
            if (!e.error()) {
                e.scrutinise(true);
            }
            if (!e.error()) {
                e.commence();
            }
*/
            action(new Event(load_program_file, 0, null), null);
        }

        animator = new Thread(this);
    }

    //  ACTION  --  Respond to events from child components

    public boolean action(Event ev, Object arg) {

        //  Start / stop continuous running

        if (ev.target == start_stop) {
            start_stop.setLabel(running ? " Start " : " Pause ");
            running = !running;
            return true;

        //  Step: process next card

        } else if (ev.target == single_step) {
            if (running) {
                start_stop.setLabel(" Start ");
                running = false;
            } else {
                panel.setAnimate(true);
                e.setTrace(tracing.getState());
                e.processCard();
                panel.setAnimate(animate.getState());
            }

        //  Reset engine, back up to first card

        } else if (ev.target == reset_engine) {
            if (running) {
                start_stop.setLabel(" Start ");
                running = false;
            }
            panel.setAnimate(true);
            e.reset();
            e.commence();
            panel.setAnimate(animate.getState());

        //  Clear the printer display

        } else if (ev.target == clear_printer) {
            ((printerDisplay) printerD).setText("");

        //  Clear the attendant display

        } else if (ev.target == clear_attendant) {
            ((attendantDisplay) attendantD).setText("");

        //  Submit analyst's program to attendant for computation

        } else if (ev.target == load_program) {
            e.loadNewCards();
            e.loadCardsFromString(programD.getText());
            if (!e.error()) {
                e.scrutinise(comments.getState());
            }
            if (!e.error()) {
                e.commence();
            }
//if (e.error()) { System.out.println("Submit error"); }

        //  Load a new program from a file or URL

        } else if (ev.target == load_program_file) {
            String cs;

            /*  This needs to be wrapped in a flak-catcher
                exception handler because the user may enter a
                URL (for example a local file: URL) which causes
                a security exception.  Since applet security
                exceptions are not part of the Java core, we
                must catch them at this level rather than the
                lower level which handles bad file names, I/O
                errors, etc.  */

            try {
                cs = e.obtainCardString(programFileName.getText());
            } catch (Exception e) {
                panel.attendantLogMessage("I am not permitted to access " +
                    programFileName.getText() + "\n");
                cs = null;
            }

            /*  To avoid confusion, if the cards were successfully
                loaded, they are submitted to the Attendant for
                mounting in the Card Reader.  */

            if (cs != null) {
                programD.setText(cs);
                ev.target = load_program;
                action(ev, arg);
            }

        } else if (ev.target == clear_program) {
            programD.setText("");
            e.loadNewCards();

        //  Change tracing state

        } else if (ev.target == tracing) {
            e.setTrace(tracing.getState());
            panel.setTrace(tracing.getState());

        //  Change animation state

        } else if (ev.target == animate) {
            animating =  animate.getState();
            panel.setAnimate(animating);

        //  Toggle whether store is visible

        } else if (ev.target == view_panel) {
            panel.setPanelShowing(view_panel.getState());
        }
        return false;
    }

    //  START  --  Applet loading complete; start animation thread

    public void start() {
        if (animator != null) {
            if (animator.isAlive()) {
                animator.resume();
            } else {
                animator.start();
            }
        }
    }

    //  STOP  --  We're leaving this page--stop animation thread

    public void stop() {
        if (animator != null) {
            animator.suspend();
        }
        view_panel.setState(false);
        panel.setPanelShowing(false);
        curveD.changePaper();
    }

    //  DESTROY  --  Endsville--destroy the animation thread

    public void destroy() {
        if (animator != null) {
            animator.stop();
        }
        if (annunciatorD != null) {
            annunciatorD.dispose();
        }
        curveD.dispose();
    }

    //  BOMB  --  Show error message and abort

    private void bomb(String err) {
        add(new Label(appName + " applet error:\n"));
        add(new Label(err));
        showStatus(appName + " applet error: " + err);
        error = true;
    }
}
