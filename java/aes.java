/*

                     Analytical Engine Simulator

    This is a command-line program which loads one or more chains of
    cards from files named on the command line, then (unless inhibited
    by a command line option), starts the Engine and displays the
    results on standard output.  See the how-to-call information
    in the usage() method for a list of available options.

    This program runs on a user's machine under a local copy of a
    Java interpreter.  Hence, all server security options are
    disabled, as only local file access is possible.

*/

public class aes {

    /*  If you have a default path for library items, specify it in
        the following declaration (and don't forget to include the
        "@@" where the file name goes).  Set this to null if you
        have no default library path.  This can be overridden by
        an -sTemplate option which appears before the file which
        contains the library request.  */

    static String defaultLibraryTemplate = "/ftp/babbage/Library/@@.ae";

    static void up(String s) {
        System.out.println(s);
    }

    static void usage() {
        up("Options:");
        up("  -c    Don't mount comment cards");
        up("  -l    List program as mounted by attendant");
        up("  -n    No execution of program");
        up("  -p    Punch copy of program as mounted by attendant");
        up("  -sLST Use LST as library search template");
        up("  -t    Print trace of program execution");
        up("  -u    Print this message");
    }

    public static void main(String argv[]) {
        AnnunciatorPanel panel = new AnnunciatorPanel();
        analyticalEngine e = new analyticalEngine(panel);
        int i;
        boolean listing = false, execute = true, trace = false,
                punched = false, comments = true;

        e.setLibraryTemplate(defaultLibraryTemplate);
        e.allowFileInclusion(true);

        for (i = 0; i < argv.length; i++) {
            if (argv[i].charAt(0) == '-') {
                if (argv[i].length() > 1) {
                    switch (argv[i].charAt(1)) {
                        case 'c':
                            comments = false;
                            break;

                        case 'l':
                            listing = true;
                            break;

                        case 'n':
                            execute = false;
                            break;

                        case 'p':
                            listing = punched = true;
                            break;

                        case 's':
                            e.setLibraryTemplate(argv[i].length() < 3 ?
                                null : argv[i].substring(2));
                            break;

                        case 't':
                            trace = true;
                            break;

                        case 'u':
                            usage();
                            return;
                    }
                }
            } else {
                e.loadCardsFromFile(argv[i]);
                if (e.error()) {
                    break;
                }
            }
        }

/*
    Gratuitous test case for loadCardsFromString

e.loadCardsFromString(
    "A write in rows\n" +
    "A write numbers as 9\n" +
    "N120 10000\n" +
    "N121 3\n" +
    "/\n" +
    "L120\n" +
    "L121\n" +
    "S122'\n" +
    "P\n"
);
/**/

        if (!e.error()) {
            e.setCurveDrawingApparatus((CurveDrawingApparatus) new
                    frameCurveDrawingApparatus());
            e.scrutinise(comments);

            if (listing) {
                e.dumpCards(punched);
            }

            if (!e.error() && execute) {
                e.setTrace(trace);
                e.run();
            }
        }
    }
}
