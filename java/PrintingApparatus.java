/*

    The Printing Apparatus

    This class is parent to all fancier implementations of printers.
    It just prints on standard output.

*/

class PrintingApparatus extends OutputApparatus {

    //  Print a string

    void Output(String s) {
        System.out.print(s);
    }
}
