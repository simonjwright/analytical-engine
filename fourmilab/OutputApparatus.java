/*

    Output Apparatus

    This class is parent to the Card Punching, Printing,
    and Copper Plate Punching Apparatuses.

*/

abstract class OutputApparatus {
    AnnunciatorPanel panel;
    Attendant attendant;

    /*  Output a string.  The new line character denotes the
        end of a record, and may occur in the middle of the
        string.  */

    abstract void Output(String s);

    /*  Provide references to the annunciator panel and
        attendant for Apparatus implementations which may need to
        communicate with them.  */

    public void setPanelAndAttendant(AnnunciatorPanel p, Attendant a) {
        panel = p;
        attendant = a;
    }

}
