/*

                          Annunciator Panel

    The annunciator panel comprises all means by which the Analytical
    Engine notifies its attendant of its current state and work in
    progress.  Whenever a relevant change occurs, the panel is
    notified so that the change can be displayed.

    This is the default annunciator panel, which does nothing.  This
    is used as a superclass by panels which display the current state
    in various ways.

*/

import java.util.*;

class AnnunciatorPanel {

    //                    MILL NOTIFICATIONS

    //  Changes of state in one of the Mill's axes

    public void changeIngress(int which, BigInt v) {
    }

    public void changeEgress(int which, BigInt v) {
    }

    //  Change in the run up lever

    public void changeRunUp(boolean run_up) {
    }

    //  Change in current operation in  the mill

    public void changeOperation(String op) {
    }

    //  Change to the run/stop state of the mill

    public void changeMillRunning(boolean running, String message) {
        if (!running) {
            if (message != null && (message = message.trim()).length() > 0) {
                attendantLogMessage("Halt: " + message + "\n");
            }
        }
    }                                  
    public void changeMillRunning(boolean running) {
        changeMillRunning(running, "");
    }                                  

    //  Ring the bell

    public void ringBell() {
        System.out.print("\007");
        System.out.flush();
    }

    //                   STORE NOTIFICATIONS

    //  Change to a column in the Store

    public void changeStoreColumn(int which, Vector v) {
    }

    //                CARD READER NOTIFICATIONS

    /*  Mount a new card chain.  When a chain is dismounted
        and the card reader is reset, this is called with a
        null argument.  Note that the card reader is reset
        before the first card chain is mounted.  */

    public void mountCardReaderChain(Vector v) {
    }

    //  Turn to a new card

    public void changeCardReaderCard(Card c, int cardNumber) {
    }

    //                 ATTENDANT NOTIFICATIONS

    //  Log a communication from the Attendant

    public void attendantLogMessage(String s) {
        System.out.print(s);
        System.out.flush();
    }

    //  Write an item into the mill operation trace

    public void attendantWriteTrace(String s) {
        attendantLogMessage(s);
    }
}
