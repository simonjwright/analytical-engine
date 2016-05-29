/*
    The Curve Drawing Apparatus

    This class is parent to all implementations which actually
    do anything.

*/

import java.awt.Color;

class CurveDrawingApparatus {
    AnnunciatorPanel panel;
    Attendant attendant;
    boolean initdone = false;
    BigInt px = BigInt.ZERO, py = BigInt.ZERO;
    Color penColour = Color.black;

    CurveDrawingApparatus(AnnunciatorPanel p, Attendant a) {
        setPanelAndAttendant(p, a);
    }

    CurveDrawingApparatus() {
        panel = null;
        attendant = null;
    }

    public void setPanelAndAttendant(AnnunciatorPanel p, Attendant a) {
        panel = p;
        attendant = a;
    }

    boolean initialised() {
        return initdone = true;
    }

    public synchronized void dispose() {
    }

    public void setX(BigInt x) {
        px = x;
    }

    public void setY(BigInt y) {
        py = y;
    }

    //  moveTo  --  Move, with the pen up, to the current co-ordinates

    public void moveTo() {
    }

    //  drawTo  --  Draw, with the pen down, to the current co-ordinates

    public void drawTo() {
    }

    //  changePen  --  Change the pen for one of a different colour

    public void changePen(Color cpen) {
        penColour = cpen;
    }

    //  changePaper --  Start a new plot on a new sheet of paper

    public void changePaper() {
    }
}
