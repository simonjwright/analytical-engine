
import java.util.*;

//  The Store

class Store {
    private Vector rack;              // Rack of variable columns
    AnnunciatorPanel panel;
    Attendant attendant;
    private boolean trace = false;

    public Store(AnnunciatorPanel p, Attendant a) {
        panel = p;
        attendant = a;
        reset();
    }

    public void set(int which, BigInt v) {
        rack.setElementAt(v, which);
        panel.changeStoreColumn(which, rack);
        if (trace) {
            attendant.traceLog("Store: V" + which + " = " + v);
        }
    }

    public void set(int which, long v) {
        set(which, new BigInt(v));
    }

    public BigInt get(int which) {
        BigInt v;

        if (rack.elementAt(which) == null) {
            set(which, 0);
        }
        v = (BigInt) rack.elementAt(which);
        if (trace) {
            attendant.traceLog("Store: Mill <= V" + which + "(" + v + ")");
        }
        return v;
    }

    public void reset() {
        rack = new Vector(1000);
        rack.setSize(1000);
        panel.changeStoreColumn(-1, rack);
    }

    public void setTrace(boolean t) {
        trace = t;
    }
}
