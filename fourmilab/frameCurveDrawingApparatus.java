
//  Frame (stand-alone application window) Curve Drawing Apparatus

import java.awt.*;
import java.util.*;

class curvePlot extends Canvas {
    AnnunciatorPanel panel;
    Attendant attendant;
    Polygon p = null;
    Dimension s;
    private final static BigInt
        K10e25 = BigInt.valueOf("10000000000000000000000000"),
        Kround = BigInt.valueOf( "5000000000000000000000000");
    Vector pvect = new Vector();
    boolean isPenDown = true;
    Color penColour = Color.black;
    int ax, ay;

    //  Constructor

    curvePlot(AnnunciatorPanel p, Attendant a) {
        panel = p;
        attendant = a;
    }

    private static final int scaleNum(BigInt v, int scale) {
//System.out.println("Scale in: " + v + " scale = " + scale);
        v = v.add(v.multiply(scale / 2), Kround.multiply(v.test()));
//System.out.println(v);
        v = BigInt.quotient(v, K10e25);
//System.out.println(v);
//System.out.println("Pixel: " + (v.intValue() + (scale / 2)));
        return v.intValue() + (scale / 2);
    }

    public void paint(Graphics g) {
        Enumeration e = pvect.elements();

        g.setColor(penColour);
        while (e.hasMoreElements()) {
            g.drawPolygon((Polygon) e.nextElement());
        }
    }

    public void penDown() {
        isPenDown = true;
    }

    public void penUp() {
        if (isPenDown) {
            p = null;
        }
    }

    public void addPoint(BigInt px, BigInt py) {
        int ax, ay;

        s = size();
        ax = scaleNum(px, s.width);
        ay = s.height - scaleNum(py, s.height);
        if (isPenDown) {
            if (p == null) {
                p = new Polygon();
                pvect.addElement(p);
                p.addPoint(ax, ay);
            }
            p.addPoint(ax, ay);
            invalidate();
        }
    }

    public void setPenColour(Color c) {
        penColour = c;
    }
}

class frameCurveDrawingApparatus extends CurveDrawingApparatus {
    Frame f = null;
    curvePlot cp;

    frameCurveDrawingApparatus(AnnunciatorPanel p, Attendant a) {
        super(p, a);
    }

    frameCurveDrawingApparatus() {
        super();
    }

    boolean initialised() {
        if (f == null) {
            f = new Frame("Curve Drawing Apparatus");
            f.add("Center", cp = new curvePlot(panel, attendant));
            cp.resize(400, 400);
            f.pack();
            f.show();
        }
        return super.initialised();
    }

    public void moveTo() {
        if (initialised()) {
            cp.penUp();
//          cp.addPoint(px, py);
        }
    }

    public void drawTo() {
        if (initialised()) {
            cp.penDown();
            cp.addPoint(px, py);
            cp.repaint();
        }
    }

    public void changePen(Color c) {
        if (initialised()) {
            cp.setPenColour(c);
        }
    }

    public void changePaper() {
        if (f != null) {
            f.dispose();
            f = null;
        }
    }
}
