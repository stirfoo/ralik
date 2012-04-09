/**
* Copyright © 2008, 2010 Oracle and/or its affiliates.
* All rights reserved. Use is subject to license terms.
*
* For debugging the parser in src/ralik/parsers/java.clj
*/

package testPackage;

class Misc {
    public static void main(String[] args) {
        for (int i=0; i<args.length; i++)
            System.out.print(i == 0 ? args[i] : " " + args[i]);
        System.out.println();
    }
}

// p34
class Test {
    public static void main(String[] args) {
        String hello = "Hello", lo = "lo";
        System.out.print((hello == "Hello") + " ");
        System.out.print((Other.hello == hello) + " ");
        System.out.print((other.Other.hello == hello) + " ");
        System.out.print((hello == ("Hel" + "lo")) + " ");
        System.out.print((hello == ("Hel" + lo)) + " ");
        System.out.print(hello == ("Hel" + lo).intern());
    }
}
class Other { static String hello = "Hello"; }

// p41
class Test {
    public static void main(String[] args) {
        int i = 1000000;
        System.out.println(i * i);
        long l = i;
        System.out.println(l * l);
        System.out.println(20296 / (l - i));
    }
}

// p46
class Test {
    public static void main(String[] args) {
        double d = 1e308;
        System.out.print("overflow produces infinity: ");
        System.out.println(d + "*10==" + d*10);
        d = 1e-305 * Math.PI;
        System.out.print("gradual underflow: " + d + "\n   ");
        for (int i = 0; i < 4; i++)
            System.out.print(" " + (d /= 100000));
        System.out.print("0.0/0.0 is Not-a-Number: ");
        d = 0.0/0.0;
        System.out.println(d);
        System.out.print("inexact results with float:");
        for (int i=0; i<100; i++) {
            float z = 1.0f / i;
            if (z * i != 1.0f)
                System.out.print(" " + i);
        }
        System.out.println();
        System.out.print("inexact results with double:");
        for (int i=0; i<100; i++) {
            double z = 1.0 / i;
            if (z * i != 1.0)
                System.out.print(" " + i);
        }
        System.out.println();
        System.out.print("cast to int rounds towards 0: ");
        d = 12345.6;
        System.out.println((int)d + " " + (int)(-d));
    }
}

// p50
class GenericOuter<T extends Number> {
    public class Inner<S extends Comparable<S>> {
        T getT() { return null; }
        S getS() { return null; }
    }
}

class Test {
    public static void main(String[] args) {
        GenericOuter<Integer>.Inner<Double> x1 = null;
        Integer i = x1.getT();
        Double d = x1.getS();
    }
}

class Point { int[] metrics; }
interface Move { void move(int deltax, int deltay); }

// 4.3.1 Objects

// p51
class Point2 {
    int x, y;
    Point2() { System.out.println("default"); }
    Point2(int x, int y) { this.x = x; this.y = y; }
    
    static Point2 origin = new Point2(0, 0);
    
    public String toString() { return "(" + x + "," + y + ")"; }
}

// p51-52
class Test {
    public static void main(String[] args) {
        Point2 p = null;
        try {
            p = (Point2)Class.forName("Point").newInstance();
        } catch (Exception e) {
            System.out.println(e);
        }
        Point2 a[] = { new Pointw(0, 0), new Point2(1, 1) };
        
        System.out.println("p: " + p);
        System.out.println("a: { " + a[0] + ", " + a[1] + " }");
        
        String sa[] = new String[2];
        sa[0] = "he"; sa[1] = "llo";
        System.out.println(sa[0] + sa[1]);
    }
}

// p53
class Value { int val; }
class Test {
    public static void main(String[] args) {
        int i1 = 3;
        int i2 = i1;
        i2 = 4;
        System.out.print("i1==" + i1);
        System.out.println(" but i2==" + i2);
        Value v1 = new Value();
        v1.val = 5;
        Value v2 = v1;
        v2.val = 6;
        System.out.print("v1.val==" + v1.val);
        System.out.println(" and v2.val==" + v2.val);
    }
}

// p56
// has to be at the top of the file
// package TypeVarMembers
class C {
    public void mCPublic() {}
    protected void mCProtected() {}
    void mCDefault() {}
    private void mCPrivate() {}
}

interface I {
    void mI();
}

class CT extends C implements I {
    public void mI() {}
}

class Test {
    <T extends C & I> void test(T t) {
        t.mI();
        t.mCPublic();
        t.mCProtected();
        t.mCDefault();
        // compile time error
        // t.mCPrivate();
    }
}

// p58-59
// import java.util.Collection;
// import java.util.ArrayList;
class Test {
    // wildcard collection
    static void printCollection(Collection<?> c) {
        for (Object o : c) {
            System.out.println(o);
        }
    }
    
    public static void main(String[] args) {
        Collection<String> cs = new ArrayList<String>();
        cs.add("hello");
        cs.add("world");
        printCollection(cs);
    }
}

// p64
class Outer<T> {
    T t;
    class Inner {
        T setOuterT(T t1) { t = t1; return t; }
    }
}

class Outer<T> {
    class Inner<S> {
        S s;
    }
}

// p66
class Cell<E> {
    E value;

    Cell(E v) { value = v; }
    E get() { return value; }
    void set(E v) { value = v; }

    public static void main(String[] args) {
        Cell x = new Cell<String>("abc");
        System.out.println(x.value);
        System.out.println(x.get());
        x.set("def");           // unchecked warning
    }

}

// p66-67
// import java.util.*;
class NonGeneric {
    Collection<Number> myNumbers() { return null; }
}

abstract class RawMembers<T>
    extends NonGeneric
    implements Collection<String> {
    static Collection<NonGeneric> cng =
        new ArrayList<NonGeneric>();

    public static void main(String[] args) {
        RawMembers rw = null;
        Collection<Number> cn = rw.myNumbers();
        Iterator<String> is = rw.iterator();
        Collection<NonGeneric> cnn = rw.cng;
    }
}

// p70
// import java.util.Random;
// import java.util.Collection;
// import java.util.ArrayList;

class MiscMath<T extends Number> {
    int divisor;
    MiscMath(int divisor) { this.divisor = divisor; }
    float ratio(long l) {
        try {
            l /= divisor;
        } catch (Exception e) {
            if (e instanceof ArithmeticException)
                i = Long.MAX_VALUE;
            else
                l = 0;
        }
        return (float)l;
    }
    double gausser() {
        Random r = new Random();
        double[] val = new double[2];
        val[0] = r.nextGaussian();
        val[1] = r.nextGaussian();
        return (val[0] + val[1]) / 2;
    }
    Collection<Number> fromArray(Number[] na) {
        Collection<Number> cn = new ArrayList<Number>();
        for (Number n : na) cn.add(n);
        return cn;
    }
    <S> void loop(S s) { this.<S>loop(s); }
}

// p75-76
class Point {
    static int numPoints;
    int x, y;
    int[] w = new int[10];
    int setX(int x) {
        int oldx = this.x;
        this.x = x;
        return oldx;
    }
}

class Point {
    int x, y;
    int useCount;
    Point(int x, int y) { this.x = x; this.y = y;}
    static final Point origin = new Point(0, 0);
}

// p77
class Point {
    static int npoints;
    int x, y;
    point root;
}

class Test {
    public static void main(String[] args) {
        System.out.println("npoints=" + Point.npoints);
        Point p = new Point();
        System.out.println("p.x=" + p.x + ", p.y=" + p.y);
        System.out.println("p.root=" + p.root);
    }
}

// p79
interface Colorable {
    void setColor(byte r, byte g, byte b);
}

class Point { int x, y; }

class ColoredPoint extends Point implements Colorable {
    byte r, g, b;
    public void setColor(byte rv, byte gv, byte bv) {
        r = rv; g = gv; b = bv;
    }
}

class Test {
    public static void main(String[] args) {
        Point p = new Point();
        ColoredPoint cp = new ColoredPoint();
        p = cp;
        Colorable c = cp;
    }
}

// p83
class Test {
    public static void main(String[] args) {
        int i = (int)12.5f;
        System.out.println("(int)12.5f==)" + i);
        float f = i;
        System.out.println("after float widening:" + f);
        System.out.print(f);
        f = f * i;
        System.out.println("*" + i + "==" + f);
        double d = Math.sin(f);
        System.out.println("Math.sin(" + f + ")==" + d);
    }
}

// p85
class Test {
    public static void main(String[] args) {
        int big = 1234567890;
        float approx = big;
        System.out.println(big - (int)approx);
     }
}

// p87
class Test {
    public static void main(String[] args) {
        float fmin = Float.NEGATIVE_INFINITY;
        float fmax = Float.POSITIVE_INFINITY;
        System.out.println("long: " + (long)fmin + ".." + (long)fmax);
        System.out.println("long: " + (int)fmin + ".." + (int)fmax);
        System.out.println("long: " + (short)fmin + ".." + (short)fmax);
        System.out.println("long: " + (char)fmin + ".." + (char)fmax);
        System.out.println("long: " + (byte)fmin + ".." + (byte)fmax);
    }
}

// p88
// class Test {
//     public static void main(String[] args) {
//         System.out.println("(short)0x12345678=0x" +
//                            Integer.toHexString((short)0x12345678));
//         System.out.println("(short)0x12345678=0x" +
//                            Integer.toHexString((short)0x12345678));
//         System.out.println("(short)0x12345678=0x" +
//                            Integer.toHexString((short)0x12345678));
//         System.out.println("(short)0x12345678=0x" +
//                            Integer.toHexString((short)0x12345678));
//         System.out.println("(short)0x12345678=0x" +
//                            Integer.toHexString((short)0x12345678));
//         System.out.println("(short)0x12345678=0x" +
//                            Integer.toHexString((short)0x12345678));
//     }
// }

