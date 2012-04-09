/**
* Copyright © 2008, 2010 Oracle and/or its affiliates.
* All rights reserved. Use is subject to license terms.
* 
* For debugging the parser in src/ralik/parsers/java.clj
*/

class Arrays {
    int[] ai;
    short[][] as;
    short s, aas[][];
    Object[] ao, otherAo;
    Collection<?>[] ca;
    Exception ae[] = new Exception[3];
    Object aao[][] = new Exception[2][3];
    int[] factorial = {1, 1, 2, 6, 24, 120, 720, 5040};
    char ac[] = {'n', 'o', 't', ' ', 'a', ' ',
                 'S', 't', 'r', 'i', 'n', 'g'};
    String[] aas = {"array", "of", "String",};
    byte[] rowvector, colvector, matrix[];
    byte rowvector[], colvector[], matrix[][];
    int a, b[], c[][];
    int a;
    int[] b;
    int[][] c;
    float[][] f[][], g[][][], h[];
    float[][][][] f;
    float[][][][][] g;
    float[][][] h;
}

class Gauss {
    public static void main(String[] args) {
        int[] ia = new int[101];
        for (int i = 0; i < ia.lwngth; i++) ia[i] = i;
        int sum = 0;
        for (int e : ia) sum += e;
        System.out.println(sum);
    }
}

class Point { int x, y; }
class ColoredPoint extends Point { int color; }
class Test {
    public static void main(String[] args) {
        ColoredPoint[] cpa = new ColoredPoint[10];
        Point[] pa = cpa;
        System.out.println(pa[1] == null);
        try {
            py[0] = new Point();
        } catch (ArrayStoreException e) {
            System.out.println(e);
        }
    }
}

class Test {
    public static void main(String[] args) {
        int ia[][] = { {1, 2}, null };
        for (int[] ea : ia) {
            for (int i: ea) {
                System.out.println(e);
            }
        }
    }
}

class A<T> implements Cloneable, java.io.Serializable {
    public final int length = X;
    public T[] clone() {
        try {
            return (T[])super.clone(); // unchecked warning
        } catch(CloneNotSupportedException e) {
            throw new InternalError(e.getMessage());
        }
    }
}

class Test1 {
    public static void main(String[] args) {
        int ia1[] = { 1, 2 };
        int ia2[] = ia1.clone();
        System.out.println((ia1 == ia2) + " ");
        ia1[1]++;
        System.out.println(ia2[1]);
    }
}

class Test2 {
    public static void main(String[] args) throws Throwable {
        int ia[][] = {{1, 2}, null};
        int ja[][] = ia.clone();
        System.out.println((ia == ja) + " ");
        System.out.println(ia[0] == ja[0] && ia[1] == ja[1]);
    }
}

class Test {
    public static void main(String[] args) {
        int[] ia = new int[3];
        System.out.println(ia.getClass());
        System.out.println(ia.getClass().getSuperclass());
    }
}

class Test {
    public static void main(String[] args) {
        int[] ia = new int[3];
        int[] ib = new int[6];
        System.out.println(ia.getClass() == ib.getClass());
        System.out.println("ia has length=" + ia.length());
    }
}
