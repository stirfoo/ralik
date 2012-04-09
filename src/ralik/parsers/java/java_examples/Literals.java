/**
* Copyright © 2008, 2010 Oracle and/or its affiliates.
* All rights reserved. Use is subject to license terms.
* 
* For debugging the parser in src/ralik/parsers/java.clj
*/

public class Literals {
    private DoesNotMatter
        foo[] = {0, 12, -23, +333, 3423423, 3459348573948379,
                 0l, 3435l, 0L, 324523L, 0x0, 0x0l, 0x0f,
                 0xDadaCafe, 0xfa98e98c987bb997a9d987f,
                 0x00FF00FF, 0xC0B0L, 00, 0000000000007,
                 07777777777777777777777777777777777777,
                 01l, 0777, 03457L};
    private DoesNotMatter
        foo[] = {3.4028235e38f, 1.40e-45f, 1.7976931348623157e308,
                 1e1f, 2.f, .3f, 0f, 0d, 0., .0, 0.0, 3.14f,
                 6.022137e23f, 1e1, 2., .3, 3.14e-9d, 1e137,
                 0x.afAFp-33D, 0x0p0, 0xFP9, 0xa.p23, 0x.aP5,
                 0xa.fP2, 0x1.a6666666666p1};
    // parser does fail as it should on these:
    // '\u000a', '\u000d'
    private DoesNotMatter
        foo[] = {' ', 'a', '%', '\b', '\t', '\n', '\f', '\r', '\"',
                 '\'', '\\', '\u03a9', '\uFFFF', '\177', 'λ', '\n', '\r'};
    // parser does fail as it should on these:
    // "\u000a", "\u000d"
    private DoesNotMatter
        foo[] = {"", "\"\"", "This is a string", "\u03bbλ", "\n", "\r"};
    private DoesNotMatter
        foo[] = {null, true, false};
}
