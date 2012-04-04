;;; java-parser.clj
;;;
;;; Tuesday, April  3 2012

(ns ralik.test.java-parser
  (:use [ralik.parsers.java])
  (:use [clojure.test]))

(testing "3.6 White Space"
  (is (java ""))
  (is (java "   \t\r\n"))
  (is (java "
")))

(testing "3.7 Comments"
  (is (java "// comment"))
  (is (java "// /* and */ have no meaning here"))
  (is (java "
// comment
"))
  (is (java "/* comment */"))
  (is (java "/* this comment /* // /** ends here: */"))
  (is (java "/**
comment
**/"))
  (is (java "/**

// comment
comment
********* /// *******     /
**/"))
  (is (java "/**
// comment
comment
********* /*//*/ *******     /
**/
// comment
/* comment *//*comment*/")))

(testing "3.8 Identifiers"
  (is (java "String"))
  (is (java "i3"))
  (is (java "αρετη"))
  (is (java "MAX_VALUE"))
  (is (java "isLetterOrDigit")))

(testing "3.9 Keywords"
  (is (not (java "abstract")))
  (is (not (java "assert")))
  (is (not (java "boolean")))
  (is (not (java "break")))
  (is (not (java "byte")))
  (is (not (java "case")))
  (is (not (java "catch")))
  (is (not (java "char")))
  (is (not (java "class")))
  (is (not (java "const")))
  (is (not (java "continue")))
  (is (not (java "default")))
  (is (not (java "do")))
  (is (not (java "double")))
  (is (not (java "else")))
  (is (not (java "enum")))
  (is (not (java "extends")))
  (is (not (java "final")))
  (is (not (java "finally")))
  (is (not (java "float")))
  (is (not (java "for")))
  (is (not (java "if")))
  (is (not (java "goto")))
  (is (not (java "implements")))
  (is (not (java "import")))
  (is (not (java "instanceof")))
  (is (not (java "int")))
  (is (not (java "interface")))
  (is (not (java "long")))
  (is (not (java "native")))
  (is (not (java "new")))
  (is (not (java "package")))
  (is (not (java "private")))
  (is (not (java "protected")))
  (is (not (java "public")))
  (is (not (java "return")))
  (is (not (java "short")))
  (is (not (java "static")))
  (is (not (java "strictfp")))
  (is (not (java "super")))
  (is (not (java "switch")))
  (is (not (java "synchronized")))
  (is (not (java "this")))
  (is (not (java "throw")))
  (is (not (java "throws")))
  (is (not (java "transient")))
  (is (not (java "try")))
  (is (not (java "void")))
  (is (not (java "volatile")))
  (is (not (java "while"))))

(testing "3.10 Literals"
  (testing "3.10.1 IntegerLiteral"
    (testing "DecimalIntegerLiteral"
      (is (java "0"))
      (is (java "3435452"))
      (is (java "3435452234234234"))
      (is (java "343545223999923045892375892334023470239704234234"))
      (is (java "0l"))
      (is (java "3435452l"))
      (is (java "0L"))
      (is (java "3435452L")))
    (testing "HexIntegerLiteral"
      (is (java "0x0"))
      (is (java "0X0l"))
      (is (java "0X0f"))
      (is (java "0X0fL"))
      (is (java "0xDadaCafe"))
      (is (java "0xad7f7e853f8ca92efa763834344fe8e8aafe655263fcbb367fbb3"))
      (is (java "0x00FF00FF"))
      (is (java "0xC0B0L")))
    (testing "OctalIntegerLiteral"
      (is (java "00"))
      (is (java "0000000000007"))
      (is (java "07777777777777777777771") "promote to BigInteger")
      (is (java "01l"))
      (is (java "0777"))
      (is (java "03457L"))))
  (testing "3.10.2 Floating-Point Literals"
    (testing "DecimalFloatingPointLiteral "
      (is (java "3.4028235e38f"))
      (is (java "1.40e-45f"))
      (is (java "1.7976931348623157e308"))
      (is (java "1e1f"))
      (is (java "2.f"))
      (is (java ".3f"))
      (is (java "0f"))
      (is (java "0d"))
      (is (java "0."))
      (is (java ".0"))
      (is (java "0.0"))
      (is (java "3.14f"))
      (is (java "6.022137e23f"))
      (is (java "1e1"))
      (is (java "2."))
      (is (java ".3"))
      (is (java "3.14e-9d"))
      (is (java "1e137")))
    (testing "HexadecimalFloatingPointLiteral"
      (is (java "0x.afAFp-33D"))
      (is (java "0x0p0"))
      (is (java "0xFP9"))
      (is (java "0xa.p23"))
      (is (java "0x.aP5"))
      (is (java "0xa.fP2"))
      (is (java "0x1.a6666666666p1")))
    (testing "3.10.3 Boolean Literals"
      (is (java "true"))
      (is (java "false")))
    (testing "3.10.4 Character Literals"
      (is (java "' '"))
      (is (java "'a'"))
      (is (java "'%'"))
      (is (java "'\\b'"))
      (is (java "'\\t'"))
      (is (java "'\\n'"))
      (is (java "'\\f'"))
      (is (java "'\\r'"))
      (is (java "'\\\"'"))
      (is (java "'\\''"))
      (is (java "'\\\\'"))
      (is (java "'\\u03a9'"))
      (is (java "'\\uFFFF'"))
      (is (java "'\\177'"))
      (is (java "'λ'"))
      (is (not (java "'\n'")))
      (is (not (java "'\r'")))
      (is (not (java "'\\u000a'")))
      (is (not (java "'\\u000d'"))))
    (testing "3.10.5 String Literals"
      (is (java "\"\""))
      (is (java "\"\\\"\""))
      (is (java "\"This is a string\""))
      (is (java "\"\\u03bbλ\""))
      (is (not (java "\"\n\"")))
      (is (not (java "\"\r\"")))
      (is (not (java "\"\\u000a\"")))
      (is (not (java "\"\\u000d\""))))
    (testing "3.10.7 The Null Literal"
      (is (java "null")))))