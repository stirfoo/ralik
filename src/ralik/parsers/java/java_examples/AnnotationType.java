/**
* Copyright © 2008, 2010 Oracle and/or its affiliates.
* All rights reserved. Use is subject to license terms.
* 
* For debugging the parser in src/ralik/parsers/java.clj
*/

/**
* Describes the "request-for-enhancement" (RFE)
* that led to the presence of the annotated API element
*/
@interface RequestForEnhancement {
   int id();			// Unique ID number associated with RFE
   String synopsis();		// Synopsis of RFE
   String engineer();		// Name of engineer who implemented RFE
   String date();		// Date RFE was implemented
}

/**
* An annotation with this type indicates that the
* specification of the annotated API wlement is
* preliminary and subject to change.
*/
@interface Perliminary {}

@interface Verboten {
    String[][] value();
}

@interface SelfRef { SelfRef value(); }

@interface Ping { Pong value(); }
@interface Pong { Ping value(); }

/**
* Associates a copyright notice with teh annotated API element.
*/
@interface Copyright {
    String value();
}

/**
* Associates a list of endorsers with the annotated class.
*/
@interface Endorsers {
    String[] value();
}

@interface Formatter {}

// Designates a formatter to pretty-print the annotated class.
@interface PrettyPrinter {
    Class<? extends Formatter> value();
}

@interface Quality {
    enum Level { BAD, INDIFFERENT, GOOD }
    Level value();
}

/**
* A person's name. This annotation type is not designed
* to be used directly to annotate program elements, but to
* define elements of other annotation types.
*/
@interface Name {
    String first();
    String last();
}

/**
* Indicates the author of the annotated program element.
*/
@interface Author {
    Name value();
}

/**
* Indicates the reviewer of the annotated program element.
*/
@interface Reviewer {
    Name value();
}

@interface RequestForEnhancementDefault {
    int id();			// No default
				//
    String synopsis();		// No default
				//
    String engineer() default "[unassigned]";
    String date() default "[unimplemented]";
}