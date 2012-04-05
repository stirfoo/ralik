/**
* Copyright © 2008, 2010 Oracle and/or its affiliates.
* All rights reserved. Use is subject to license terms.
*
* May or may not contain code that requires the above copyright.
* 
* For debugging the parser in src/ralik/parsers/java.clj
*/

class Scratch {
    public T[] clone() {
        try {
           return (T[])super.clone(); // unchecked warning
        } catch(CloneNotSupportedException e) {
            throw new InternalError(e.getMessage());
        }
    }
}