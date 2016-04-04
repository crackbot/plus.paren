<a id='x-28SERVE-2EPAREN-3A-40MAIN-MANUAL-20MGL-PAX-3ASECTION-29'></a>

# Plus manual

## Table of Contents

- [1 plus.paren ASDF System Details][f52d]
- [2 Random utilities][d40d]
- [3 Class definitions for Javascript][f3b6]
- [4 Bindings macros for Parenscript][a0d8]
- [5 Runtime manual][8c5d]
    - [5.1 Standard CL functions][d47a]
    - [5.2 plus.paren additional functions][75ee]

###### \[in package PLUS.PAREN\]
<a id='x-28-22plus-2Eparen-22-20ASDF-2FSYSTEM-3ASYSTEM-29'></a>

## 1 plus.paren ASDF System Details

- Version: 0.0.1
- Description: Utility library for parenscript, useful js functions and ps macros.
- Licence: The MIT License (MIT)
- Author: Crackbot <thecrackbot@gmail.com>
- Maintainer: Crackbot <thecrackbot@gmail.com>

plus.paren provides more common lisp functions ported to parenscript as well as some other non standard cl but useful in js functions and utilities

<a id='x-28PLUS-2EPAREN-3A-40GENERAL-UTILITIES-20MGL-PAX-3ASECTION-29'></a>

## 2 Random utilities

<a id='x-28PLUS-2EPAREN-3A--3E-20-28PLUS-2EPAREN-3A-3APSMACRO-29-29'></a>

- [psmacro] **-\>** *&REST BODY* 

    A shortcut for function call on an object, same as chain but
    shorter

<a id='x-28PLUS-2EPAREN-3A-40-25-20-28PLUS-2EPAREN-3A-3APSMACRO-29-29'></a>

- [psmacro] **@%** *&REST BODY* 

    Shortcut for this accessor

<a id='x-28PLUS-2EPAREN-3ADEFUN-2FPARTIAL-20-28PLUS-2EPAREN-3A-3APSMACRO-29-29'></a>

- [psmacro] **DEFUN/PARTIAL** *NAME LAMBDA-LIST &BODY BODY* 

    Define a function with partial application support.
    
    For example
    
    ```lisp
    (defun/partial sum (x y z)
      (+ x y z))
    ```
    
    Can be executed as
    
    ```lisp
    (((sum 1) 2) 3)
    ((sum 1 2) 3)
    (sum 1 2 3)
    ```
    
       With equivalent result.

<a id='x-28PLUS-2EPAREN-3ACREATE-2A-20-28PLUS-2EPAREN-3A-3APSMACRO-29-29'></a>

- [psmacro] **CREATE\*** *&REST BODY* 

    Different way of creating an object, where each key can be a variable.
    
    ```lisp
    (create* some-variable value)
    ```
    
    expands into
    
    ```lisp
       (let ((obj (create)))
         obj[some-variable] = value;
         obj)
    ```


<a id='x-28PLUS-2EPAREN-3ADOLIST-IDX-20-28PLUS-2EPAREN-3A-3APSMACRO-29-29'></a>

- [psmacro] **DOLIST-IDX** *(VAR LIST-IDX ARRAY &OPTIONAL (RESULT NIL RESULT?)) &BODY BODY* 

    Dolist with one additional variable that is binded to array index.

<a id='x-28PLUS-2EPAREN-3ASWITCH-WITH-BREAK-20-28PLUS-2EPAREN-3A-3APSMACRO-29-29'></a>

- [psmacro] **SWITCH-WITH-BREAK** *VAL &BODY STATES* 

    Switch with explicit break, so you don't have to add (break) to
    every form.

<a id='x-28PLUS-2EPAREN-3A-40CLASS-MANUAL-20MGL-PAX-3ASECTION-29'></a>

## 3 Class definitions for Javascript

<a id='x-28PLUS-2EPAREN-3ADEFJSCLASS-20-28PLUS-2EPAREN-3A-3APSMACRO-29-29'></a>

- [psmacro] **DEFJSCLASS** *NAME SUPER-CLASSES &REST BODY* 

    Create parenscript classes.
       This form provides lighweight class definition.
       Syntax for class definition is the following:
    
    ```lisp
      (defjsclass $name ($superclass ...)
        "Documentation string"
        (:extensions)
        $initialization forms
        (defun $method-name ($method-param ...)
          $method-code))
    ```


<a id='x-28PLUS-2EPAREN-3ADEFMETA-20-28PLUS-2EPAREN-3A-3APSMACRO-29-29'></a>

- [psmacro] **DEFMETA** *NAME &REST BODY* 

    Define meta for a class.
    
    Meta object can be used to store properties that are static for
    every class instance.

<a id='x-28PLUS-2EPAREN-3ASETF-25-20-28PLUS-2EPAREN-3A-3APSMACRO-29-29'></a>

- [psmacro] **SETF%** *&REST ARGS* 

    Call setf with every key arg bound to this.
    
    ```lisp
      (setf% hello "world")
    ```
    
    Is equivalent to
    
    ```lisp
      (setf (@ this hello) "world")
    ```


<a id='x-28PLUS-2EPAREN-3A-2AWITH-SELF-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*WITH-SELF\*** *T*

    When set to t will expand all defun forms with "self" variable
    defined and bound to this, default is t

<a id='x-28PLUS-2EPAREN-3A-40BINDINGS-MANUAL-20MGL-PAX-3ASECTION-29'></a>

## 4 Bindings macros for Parenscript

<a id='x-28PLUS-2EPAREN-3AWHEN-LET-20-28PLUS-2EPAREN-3A-3APSMACRO-29-29'></a>

- [psmacro] **WHEN-LET** *BINDINGS &BODY FORMS* 

    Create new variable bindings, and conditionally executes `FORMS`.

<a id='x-28PLUS-2EPAREN-3A-40RUNTIME-MANUAL-20MGL-PAX-3ASECTION-29'></a>

## 5 Runtime manual

Different functions defined as a `SERVE.PAREN` runtime

<a id='x-28PLUS-2EPAREN-3A-40STANDARD-CL-20MGL-PAX-3ASECTION-29'></a>

### 5.1 Standard CL functions

<a id='x-28PLUS-2EPAREN-3APARTIAL-20-28PLUS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20PLUS-2EPAREN-3A-3A-2ASTANDARD-CL-2A-29-29'></a>

- [function] PARTIAL *FN &REST ARGS* 

<a id='x-28EQL-20-28PLUS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20PLUS-2EPAREN-3A-3A-2ASTANDARD-CL-2A-29-29'></a>

- [function] EQL *A B* 

    Check if a and b [`EQL`][d3a6]

<a id='x-28FIND-20-28PLUS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20PLUS-2EPAREN-3A-3A-2ASTANDARD-CL-2A-29-29'></a>

- [function] FIND *ITEM LST &KEY (TEST #'EQL)* 

    Find item in list using `TEST`

<a id='x-28FIND-IF-20-28PLUS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20PLUS-2EPAREN-3A-3A-2ASTANDARD-CL-2A-29-29'></a>

- [function] FIND-IF *PRED LST* 

    Return first value from list that pass `PRED` test

<a id='x-28REVERSE-20-28PLUS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20PLUS-2EPAREN-3A-3A-2ASTANDARD-CL-2A-29-29'></a>

- [function] REVERSE *LST* 

    Return reversed list

<a id='x-28ADJOIN-20-28PLUS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20PLUS-2EPAREN-3A-3A-2ASTANDARD-CL-2A-29-29'></a>

- [function] ADJOIN *ITEM LST &KEY (TEST #'EQL)* 

    Add item to lst unless it's already present

<a id='x-28APPEND-20-28PLUS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20PLUS-2EPAREN-3A-3A-2ASTANDARD-CL-2A-29-29'></a>

- [function] APPEND *&REST ARRAYS* 

    Concatenate arrays

<a id='x-28EVERY-20-28PLUS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20PLUS-2EPAREN-3A-3A-2ASTANDARD-CL-2A-29-29'></a>

- [function] EVERY *PRED SEQ* 

<a id='x-28COPY-LIST-20-28PLUS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20PLUS-2EPAREN-3A-3A-2ASTANDARD-CL-2A-29-29'></a>

- [function] COPY-LIST *LST* 

<a id='x-28LAST-20-28PLUS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20PLUS-2EPAREN-3A-3A-2ASTANDARD-CL-2A-29-29'></a>

- [function] LAST *LST* 

<a id='x-28REMOVE-IF-20-28PLUS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20PLUS-2EPAREN-3A-3A-2ASTANDARD-CL-2A-29-29'></a>

- [function] REMOVE-IF *PRED LST* 

<a id='x-28COUNT-20-28PLUS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20PLUS-2EPAREN-3A-3A-2ASTANDARD-CL-2A-29-29'></a>

- [function] COUNT *ITEM SEQ* 

<a id='x-28REST-20-28PLUS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20PLUS-2EPAREN-3A-3A-2ASTANDARD-CL-2A-29-29'></a>

- [function] REST *LST* 

<a id='x-28POSITION-20-28PLUS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20PLUS-2EPAREN-3A-3A-2ASTANDARD-CL-2A-29-29'></a>

- [function] POSITION *ITEM SEQ* 

<a id='x-28ASSOC-20-28PLUS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20PLUS-2EPAREN-3A-3A-2ASTANDARD-CL-2A-29-29'></a>

- [function] ASSOC *ITEM ALIST &KEY (TST #'EQL)* 

<a id='x-28SUBSEQ-20-28PLUS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20PLUS-2EPAREN-3A-3A-2ASTANDARD-CL-2A-29-29'></a>

- [function] SUBSEQ *SEQ START &OPTIONAL (END (LENGTH SEQ))* 

<a id='x-28PLUS-2EPAREN-3AMEMEBER-20-28PLUS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20PLUS-2EPAREN-3A-3A-2ASTANDARD-CL-2A-29-29'></a>

- [function] MEMEBER *ITEM LST &KEY TEST* 

    Check if `ITEM` is a member of `ARR`.

<a id='x-28MAPCAN-20-28PLUS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20PLUS-2EPAREN-3A-3A-2ASTANDARD-CL-2A-29-29'></a>

- [function] MAPCAN *FUN ARR* 

<a id='x-28MAPCAR-20-28PLUS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20PLUS-2EPAREN-3A-3A-2ASTANDARD-CL-2A-29-29'></a>

- [function] MAPCAR *FUN &REST ARRS* 

<a id='x-28MAP-INTO-20-28PLUS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20PLUS-2EPAREN-3A-3A-2ASTANDARD-CL-2A-29-29'></a>

- [function] MAP-INTO *FN ARR* 

    Call FN on each element in `ARR`, replace element with the return value.

<a id='x-28MAP-20-28PLUS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20PLUS-2EPAREN-3A-3A-2ASTANDARD-CL-2A-29-29'></a>

- [function] MAP *FN ARR* 

    Call FN on each element in `ARR` and return the returned values in a new array.

<a id='x-28SET-DIFFERENCE-20-28PLUS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20PLUS-2EPAREN-3A-3A-2ASTANDARD-CL-2A-29-29'></a>

- [function] SET-DIFFERENCE *ARR ARR-TO-SUB* 

    Return a new array with only those elements in `ARR` that are not in `ARR-TO-SUB`.

<a id='x-28REDUCE-20-28PLUS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20PLUS-2EPAREN-3A-3A-2ASTANDARD-CL-2A-29-29'></a>

- [function] REDUCE *FUNC LIST &OPTIONAL INIT* 

<a id='x-28NCONC-20-28PLUS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20PLUS-2EPAREN-3A-3A-2ASTANDARD-CL-2A-29-29'></a>

- [function] NCONC *ARR &REST ARRS* 

<a id='x-28MAPLIST-20-28PLUS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20PLUS-2EPAREN-3A-3A-2ASTANDARD-CL-2A-29-29'></a>

- [function] MAPLIST *FN ARR* 

<a id='x-28PLUS-2EPAREN-3A-40PLUS-LIBRARY-20MGL-PAX-3ASECTION-29'></a>

### 5.2 plus.paren additional functions

<a id='x-28PLUS-2EPAREN-3AFILTER-20-28PLUS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20PLUS-2EPAREN-3A-3A-2APLUS-LIBRARY-2A-29-29'></a>

- [function] FILTER *PRED LST* 

    Same as remove-if-not

<a id='x-28PLUS-2EPAREN-3ANREPLACE-20-28PLUS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20PLUS-2EPAREN-3A-3A-2APLUS-LIBRARY-2A-29-29'></a>

- [function] NREPLACE *LST OBJ1 OBJ2 &KEY TEST* 

    replace obj1 with obj2 inside lst, eq-fun is used to find obj1
    inside lst

<a id='x-28PLUS-2EPAREN-3AFIND-IDX-20-28PLUS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20PLUS-2EPAREN-3A-3A-2APLUS-LIBRARY-2A-29-29'></a>

- [function] FIND-IDX *LST OBJ &OPTIONAL (EQ-FUN (LAMBDA (X Y) (EQ X Y)))* 

    Same as find but returns index instead of element

<a id='x-28PLUS-2EPAREN-3AVALS-20-28PLUS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20PLUS-2EPAREN-3A-3A-2APLUS-LIBRARY-2A-29-29'></a>

- [function] VALS *OBJ* 

    Object values

<a id='x-28PLUS-2EPAREN-3AKEYS-20-28PLUS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20PLUS-2EPAREN-3A-3A-2APLUS-LIBRARY-2A-29-29'></a>

- [function] KEYS *OBJ* 

    Object keys

<a id='x-28PLUS-2EPAREN-3ALEN-20-28PLUS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20PLUS-2EPAREN-3A-3A-2APLUS-LIBRARY-2A-29-29'></a>

- [function] LEN *VAL* 

<a id='x-28PLUS-2EPAREN-3AONEP-20-28PLUS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20PLUS-2EPAREN-3A-3A-2APLUS-LIBRARY-2A-29-29'></a>

- [function] ONEP *VAL* 

<a id='x-28PLUS-2EPAREN-3AONEM-20-28PLUS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20PLUS-2EPAREN-3A-3A-2APLUS-LIBRARY-2A-29-29'></a>

- [function] ONEM *VAL* 

<a id='x-28PLUS-2EPAREN-3ADEFAULTS-20-28PLUS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20PLUS-2EPAREN-3A-3A-2APLUS-LIBRARY-2A-29-29'></a>

- [function] DEFAULTS *OBJ DEF* 

    Iterate through `DEF` keys and values setting any key found in
    `DEF` but not found in `OBJ` to corresponding value

  [75ee]: #x-28PLUS-2EPAREN-3A-40PLUS-LIBRARY-20MGL-PAX-3ASECTION-29 "plus.paren additional functions"
  [8c5d]: #x-28PLUS-2EPAREN-3A-40RUNTIME-MANUAL-20MGL-PAX-3ASECTION-29 "Runtime manual"
  [a0d8]: #x-28PLUS-2EPAREN-3A-40BINDINGS-MANUAL-20MGL-PAX-3ASECTION-29 "Bindings macros for Parenscript"
  [d3a6]: #x-28EQL-20-28PLUS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20PLUS-2EPAREN-3A-3A-2ASTANDARD-CL-2A-29-29 "(EQL (PLUS.PAREN::STATIC-PS-FUNCTION PLUS.PAREN::*STANDARD-CL*))"
  [d40d]: #x-28PLUS-2EPAREN-3A-40GENERAL-UTILITIES-20MGL-PAX-3ASECTION-29 "Random utilities"
  [d47a]: #x-28PLUS-2EPAREN-3A-40STANDARD-CL-20MGL-PAX-3ASECTION-29 "Standard CL functions"
  [f3b6]: #x-28PLUS-2EPAREN-3A-40CLASS-MANUAL-20MGL-PAX-3ASECTION-29 "Class definitions for Javascript"
  [f52d]: #x-28-22plus-2Eparen-22-20ASDF-2FSYSTEM-3ASYSTEM-29 "(\"plus.paren\" ASDF/SYSTEM:SYSTEM)"
