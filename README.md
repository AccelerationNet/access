# Access

A Common Lisp library to unify access to the most common data
structures and to allow you to operate on them as they are (ie as a
bunch of dictionaries with slightly different apis)

## access, accesses , (setf access), (setf accesses)

These functions allow unified access to these data structures:

 * accessor access to CLOS objects
 * slot access to CLOS objects if the key matches a slot name but not
   an accessor
 * plists
 * alists
 * hash-tables

They also opts to produce nil as opposed to signaling errors when they
fail to access (eg (access nil 'anything) produces nil rather than
signaling a missing method on nil (though if 'anything is specialized
on nil it will call that method))

This library will probably appeal most to new comers to the language
as everyone else will probably be happy just calling each type of
access according to its own api.

### Limitations

 * Accessors should share slot names for this to work best.  This is
   due to differences in "direct" class slots versus indirect slots
   (only direct slots have the reader value filed out).
 * While most structures use equalper to get around differnt key
   packages and strings vs symbols.  Hash-tables do not currently
   support an equalper style interface.  As such some small care needs
   to be taken.  We try to support this by looking up values by
   symbol, then by symbol-name if symbol fails to produce a result.

### A word on performance

This libary is meant to make writing the program easier.  It does many
runtime lookups and checks to make sure that funcations called can
support the types they are called with.  As such it should not be used
in code where performance is important. It should however allow you to
prototype more rapidly and change the backing data stores without
having to change their access (ie I can switch from a plist to an
alist and everything will continue to work)

## Utilities

### has-slot?, has-reader?, has-writer?

Given a function or symbol, see if the object has a slot named that or
a reader/writer function associated with that name

### class-slot-names, class-slot-readers, class-slot-writers 

Returns the names associated with the classes slots.  Readers and
writers returns the functions used to access and set these slots,
however these currently only support readers/writers with the same
name as the slot.

### call-if-applicable, call-applicable-fns
Given an object and a function / funcation-name, this will call the
function passing in the object if it seems like that will work



### class-of-object 
A helper to find you the class of a given thing

```
  (typecase o
      (symbol (find-class o))
      (standard-class o)
      (standard-object (class-of o)))
```

### equalper

A predicate to make comparing symbols in different packages easier, by
comparing them case-insensitively based on symbol-name.  In other
respects it is equalp.

### plist-val, rem-plist-val, set-plist-val

Functions to ease access to plist values (used by access when
detecting a plist)



## DOT syntax

DOT syntax is invoked with #D reader macro on a form or by wrapping
that form in a with-dot call

Many new comers to the language long for their dot operator from other
lanugages they know.  This functionality is provided (when desired) by
enable-dot-syntax and in a block by using the with-dot macro.  I wrote
these for fun and much prefer just using the access functions directly
(ie. I never actually use these syntax transformers).  That said, when
the dot syntax is enabled symbols with a dot in them will be
transformed to the appropriate accesses calls.


  EX: #Dfoo.bar.bast => (accesses foo 'bar 'bast)
  EX: (with-dot () (setf ht.key.subkey new-val)) => (setf (accesses ht 'key 'subkey) new-val)

## Authors

 * [Acceleration.net](http://www.acceleration.net/) - [Donate](http://www.acceleration.net/programming/donate-to-acceleration-net/)
  * [Russ Tyndall](http://russ.unwashedmeme.com/blog)
  * [Nathan Bird](http://the.unwashedmeme.com/blog)
  * [Ryan Davis](http://ryepup.unwashedmeme.com/blog)

```
;; Copyright (c) 2011 Russ Tyndall , Acceleration.net http://www.acceleration.net
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```





