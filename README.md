# Answer Sources in Prolog

Nan.System.Sources  
Nan.System.Sources/Prolog 1.2.0-beta  
Answer Sources in Prolog  
Copyright 2015-2017 Julio P. Di Egidio  
Licensed under GNU GPLv3.  
http://julio.diegidio.name/Projects/Nan.System.Sources/  
https://github.com/jp-diegidio/Nan.System.Sources-Prolog/  

**This library implements Answer Sources in Prolog.**  
*This is an open and free project.*  
Source code available and pull requests accepted.  

  1. What are answer sources
  2. How to use this library
  3. Still to be done

## 1. What are answer sources

Answer sources can be seen as generalized iterators, allowing a given
program to control answer production in another. Each _source_ works as a
separate program interpreter, and, in our implementation, in its own
process, via multi-threading.

For the rationale and design, we have followed almost exactly Paul Tarau on
_fluents_ [1], with at least the following differences:

- Our _sources_ use multi-threading unlike Tarau's;
- Our _sources_ execute arbitrary Prolog goals, i.e. are not restricted
  to Horn clauses;
- Thanks to multi-threading, our `next` operation (`get` in Tarau's)
  supports an asynchronous pattern: e.g. this allows for easy
  implementation of parallelism in combinators;
- Our _sources_ implement a `reset` operation that restarts the
  enumeration, so Tarau's `split` combinator is not necessary;
- The overhead of threading being unnecessary for combinators, support
  for combined _sources_ is built into our source object module;
- Our _answer_ term extends Tarau's to report exact determinism.

**TODO**: A still needed final step is to extend our _sources_ from _fluents_
to _interactors_ [2], which requires implementation of a `yield` operation
(`return` in Tarau's) to be available to the target predicate, i.e. in the
context of the source worker loop.

A "Preview" version of Answer Sources was initially presented here [3].
A flow diagram for the `yield` operation (denoted `return` in the diagram)
was later presented here [4].

[1] P. Tarau, "Fluents: A Refactoring of Prolog for Uniform Reflection
and Interoperation with External Objects":  
<http://www.cse.unt.edu/~tarau/research/LeanProlog/RefactoringPrologWithFluents.pdf>  
[2] P. Tarau, A. Majumdar, "Interoperating Logic Engines":  
<http://www.cse.unt.edu/~tarau/research/LeanProlog/InteroperatingLogicEngines.pdf>  
[3] J.P. Di Egidio, "Answer Sources in Prolog (SWI) - Preview":  
<http://seprogrammo.blogspot.it/2015/09/answer-sources-in-prolog-swi-preview.html>  
[4] J.P. Di Egidio, "Answer Sources: from Fluents to Interactors":  
<http://seprogrammo.blogspot.co.uk/2015/09/answer-sources-from-fluents-to.html>  

## 2. How to use this library

This library comprises the following modules:

  - `sources` (nan/system/sources.pl)  
    Provides the predicates that implement Answer Sources in Prolog.
  - `sources_types` (nan/system/sources_types.pl)  
    Provides type testing predicates to validate arguments in user code.

For detailed documentation on the available modules and predicates, please
see the code docs accompanying the Prolog code files.

This library was developed and tested with [SWI-Prolog 7.3.25](<http://www.swi-prolog.org/>).

**Basic usage example**:

```prolog
?- pack_install(nan_system_sources).
true.

?- use_module(library(nan/system/sources)).
true.

?- using_source(I, between(1, 2, I), _S,
   (   repeat,
       source_next(_S, answer(_Det, the(I))),
       (_Det == last -> !; true)
   )).
I = 1 ;
I = 2.
```

## 3. Still to be done

  - Extend from fluents to interactors (implement `yield/1`).
  - Improve compatibility with other Prolog systems (implement a
    compatibility layer).
  - Improve documentation and test cases.
