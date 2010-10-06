# clj-cera

Clojure port of the Complex Event Recognition Architecture (CERA).

See the following for more information on CERA:

* The main paper describing CERA (PDF): [Multimodal Event Parsing for Intelligent User Interfaces](http://www.entish.org/published/P2338-fitzgerald.pdf)
* Another paper discussing CERA (PDF): [Complex Event Pattern Recognition Software for Multimodal User Interfaces](http://www.cs.cmu.edu/~aphillips/publications/cera-mics-04.pdf)
* Open-source versions of CERA in Python and C++ at SourceForge.net: [CERA](http://sourceforge.net/projects/cera/)

In the discussion below, I'm assuming the reader is familiar with the first paper above and so understands the gist of CERA and the meaning of signals, patterns and recognizers in this context.

Clj-cera is an exercise in porting the original object-oriented (OO) version of CERA into idiomatic Clojure. It uses a records (via `defrecord`) for `Status` and `Signal` types, a `Recognizer` protocol, and each recognizer implements this protocol via different `defrecord` types. The Clojure implementation relies on the polymorphism in the `Recognizer` protocol. This polymorphism allows the recognizers to be nested together.

The `Recognizer` protocol defines three methods: `transition`, `recognized` and `contravened?`. The `Recognizer`s of the original CERA code had `signal`, which I renamed to transition to avoid confusion between the signal type and the signal action (noun and verb confusion). My `contravened?` method is the same as the original `is_contravened` and `isContravened` (the Python and C++ names). I've added the `recognized` method, which returns a sequence of actual `Signal`s a recognizer has recognized (matched) once it has completed.

Perhaps the largest (and mind-bending) change is that clj-cera's recognizers are immutable. This means the `transition` method returns a new recognizer in its next state after consuming the probe signal. This is in contrast to the original OO implementation's `signal` method, which changes the recognizers internal mutable state, and returns the recognizer's current status.

The Python and C++ code use an immutable `Pattern` class, which is not needed in this Clojure version since the recognizers are immutable. So instead of creating a `Pattern` object and calling `make-recognizer`, one can simply keep reuse the initial recognizer over and over.

The `BaseRecognizer` (the bottom-most recognizer that actually matches signals and does not allow deeper nesting) is more generalized than the original OO version, allowing arbitrary predicates for the signal and contravention matching. The CERA paper discusses arbitrary predicates, however the OO code only allows for signals with matching data.

## Usage

See `test/clj_cera/test/test_core.clj` for examples of usage.

<!--
## Installation
-->

## License

Copyright (c) 2010 Rick Mouritzen. All rights reserved.

The use and distribution terms for this software are covered by the
Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
which can be found in the file epl-v10.html at the root of this distribution.
By using this software in any fashion, you are agreeing to be bound by
the terms of this license.

You must not remove this notice, or any other, from this software.

***
This is the same license used by Clojure.
