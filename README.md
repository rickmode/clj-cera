# clj-cera

Clojure port of the Complex Event Recognition Architecture (CERA).

See the following for more information on CERA:

* The main paper describing CERA (PDF): [Multimodal Event Parsing for Intelligent User Interfaces](http://www.entish.org/published/P2338-fitzgerald.pdf)
* Another paper discussing CERA (PDF): [Complex Event Pattern Recognition Software for Multimodal User Interfaces](http://www.cs.cmu.edu/~aphillips/publications/cera-mics-04.pdf)
* Open-source versions of CERA in Python and C++ at SourceForge.net: [CERA](http://sourceforge.net/projects/cera/)

In the discussion below, I'm assuming the reader is familiar with the first paper above and so understands the gist of CERA and the meaning of signals, patterns and recognizers in this context.

Clj-cera is an exercise in porting the original object-oriented (OO) version of CERA into idiomatic Clojure. It uses a records (via `defrecord`) for `Status` and `Signal` types, a `Recognizer` protocol, and each recognizer implements this protocol via different `defrecord` types. The Clojure implementation relies on the polymorphism in the `Recognizer` protocol, allowing composing (nesting) the recognizers.

The `Recognizer` protocol defines three methods: `transition`, `recognized` and `contravened?`. The `transition` method is named `signal` in the original CERA code. The new name avoids the noun/verb confusion between the signal type and the signal action, and instead notes that recognizers are finite-state machines (FSM) which consume signals to transition between states. The `contravened?` method is the same as the original Python `is_contravened` / C++ `isContravened` method. The `recognized` method is new; it returns a sequence of the `Signal`s a recognizer has recognized (matched) once completed.

Perhaps the largest (and mind-bending) change into idiomatic Clojure is immutability of recognizers. The `transition` method returns a new recognizer in its next state after consuming the probe signal. The original OO implementation's `signal` method mutates the recognizer object's internal state and returns the recognizer's current status.

The Python and C++ code use an immutable `Pattern` class. The immutable `Pattern` has a factory `make_recognizer` which creates the stateful recognizer FSM. In this Clojure version, a recognizer is a persistent data structure. So rather than separate the pattern and the recognizer FSM, reuse of recognizers means merely starting from its first persistent version (its initial condition before consuming any signals).

The `BaseRecognizer` (the bottom-most recognizer that actually matches signals and does not allow deeper nesting) is more generalized than the original OO version, allowing arbitrary predicates for the signal and contravention matching. The CERA paper discusses arbitrary predicates, however the OO code only allows for signals with matching data.

A small change is that the clj-cera recognizers do not hold a callback. In the original OO code, the recognizers held a callback passed to them via the `Pattern` `make_recognizer` method, but do not themselves use this callback, so it's just a place for the higher level `Agenda` to hold them until they are necessary. Further, only top-level recognizers even use this callback, so the slot is wasted for all nested recognizers. If clj-cera is carried further, this callback mechanism would fit in a layer around recognizers, and not the recognizers themselves. 

The current implementation does not implement the Allen recognizers (though I do not see any hurdles doing so). It also doesn't have an `Agenda` implementation, which is the manager that pumps signals to the various active recognizers.

## Usage

See `test/clj_cera/test/test_core.clj` for examples of usage.

<!--
## Installation
-->

## License

Copyright (c) 2010 Rick Mouritzen. All rights reserved.

The use and distribution terms for this software are covered by the
[Eclipse Public License 1.0](http://opensource.org/licenses/eclipse-1.0.php)
which can be found in the file epl-v10.html at the root of this distribution.
By using this software in any fashion, you are agreeing to be bound by
the terms of this license.

You must not remove this notice, or any other, from this software.

This is the same license used by Clojure.

The license for the original CERA code is in the LICENSE file within the [source distribution](http://sourceforge.net/projects/cera/files/cera/1.0/cera-1.0.tar.gz/download) [here](http://sourceforge.net/projects/cera/files/).
