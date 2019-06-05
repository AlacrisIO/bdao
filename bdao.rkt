#lang at-exp racket @; -*- Scheme -*-
#|
;; Building Distributed Applications in OCaml
;; LambdaConf 2019, Friday June 7th, 1300-1350

;; https://lambdaconf.zohobackstage.com/LambdaConf2019#/agenda?day=3&filterBy=speaker&filterValue=6967000000308154&lang=en&sessionId=6967000000359626

;; To compile the slides, use:
;;    racket bdao.rkt > index.html

;; This document is available under the bugroff license.
;;    http://tunes.org/legalese/bugroff.html

;; Official Abstract:
;; This talk will present a collection of techniques you can use to write distributed applications in OCaml, from common monads to functions derived from type structure. It will show how functional programming makes it easy to build robust programs thanks to the power of abstraction and composition. In this talk, we will explore some functional programming techniques used while implementing our blockchain software in OCaml:
;; * pure functional programming with monads (mostly state, error and future)
;; * content-addressed storage
;; * escape to impure behavior for logging, and, interestingly, reading from content-addressed storage.
;; * higher-order modules to abstract over persistent storage
;; * lenses and zippers to access data functionally
;; * how zippers trivialize merklization of data structures to make them verifiable
;; * automatically deriving (un)marshaling from the type structure of the data using ppx (metaprogramming for OCaml).
;; The talk will emphasize how to make types your friends rather than your enemies, what you pay in doing so, and what you gain for that price.
;; (Note that the techniques involved can be used even in absence of a static type system insufficient to fully check the correctness of your proper usage—for instance a type system without dependent types such as in OCaml.)
|#

(require scribble/html "reveal.rkt")

(define (ocaml . x) (apply code class: 'ocaml x))

(slide ()
       @table[(tr(td width: "10%")
                 (td width: "32%"
                     (img src: "resources/pic/alacris_logo.png"
                          style: "border: 0; vertical-align: top; background-color: transparent"))
                 (td width: "15%")
                 (td (font fgcolor: *red* size: 7
                          @b{𝔹@(~ 2)uilding @(br)
                             𝔻@(~ 2)istributed @(br)
                             𝔸@(~ 2)pplications in @(br)
                             𝕆@(~ 2)Caml})))]
  ~ @(L(@small ~))
  @p{François-René Rideau, @(~)@em{Alacris}}
  @C{@ocaml{fare@"@"alacris.io}}
  ~
  @p{LambdaConf 2019, 2019-06-07}
  @url{https://alacrisio.github.io/bdao/})

#|
Why use OCaml rather than Lisp?
* cryptocurrency: can't afford a single bug.
* types can find bugs @em{before} deployment.
* explicitly communicate design to coworkers.
* useful discipline with many benefits…

Downsides of OCaml versus Lisp?
* Types-first makes exploration harder.
* Types can't express the Lisp-easy solution.
* Some types you use boggle the mind.
* Dynamic usage patterns are slow and awkward.
* Static patterns are repetitive and error-prone.
* No macros. PPX a poor yet expensive substitute.

Bugs eliminated by types, not tests
* type mismatch (int for string, etc.).
* improper function calls.
* pattern exhaustiveness.
* errors in untested or intermediate results.
* incomplete refactoring
* abstraction violation (esp. w/ modules)

Bugs not eliminated by types
* Bugs within a module
  @comment{By restricting interactions, not just fewer bugs,
           but simpler bugs that fit a brainful.}
* Configuration bugs
  @comment{Or bugs in your extension language}
* Interval errors
  @comment{But fewer, and caught earlier}
* Concurrency Issues
  @comment{But fewer, and caught earlier}


Performance
* No runtime-check
* A lot of optimizations (even more if pure)
* Dynamic behavior actually made worse, when needed.
* Macros are harder, limiting user-provided optimizations.

------>8------>8------>8------>8------>8------>8------>8------>8------>8------


This talk
* A collection of general-purpose techniques, in OCaml
* Based on a library for distributed applications
* XXX
* From a Lisper at heart


Monads to reify side-effects
* Error (Lisp: raising conditions)
* State (Lisp: assignment)
* Future (Lisp: threads, futures)
* Reader (Lisp: dynamic binding)
* Not used (so far): Continuations, Non-determinism…
* Intended soon: reify all I/O (notably DB, network)

In the future, also reify network communication,
and maybe also thread scheduling, so that failure scenarios
can be recorded and replayed, to find bugs and write tests.

Example error monad vs exceptions... forces you to think about
who is responsible for what errors or for recovery from them,
and how to fold multiple layers of potential errors.
Example of what is obviously allowed or not allowed with monad on or off. (moffad?)

Example State monad vs state...
makes unintentional side-effects impossible.

Example Future monad vs threads
makes it more obvious what is atomic or not.

Discriminate between effects enabled and disabled. Makes it explicit where the effects happen.
Makes it easier to reason about the program: most parts have no effects, and those
parts that have effects can focus on that effect and remain simple.
Problem: switching between effect and no effect is very expensive, as it can percolate
up the call chain into the entire program, and sometimes even contaminate
data structures and everything that touches them. The problem being that
even the syntax for basic function call changes:
not only do you have to "upgrade" (or "downgrade", depending on POV) all effect types everywhere,
the sad lack of syntactic abstraction means means that when you use monads rather than pure functions,
you have to write everything in CPS or ANF rather than direct style, and vice-versa.
(if code transformation is A-OK, why not go all the way to direct style???
    do-notation is a half-assed idea, and
    those who see it as the summum of monadic notation are brain-damaged)

in OCaml, from common monads to functions derived from type structure. It will show how functional programming makes it easy to build robust programs thanks to the power of abstraction and composition. In this talk, we will explore some functional programming techniques used while implementing our blockchain software in OCaml:

Content-addressed storage
* wrap values in lazy-loading... is that an effect?
* escape to impure behavior for logging, and, interestingly, reading from content-addressed storage.
* higher-order modules to abstract over persistent storage
* lenses and zippers to access data functionally




* how zippers trivialize merklization of data structures to make them verifiable
* automatically deriving (un)marshaling from the type structure of the data using ppx (metaprogramming for OCaml).
The talk will emphasize how to make types your friends rather than your enemies, what you pay in doing so, and what you gain for that price.
(Note that the techniques involved can be used even in absence of a static type system insufficient to fully check the correctness of your proper usage—for instance a type system without dependent types such as in OCaml.)
|#

(slide-group "Marshaling"
(gslide () @h1{Marshaling set straight}
@L{Translating data structures into byte sequences}
@L{a.k.a. Serializing, Encoding, Formatting, Unparsing}
~
@L{Must be matched by @em{unmarshaling} (a.k.a. etc.)}
~
@L{Great sink of labor (old IBM study: 30% of code)}
@L{Great fount of bugs & vulnerabilities…})

(gslide () @h1{Level 1: Processing Byte Buffers}
@L{@ocaml{server: input_stream -> output_stream -> unit}}
~
@L{Read: @ocaml{let b = inbuf.(n) in if b == 1 then …}}
@L{Write: @ocaml{match val with C x -> outbuf.(n) = (f x) … }}
~
@L{OCaml types don't help at all (but bounds checks do)}

@L{Might as well use C - it'd be faster}
@comment{OK, in FORTH. Let's at least pick a functional language, not a dysfunctional language}
~
@L{"Shotgun parser": parser peppered into processor}

@L{Very fast... like grapeshot fired at your foot}
~
@fragment[@C{LangSec: DON'T DO IT}]
@comment{Language Theoretic Security -- see talk by Meredith Patterson in a previous LambdaConf})

(gslide () @h1{Level 2: Processing JSON (or XML, etc.)}
@L{@ocaml{server: json -> json}}
~
;; Read, Write...
~
@L{Dynamic type safety}
@L{Might as well use Lisp - macros would help a lot}
@L{Still "Shotgunning" structural integrity checks}
@L{Somewhat slow, only so safe, painful in OCaml}
~
@fragment[@L{Missing the whole point of OCaml}])

(gslide () @h1{Level 3: Marshaling messages by hand}
  @L{@ocaml{server: request -> response}}
  @L{@ocaml{type 'a marshaler = Buffer.t -> 'a -> unit}}
  @L{@ocaml{type 'a unmarshaler = int -> Bytes.t -> 'a*int}}
  ~
  @L{Lots of bug-prone drudgery writing (un)parsers})

(gslide () @h1{Level 4: Marshaling combinators}
  @L{@ocaml{type 'a marshaling = @(br)   { marshal: 'a marshaler; unmarshal: 'a unmarshaler }}}
  @L{@ocaml{val marshaling2 : ('x -> 'a*'b) -> ('a -> 'b -> 'x) -> @(br)  
            'a marshaling -> 'b marshaling -> 'x marshaling}}
  ~
  @L{Much less bug-prone and drudgery, but still too much})

(gslide () @h1{Level 5: Derive marshaling from type}
  @L{OCaml PPX: metaprogramming}
  @L{Compared to Lisp macros, quite heavy to use and not composable})

(gslide () @h1{Level 6: Use GADT}
  @L{@ocaml{server: 'a request -> 'a response}}
  ~
  @L{Problem: even tougher to use with PPX}
  @L{Still a TBD item for us})); Marshaling


(slide-group "Conclusion"
(gslide () @h1{Take Home Points (redux)}
 ~
 @L{(Mostly) Pure Functional is great for DApps}
 ~
 @L{Types can guide writing and refactoring your App}
 ~
 @L{}
 ~
 @L{Alacris is building open-source infrastructure})

(gslide () @h1{The Meta-Story}
 ~
 @L{Types can be your friends or your enemies}
 ~
 @L{Life is happier when they are your friends}
 ~
 @L{Life is happier when type friends are less dumb}
 ~
 @L{OCaml type friends are decent, could be even better})

(gslide () @h1{Thanks}
 ~
 @L{Our startup:   @em{Alacris}  @url{https://alacris.io/}}
 ~
 @L{WE ARE EITHER HIRING OR BANKRUPT!   (No constructive proof yet.)}
 ~
 @L{DO YOU HAVE EXTRA MILLIONS?   Seed capital, Research grants…}
 ~
 @L{SHOW ME THE CODE!   @url{https://github.com/AlacrisIO/legicash-facts}}))

(reveal)
