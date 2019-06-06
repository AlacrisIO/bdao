#lang at-exp racket @; -*- Scheme -*-
#|
;; Building Distributed Applications in OCaml
;; LambdaConf 2019, Friday June 7th, 1300-1350

;; https://lambdaconf.zohobackstage.com/LambdaConf2019#/agenda?day=3&filterBy=speaker&filterValue=6967000000308154&lang=en

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
  @C[style: "font-size: 1.2em; font-weight: bold;"]{Making types your friends, not your enemies}
  ~
  @p{François-René Rideau, @(~)@em{Alacris} @(br) @ocaml{fare@"@"alacris.io}}
  @(L(@small ~))
  @p{LambdaConf 2019, 2019-06-07 @(br)
     @url{https://alacrisio.github.io/bdao/}})

(define (take-home #:redux redux)
  (gslide () @h1{Take Home Points@(when redux " (redux)")}
    ~
    @L{Typed FP enables robust programming at scale}
    ;; thanks to the power of abstraction and composition. ; TODO: slides for that?
    ~
    @L{You pay a steep price for it}
    ~
    @L{Pure FP is especially fit for distributed applications}
    ~
    @L{To improve Typed FP: better types & metaprogramming}))

(slide-group "Introduction"

(gslide () @h1{This talk}
  @L{A collection of general-purpose techniques}
  @L{In OCaml, yet relevant to any modern typed FP}
  @L{Oriented towards distributed applications}
  @L{Used in the context of blockchain, but more general}
  ~
  @fragment[@C{... From a Lisper at heart}])

(gslide () @h1{Going from Lisp to OCaml}
  ~
  @L{Experience report adopting language-enforced types}
  ~
  @L{The Good: where the typesystem really helps}
  @L{The Bad: where the typesystem gets in the way}
  @L{The Ugly: how the typesystem has to improve})

(gslide () @h1{Why use OCaml rather than Lisp?}
  @L{Cryptocurrency applications: can't afford a single bug.}
  ~
  @L{Types can find bugs @em{before} deployment.}
  @L{Types critical to communicate design constraints to coworkers.}
  @L{Modules enable robust abstraction over many levels of semantics}
  @L{Having to put types first makes you ask important questions.})

(gslide () @h1{Downsides of OCaml versus Lisp?}
  @L{The typesystem rejects the Lisp-easy solution.}
  @L{The syntax is a hindrance at many levels.}
  @L{Types-first makes exploration harder.}
  @L{Type errors can boggle the mind.}
  @L{Dynamic evaluation is slow and awkward.}
  @L{Static patterns are repetitive and error-prone.}
  @L{No macros. PPX a poor yet expensive substitute.})

(take-home #:redux #f))


;; TODO: make that a group of slides, not a single slide?
(gslide () @h1{Bugs eliminated by types}
  @L{Type mismatch (@tt{int} for @tt{string}, etc.)}
  @L{Improper function calls}
  @L{Pattern exhaustiveness}
  @L{Errors in untested intermediate computations}
  @L{Incomplete refactoring}
  @L{Abstraction violation (using parametrized modules)})

(gslide () @h1{Bugs not eliminated by types}
  @L{Logic bugs within a module}

  @L{Configuration bugs}
  @comment{Or bugs in your extension language}

  @L{Integer interval errors}
  @comment{But fewer, and caught earlier}

  @L{Concurrency Issues}
  @comment{They can be narrowed, using session types...
           but only so much, and not when interacting with existing services}

  @L{}
  @comment{}
  @L~
  @C{Still, by restricting interactions,
     bugs are not just fewer, but simpler @(br)
     — they fit a brainful.})

(gslide () @h1{Performance}
  @L{Skipping runtime typechecks!}
  @L{Types enable many optimizations}
     @comment{Even more for pure functional code, but OCaml cannot afford to do them}
  ~
  @L{Dynamic evaluation when needed is actually slower} ;; bad manual interpreters.
  @L{Macros are harder, limiting user-provided optimizations}) ;; no manual compilers.

(gslide () @h1{Monads vs Lisp}
   (table class: "noborder" id: "noborder"
     (tr (th "Monad") (th "Lisp"))
     (tr (tL "Error") (tL "raising conditions"))
     (tr (tL "State") (tL "assignment"))
     (tr (tL "Future") (tL "threads, futures"))
     (tr (tL "Reader") (tL "dynamic parameters"))
     (tr (tL "Control") (tL "partial continuations"))
     (tr (tL "NonDet") (tL "non-determinism"))
     (tr (tL "IO") (tL "intercepting IO primitives"))
     (tr (tL "Parsing") (tL "parser DSL"))))

@comment{
In the future, also reify network communication,
and maybe also thread scheduling, so that failure scenarios
can be recorded and replayed, to find bugs and write tests.

Lisp: extend with macros
Haskell: language extensions (very expensive)
}

;; TODO: example of what is obviously allowed or not allowed with monad on or off. (moffad?)
(gslide () @h1{Monads vs Lisp costs & benefits}
  @L{Monads make explicit what effects may happen where}
  @comment{So it's easier to reason about who does what,
           but it's costlier to express computations}
#| Makes it easier to reason about the program: most parts have no effects, and those
parts that have effects can focus on that effect and remain simple.
|#

  @L{Monads force you to think about responsibility}
  @comment{So it's harder to fail to handle those responsibilities,
           but it's costlier to express computations}

  @L{Monads introduce plumbing that implementations may not support}
  @comment{Thus you may use monads in Lisp too, for partial continuations, parsing, etc.}

  @L{Monads build higher towers of structure}
  @comment{Lisp keeps the structure flatter and simpler;
           it introduces many opportunities for cheap punning and conciseness;
           but also many opportunities for bugs, abstraction leakage, etc.}

  @L{Monad syntax is low-level - CPS or ANF, not nested expressions}
  @comment{
   Importantly, it's DIFFERENT from the regular syntax.
   This is a big problem in refactoring both ways as you move the effects around,
   and you must switch between syntaxes.
   A better solution is possible --- just not implemented.

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
  })

(slide-group "Marshaling"
(gslide () @h1{Marshaling data to bytes and back}
@L{Translating data structures into byte sequences}
@L{a.k.a. Serializing, Encoding, Formatting, Unparsing}
~
@L{Must be matched by @em{unmarshaling} (a.k.a. etc.)}
~
@L{Necessary to communicate and/or persist data}
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

(slide-group "Content-Addressed Storage"
(gslide () @h1{Content-Addressing})

(gslide () @h1{Abstracting Content-Addressing}
  @L{wrap values in lazy-loading}
  @L{Higher-Order modules to abstract over persistence of not}
  @fragment{@L{Break the pure interface???}})

(gslide () @h1{Is it an effect?}
  @L{Escape to impure behavior}
  @L{Pure at one level, impure at another}
  @L{Same for logging}
  @fragment{@L{Same for lazy evaluation}}
  @fragment{@L{Same for allocation!}}
  @fragment{@C{Same for everything!}}))

(slide-group "Optics"
(gslide () @h1{Lenses})

(gslide () @h1{Zippers}
  @L{Lens as data rather than function})

(gslide () @h1{Zipping through a tree}
  @tt{
  type (+'a) step @(br)
  type (+'a) path @(br)
  val step_map: ('a -> 'b) -> 'a step -> 'b step
  val path_map: ('a -> 'b) -> 'a path -> 'b path
  type zipper = t * t path
  val zip : t -> zipper
  val unzip : zipper -> t
  val next: zipper -> zipper list
  val find_path : key -> t -> zipper
  })

(gslide () @h1{Merklization}
  @L{Merklize: make computations verifiable}
  @L{Zippers trivialize merklization:}
  @ocaml{
let get (key: key) (mt: mtrie) : t option =
  match map_fst Wrap.get (find_path key mt) with
    | Leaf {value}, up ->
        Some { key ; trie = dv_digest mt
             ; leaf = SynthMerkle.leaf value
             ; steps = (path_digest up).steps }
    | _ -> None}))

(slide-group "Conclusion"
 (take-home #:redux #t)

(gslide () @h1{The Meta-Story}
 ~
 @L{Types can be your friends or your enemies}
 ~
 @L{Life is happier when they are your friends}
 ~
 @L{Life is happier when type friends are less dumb}
 ~
 @L{OCaml type friends are decent, could be even better})

@comment{
(Note that the techniques involved can be used even in absence of a static type system insufficient to fully check the correctness of your proper usage—for instance a type system without dependent types such as in OCaml.)
}

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

;; Why wouldn't have had any chance of delivering without this infrastructure without this.
;; Lisp OO vs OCaml modules
