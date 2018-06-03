# CLOJURE IN ACTION



## CHAPTER 1: Introducing Clojure


#### BASIC
Clojure’s strengths don’t lie on a single axis. On the one hand, it’s designed as a
hosted language, taking advantage of the technical strengths of platforms like the JVM ,
Microsoft’s Common Language Runtime ( CLR ), and JavaScript engines on which it
runs, while adding the “succinctness, flexibility, and productivity” (http://clojure.org/
rationale) of a dynamically typed language. Clojure’s functional programming fea-
tures, including high-performance immutable data structures and a rich set of API s
for working with them, result in simpler programs that are easier to test and reason
about. Pervasive immutability also plays a central role in Clojure’s safe, well-defined
concurrency and parallelism constructs. Finally, Clojure’s syntax derives from the Lisp
tradition, which brings with it an elegant simplicity and powerful metaprogramming
tools (http://clojure.org/rationale).


#### LISP
Clojure is a fresh take on Lisp, one of the oldest programming language families still
in active use (second only to Fortran). Lisp isn’t a single, specific language but rather
a style of programming language that was designed in 1958 by Turing award winner
John McCarthy. Today the Lisp family consists primarily of Common Lisp, Scheme,
and Emacs Lisp, with Clojure as one of the newest additions.


#### FUNCTIONAL
The minimum requirement to be a functional language is to treat functions as
something more than named subroutines for executing blocks of code. Functions in
an FP language are values, just like the string "hello" and the number 42 are values.
You can pass functions as arguments to other functions, and functions can return
functions as output values. If a programming language can treat a function as a value,
it’s often said to have **“first-class” functions**.


#### FUNCTIONAL LANGUAGE FEATURES
In addition to functions as first-class values, most FP languages also include the fol-
lowing unique features:
* **Pure functions** with **referential transparency**
* **Immutable data structures** as the default
* Controlled, explicit changes to **state**

These three features are interrelated. Most functions in an FP design are pure, which
means they don’t have any side-effects on the world around them such as changing
global state or doing I/O operations. Functions should also be referentially transparent, 
meaning that if you give the same function the same inputs, it will always return the same output.
At the most elementary level, functions that behave this way are simple, and it’s simpler 
and easier to reason about code that behaves consistently, without respect to the implicit 
environment in which it runs. Making immutable data structures the language default guarantees 
that functions can’t alter the arguments passed to them and thus makes it much easier to write pure, 
referentially transparent functions. In a simplistic sense, it’s as if arguments are always passed by
value and not by reference.

> “Hold on,” you might say, “passing arguments by value and copying data structures
> everywhere is expensive, and I need to change the values of my variables!” Clojure’s
> immutable data structures are based on research into the implementation of perfor-
> mant, purely functional data structures designed to avoid expensive copying. 2 In the-
> ory, if you make a change to an immutable data structure, that change results in a
> brand-new data structure, because you can’t change what’s immutable. In reality, Clo-
> jure employs **structural sharing** and other techniques under the hood to ensure that
> only the minimum amount of copying is performed and that operations on immuta-
> ble data structures are fast and conserve memory. In effect, you get the safety of pass-
> ing by value with the speed of passing by reference.


Dve slike kao primer, plus kratko objasnjenje


#### VALUES
Clojure lets you change the values variables hold but with well-defined semantics
regarding how and when the changes take place. If you have one variable and you
want to change its value, Clojure lets you do that **atomically**, so you’re certain that if
multiple threads of execution are looking at a variable’s value, they always get a con-
sistent picture, and that when it changes it does so in a single, atomic operation. If
you need to change multiple variables together as a unit, Clojure has a separate facil-
ity using its **software transactional memory ( STM )** system to change multiple vari-
ables as part of a transaction and rollback changes if they don’t all complete as
expected. If you need to change a variable but want that change to happen on a sep-
arate thread of execution so it doesn’t block the main thread of your program,
Clojure provides facilities for that as well.

> In this case, **“atomic”** is a synonym for “indivisible.” If an operation is atomic, then no other operations can
> interfere with the underlying state while it’s being changed. If any other processes attempt to get the state of
> a variable during an atomic operation, they simply get the last value of the variable before the atomic opera-
> tion began. In the case of other processes attempting to change the underlying state during an atomic oper-
> ation, they’re held off until the atomic operation is complete.


#### FUNCTIONAL PURITY
Functional languages are often judged by their functional “purity,” or strict adher-
ence to the theoretical underpinnings of functional programming language design.
On the one hand, Clojure’s default use patterns encourage pure functional program-
ming: immutable data structures, higher-order functions and recursion that take the
place of imperative loops, and even a choice between lazy or eager evaluation of col-
lections. On the other hand, Clojure is pragmatic. Even though most problems can be
solved using immutable data structures and functional programming patterns, certain
tasks are more clearly modeled with mutable state and a more imperative approach.


#### CLOJURE AND JVM
Clojure was designed as a **hosted language**. Whereas most programming language proj-
ects combine a language design with an accompanying runtime platform for that lan-
guage, Rich Hickey, Clojure’s creator, decided to focus on Clojure-the-language and
rely on existing VM s for the runtime platform. He began his work on the JVM , but Clo-
jure has since spread to the CLR with interoperability with the . NET ecosystem (Clojure-
CLR ), as well as to browser and server-side JavaScript engines (ClojureScript).

#### FIRST CLASS CITIZEN
Clojure compiles down to bytecode on the JVM
and Common Intermediate Language ( CIL ) on the CLR , meaning that it participates
as a first-class citizen of the VM s it runs on.


### LISP SYNTAX

#### INFIX & PREFIX NOTATION
(+ 1 2) ; prefix
1 + 2   ; postifx

(+ 1 2 3)
(str "First" "Second")

Prefix notation is also called **polish notation**.

#### ADVANTAGES TO PREFIX NOTATION
First, there’s no difference between functions and operators because **Clojure doesn’t have operators**.
*There’s no system of operator precedence to memorize*. The forms str and + are both
regular Clojure functions; one just happens to have a nonalphabetic character as its
name. Second, because you don’t need to interleave operators between arguments,
it’s natural for these kinds of functions to take an arbitrary number of arguments
(called **variable arity**), allowing you to add more arguments without fear of forgetting
to put an operator in between each one.


#### PARENTHESES ()

(+ 3 8)

Parenthesis in Clojure serve 2 purposes: (simplifed example)
* Calling functions
* Creating Lists

#### CALLING FUNCTIONS
All of the code so far has shown examples of the first pur-
pose—to call functions. **Inside a set of parentheses, the first
language form is always a function, macro, or special form,
and all subsequent forms are its arguments.** Figure 1.3 is a
simple example of this use of parentheses. We’ll cover 
what macros and special forms are as we encounter them,
but for now you can think of them as functions that get 
special treatment.

LEFT PARENTHESIS = FUNCTION INVOCATION

#### CREATING LISTS & LISP POWER
On the one hand, Clojure has literal syntax for collections other than lists,
and idiomatic Clojure programs use all of the collection types based on their different per-
formance strengths. Clojure isn’t as list-centric as other Lisps, in part because it provides lit-
eral syntax for these other types of collections. On the other hand, **at the meta level, your
entire Clojure program is a series of lists**: *the very source code of your program is inter-
preted by the Clojure compiler as lists that contain function names and arguments that need functions
to be parsed, evaluated, and compiled*. Because the same language features are available at both
the lower compiler levels and in your normal program code, Lisp enables uniquely pow-
erful **meta-programming capabilities**.


#### JVM INTEROP
Interoperation with host platform (JVM) = INTEROP

Java is three distinct pieces that were designed and shipped together: **a language**, a
**virtual machine**, and a **standard library**. Parts of Clojure are written in Java the lan-
guage, but Clojure itself doesn’t use it.

Instead Clojure code is compiled directly into bytecode for the JVM to run. 
Clojure also requires you to use the standard library for many basic functions.

In many cases Clojure uses Java types and the standard library directly. For example,
strings in Clojure are Java String objects and literal numerals are Java Long objects, and
Clojure’s collections implement the same collection interfaces implemented by Java
collections. Reusing Java types and interfaces has the added benefit that Java code can
use Clojure types (such as its immutable data structures) seamlessly.

Sometimes Clojure wraps Java library features with functions of its own, like many
of the functions in Clojure’s clojure.string namespace that delegate to methods in
Java’s String class. But often there’s no Clojure wrapper and you’ll need to call Java
methods directly. For example, Clojure doesn’t implement regular functions for
mathematical methods like abs[olute], exp[onent], log, sin, cos, and tan found in the
java.lang.Math 6 class, which therefore need to be invoked via the Java interop syntax.


#### JAVA TYPES, CLASSES, OBJECTS
Java is an *object-oriented language* based on a *class hierarchy with single inheritance*.
In addition to classes, common behaviors can be grouped into *interfaces*, which act as
simple outlines of method signatures that classes that implement the interface must
support. Only one public class or interface can be defined in a single file, and those
files must be placed in directories that are on *Java’s classpath*. The Java classpath is
akin to C’s search path or Ruby’s $LOAD_PATH in that it’s a collection of directories that
the Java compiler will search when looking for files to compile as part of your pro-
gram. The fully qualified name of a Java class or interface consists of its *package name*
followed by the *name of the class or interface being defined*; for example, Java’s Math
class is situated in the java.lang package. This allows individual classes to share the
same name (for example, Math ) as long as they aren’t in the same package and thus
have a unique full name when loaded into the JVM (for example, java.lang.Math vs.
com.mycompany.Math ).

All the classes in java.lang are imported by default in all Clojure programs.

Many CLojure data structures (especially collections) implement Java interfaces.


#### INTEROP
. - operator/function basic java interop

(. Math PI)
(. Math abs -3)
(. "foo" toUpperCase)

#### SYNTASTIC SUGAR
To accommodate the fact that, outside of Java interop, the first form in a Clojure
expression is a function, macro, or special form, Clojure provides some syntactic sugar
to make this code more idiomatic.

Math/PI
(Math/abs -3)

Fields and methods that are **static** (defined on the class and not on instances of the
class) are accessed with a **forward slash**. In the Java Math class, PI is a static field and
not a method, so it doesn’t need to be *invoked (using parentheses)* to return a value.
But abs is a method, so it must still be invoked with parentheses.

Rewrite to make it look more like function call:
(.toUpperCase "foo")

#### INSTANCE OF CLASS
To create instances of classes, you can use the **new operator** or a **trailing dot** to indicate
that the class’s constructor should be called:

(new Integer 44)
(Integer. "42")

The trailing dot here, the leading dot for instance fields and methods, and the for-
ward slashes for static fields and methods are all syntactic conveniences. During Clo-
jure’s macro expansion phase of compiling code, the trailing dot expands to use the
new special form, and the others expand to the standalone dot form demonstrated at
the beginning of this section, so they’re all literally equivalent by the time your Clo-
jure code is evaluated.


#### THREADS AND CONCURRENCY
A thread represents program execution. Every program, regardless of programming
language, has at least one main thread or process in which the application code is
being evaluated. In addition to this main application thread, language runtimes gen-
erally provide a way to start new, separate threads of execution.

JVM threads map directly to native system threads, which means they can take advantage 
of multiple CPU cores “for free” by letting the operating system manage scheduling threads 
and delegating to CPU s. By engaging all available cores on a machine using native threads, 
the JVM provides genuine and performant parallelism.

By virtue of the fact that Clojure’s core data structures are all immutable, the issue
of shared mutable state becomes largely moot. When some mutable state is required,
Clojure provides concurrency data structures called vars, atoms, refs, and agents that
have clearly defined semantics for how to change the underlying state they reference.


-------------------------------------------------------------------------------


## CHAPTER 2: Data structures and types

#### REPL (READ EVALUATE PRINT LOOP)
REPL u clojure je veoma mocan alat za razvoj.

user> 

- default namespace and prompt for repl

FLOW:
1. TYpe in a form (press enter)
2. Clojure *reader* accepts stream of characters from prompt and converts it to Clojure *data structures*
3. Data structures are *evaluated* to produce the result of program (usully another data structure)
4. Clojure *printer* attempts to print the result in a format that can be read back by the reader
5. Finaly CLojure *loops* back and waits for more input

#### FORM / EXPRESSION
> If you want to be super-pedantic, there’s a distinction between “form” and “expression.” A form is a single
> readable unit, such as a number, a pair of matched quotation marks for a string, or a pair of matched paren-
> theses. Forms are what matter when reading. An expression is something with a value, such as a data structure.
> Expressions are what matter when evaluating. This distinction between forms and expressions, reading and
> evaluating, is another symptom of Lisp’s nonsyntax as explained in chapter 1: it’s possible to read forms with-
> out knowing how to evaluate them as expressions!

#### VAR

``` clojure
(def my-addition (fn [operand1 operand 2] (+ operand1 operand2)))
```

> Expression defines a namespace-qualified global *user/my-addition* , which contains an addition function.
> That funny-looking *#'user/my-addition* is a var (created and returned by def ). A **var**
> is a named, mutable container that holds a value, in this case the addition function.
> You’ll learn more about vars later. For now, just know that if you want to save the value
> of an expression to refer to it later, use *(def variable-name "value to save")*


#### INVOKATION

``` clojure
(my-addition 100 30)
=> 130
```

#### RETURN
There is no *explicit* return, the last expression evaluated in function is return. (similar in ruby)


#### HELLO WORLD

``` clojure
(println "Hello, world!")
Hello, world!
=> nil
```

> Pretty simple, right? But there are still a few points to note. First, notice that “Hello,
> world!” was printed on a line by itself, with no => in front of it, and that a second line
> says nil . What’s going on? The function println is unusual (well, for Clojure)
> because it’s a side-effecting function: it prints a string to standard-out and then
> returns nil . Normally you want to create pure functions that only return a result and
> don’t modify the world (for example, by writing to the console). So the Hello, world!
> line was printed during the REPL ’s evaluation phase, and the => nil line was printed
> during the REPL ’s print phase.

#### MAGIC REPL VARS & GUIDE

\*1 - last successfuly read form
\*2 - second last
\*3 - third last
\*e - last error

(doc name)          - look up function documentation
(find-doc "string") - finds documentation for all f() and macros which match string
(apropos 'doc)      - similar to find-doc but only prints the names of functions that match

#### CLOJURE SYNTAX

* Prefix notation
  > In most languages, mathematical functions like addition and subtraction are special
  > cases built into the language as operators to make it possible to represent math in the
  > more familiar in-fix notation. Clojure avoids this special case by not having any opera-
  > tors at all. Instead, math operators are just Clojure functions. All functions work the
  > same way, whether they’re math related or not.


* Whitespace
  Clojure uses parentheses(braces, square brackets) to delimiter code. 
  You dont need to use commas to dlimit elements of list, but you can and clojure will ignore it.
  (+ 1 2 3 4)
  (+ 1, 2, 3, 4)

  Clojure uses commas sometimes for printing some results in REPL.


* Comments
  ; one or more semi-colon start a comment
  
  #### Conventions
  * Single ;   - used where comment appears after some program text
  * Double ;;  - comment out entire line
  * Triple ;;; - block comments
  
  #### Macro comment
  Clojure provides conveninet macro **comment** that can be used for multiline comments.

``` clojure
  (comment
    (defn this-is-test [x y]
      (+ x y)))
  => nil
```

* Case sensitivity
  Clojure is **case-sensitive**. While most LISPs are NOT case sensitive.
  
  
#### CLOJURE DATA STRUCTURES

#### FALSE
nil = null - nothing

true, false

Boolean values - everything other then *false and nil* is considered *true*.


#### CHARACTERS AND STRINGS

* CHARACTERS
Clojure characters are *Java characters - unsigned 16-bit UTF-16 code points*.
Clojure has a *reader macro* \ it is used to denote a character \a \g.


* STRINGS
Are *Java strings*. They are denoted using double quotes "string", because single quote
is a reader macro.

Java STRING Class methods:
(.contains "clojure-in-action" "-")
(.endsWith "program.clj" ".clj")

> . Used to call nonstatic Java method


* NUMBERS
Basic numbers that are being used:
* 64-bit Integers (Java primitive longs)
* 64-bit floating point numbers (Java primitive doubles)

For bigger numbers we use *big integers* or *big decimals*.

* RATIO
Less common number type. Created when 2 int are divided such that they cant be reduces any further.
(/ 4 9) => 4/9

* CONTAGIOUSNESS
When different number types mix toghter the number with higher contagiousness will "infect" the result
with its type.

* OVERFLOW
When number operation produces result that is too large to represent as CLojure int.  operations: + / *
Then it throws java.lang.ArithmeticException.

* PROMOTING NUMBER RESULT
If we want Clojure to auto-promote the result to big int instead there is alternative syntax for math functions.
+' -' inc' dec' - same as normal but with quote at end


#### SYMBOLS AND KEYWORDS

> Symbols are the identifiers in a Clojure program, the names that signify values. For
> example, in the form (+ 1 2) the + is a symbol signifying the addition function.
> Because Clojure separates reading and evaluating, symbols have two distinct aspects:
> their existence in the program data structure after reading and the value that they
> resolve to. Symbols by themselves are just names with an optional namespace, but when
> an expression is evaluated they’re replaced with the value they signify.

> Here are some representative examples of valid symbols: foo , foo/bar , ->Bar ,
> -foo , foo? , foo-bar , and foo+bar . And here are some invalid attempts at symbols: /bar ,
> /foo , and +1foo .

> In a program, symbols normally resolve to something else that isn’t a symbol. But
> it’s possible to treat a symbol as a value itself and not an identifier by quoting the sym-
> bol with a leading single-quote character. The quote tells the reader that the next
> form is literal data and not code for it to evaluate later. Notice the difference:

``` clojure
arglebarg
;CompilerException java.lang.RuntimeException: Unable to resolve symbol:
;arglebarg in this context.

'arglebarg
;=> arglebarg
```


**Essentially what’s happening when you quote a symbol is you’re treating the sym-
bol as data and not code.** In practice you’ll almost never quote symbols to use them as
data because Clojure has a special type specifically for this use case: the **keyword**. A key-
word is sort of like an autoquoted symbol: keywords never reference some other value
and always evaluate to themselves. Keyword syntax is almost like symbol syntax, except
keywords always begin with a colon. Here are some examples of keywords: :foo , :foo/
bar , :->foo , and :+ . You’ll end up using keywords very often in your Clojure code, typ-
ically as keys in hash maps and as enumerated values.

Additional ways to construct keyword and symbol - symbol, keyword functions.
Examine them with: name, namespace

``` clojure
(keyword "foo")
;=> :foo

(symbol "foo" "bar")
;=> foo/bar

(name :foo/bar)
;=> "bar"

(namespace :foo)
;=> nil

(name "baz")
;=> "baz"
```


#### LISTS

> Lists are the basic collection data structure in Clojure. If you’re familiar with lists from
> other languages, Clojure lists are **singly linked lists**, meaning that it’s easy to go from
> the first to the last element of a list but impossible to go backward from the last to the
> first element. This means that you can **only** add or remove items from the “front” of
> the list. But this also means that multiple different lists can share the same “tails.” This
> makes lists the simplest possible immutable data structure.

``` clojure
(list 1 2 3 4 5)
;=> (1 2 3 4 5)

(list? *1)
;=> true
```

* CONJ
Create a new list with another value added to it:

``` clojure
(conj (list 1 2 3 4 5) 6)
;=> (6 1 2 3 4 5 6) ; only adds/removes to the FRONT


(conj (list 1 2 3) 4 5 6)
;=> (6 5 4 1 2 3)

(conj (conj (conj (list 1 2 3) 4) 5) 6) ; same as previus
```

* PEEK, POP
You can treat lists as stack. peek returns head, pop returns tail.
Similar to LISP cdr/car to extent (looks less flexible).

``` clojure
(peek (list 1 2 3))
;=> 1

(pop (list 1 2 3))
;=> (2 3)
```

* COUNT
Count the number of item in a list:

``` clojure
(count (list))
;=> 0

(count (list 1 2 3 4))
;=> 4
```


#### SPECIAL STATUS OF LISTS

> The list is special because each expression of Clojure code is a list. The list may contain
> other data structures such as vectors, but the list is the primary one.

> In practice, this implies that lists are treated differently. Clojure assumes that the
> first symbol appearing in a list represents the name of a function (or a macro). The
> remaining expressions in the list are considered arguments to the function.

Similar concept in LISP with single quote: 
``` clojure
(+ 1 2 3)

(def numbers (1 2 3))    ; treats this as arguments 1 is not a function call
;=> ERROR

(def numbers '(1 2 3))   ; treats this as data
;=> #'user/numbers
```


#### VECTORS
Vectors are like lists, except for two things: they’re denoted using square brackets, and
they’re indexed by number. Vectors can be created using the vector function or liter-
ally using the square bracket notation:

``` clojure
(vector 10 20 30 40)
;=> [10 20 30 40]

(def another-vector [10 20 30 40 50])
;=> #'user/another-vector
```

* HELPER FUNCTIONS

``` clojure
(get another-vector 2)
;=> 30

(nth another-vector 2)
;=> 30

(get another-vector 10)
;=> nil

(nth antoher-vector 10)
;=> ERROR


(assoc another-vector 2 25)  ; change existing index
;=> [10 20 25 40 50]

(assoc another-vector 5 60)  ; you can add at the END
;=> [10 20 30 40 50 60]

(assoc another-vector 6 70)  ; you can NOT add past the end
;=> ERROR

(conj [1 2 3 4 5] 6)  ; adds element to the END (on lists it adds only on BEGGINING)
;=> [1 2 3 4 5 6]     ; fastest spot on vectors end, lists start

(peek [1 2])          ; also works from end
;=> 2

(pop [1 2])           ; end
;=> 1

(peek [])
;=> nil

(pop [])
;=> ERROR
```

* VECTOR ADDITIONAL PROPERTY
Vectors are also functions that can take single argument. Argument is index.
This allows for vectors to be used where functionsa are expected (functional composition)

``` clojure
(another-vector 3)  ; same as (get another-vector 3)
;=> 40
```

#### MAPS
Maps are similar to associative arrays or dictionaries in languages like Python, Ruby,
and Perl. A map is a sequence of key-value pairs. The keys can be pretty much any
kind of object, and a value can be looked up inside a map with its key. Maps are
denoted using braces.

> Map literals and the hash-map function aren’t exactly equivalent, because Clojure actually has two different map
> implementations: hash-map and array-map. Array maps store keys and values in sorted order and perform look-
> ups by scanning instead of hashing. This is faster for small maps, so smaller map literals (approximately 10 keys or
> less) actually become an array map instead of a hash map. If you assoc too many keys to an array map, you’ll even-
> tually get a hash map instead. (The opposite is not true, however: a hash map will never return an array map if it gets
> too small.) Transparently replacing the implementation of a data structure is a common performance trick in Clo-
> jure made possible by the use of immutable data structures and pure functions. The hash-map and array-map
> functions will always return the corresponding structure, regardless of the number of arguments you call them with.

``` clojure
;; DEFINITION
(def the-map {:a 1 :b 2 :c 3})
;=> #'user/the-map

(hash-map :a 1 :b 2 :c 3)
;=> {:a 1, :c 3, :b 2}


;; LOOKUP KEY-VALUES PAIRS
;; Similar as with vectors, maps are also functions

(the-map :b)
;=> 2

;; KEYWORDS
;; Are also functions

(:b the-map)
;=> 2

(:z the-map 26) ; keywords function also can return default value if not found
;=> 26
```

### ALL CLOJURE DATA STRUCTURES ARE IMMUTABLE


``` clojure
;; INSERTING NEW KEY VALUE INTO MAP
(def updated-map (assoc the-map :d 4))
;=> #'user/updated-map

updated-map
;=> {:d 4, :a 1, :b 2, :c 3}

(dissoc updated-map :a)  ; Remove key
;=> {::b 2, :c 3}
```

* CHANGING NESTED MAPS
Note the use of nested maps. Because maps are **immutable**, if you wanted to update
Kyle’s summary for his monthly average, you **couldn’t simply drill down to the spot** on
the map and update it in place as you would in most other languages. *Instead, you
would need to go down to the place you want to change, create the changed map, and
assoc the changes into the intermediate maps on your way back up to the root.* Doing
this all the time would be tedious and error prone.

``` clojure
(def users {:kyle {
              :date-joined "2009-01-01"
              :summary {
                :average {
                  :monthly 1000
                  :yearly 12000}}}})
                  
                  
;; UPDATION NESTED COLLECTIONS
(assoc-in users [:kyle :summary :average :monthly] 3000)
;=> {:kyle {:date-joined "2009-01-01", :summary {:average {:monthly 3000,
:yearly 12000}}}}

;; DOCS
(assoc-in map-name [key & more-keys] value)


;; READ NESTED STRUCTURE
(get-in users [:kyle :summary :average :monthly])
;=> 1000


;; UPDATE VALUES
(update-in users [:kyle :summary :average :monthly] + 500)
;=> {:kyle {:date-joined "2009-01-01", :summary {:average {:monthly 1500,
:yearly 12000}}}}

;; DOCS
(update-in map [key & more-keys] update-function & args)


```


#### SEQUENCES
A sequence isn’t a collection type. Rather, a sequence is an interface (called ISeq ) that
exposes a “one thing followed by more things” abstraction. This interface is imple-
mented pervasively by Clojure’s data structures, functions, and macros. The sequence
abstraction allows all data structures to look and act like lists, even if the underlying
values are some other collection type (such as a vector or hash map) or are even cre-
ated lazily as they’re needed.

Similar to peek/pop for collection types.

ISeq provides 3 functions:

``` clojure
(first (list 1 2 3))
;=> 1

(rest (list 1 2 3))
;=> 2 3

(rest [1 2 3])
;=> (2 3)

(first {:a 1 :b 2})  ; order of items is not guaranteed
;=> [:b 2]

(rest {:a 1 :b 2})   ; same
;=> [:a 1]

(first [])
;=> nil

(rest [])
;=> ()
```

* CONS
cons = construct - Similar thing exists in LISP (basic block)

Creates a new sequence given an element and existing sequence:

``` clojure
(cons 1 [2 3 4 5])     ; adds item to beginning of sequence (even on vectors)
;=> (1 2 3 4 5)         

(list? (cons 1 (list 2 3)))    ; sequence abstraction is LAZY
;=> false                      ; meaning that functions (first, rest , cons) dont do extra work to create lists
                               ; even though the results prints like a list
```

> The sequence abstraction allows everything to seem as though real lists were being
> manipulated but avoids actually creating any new data structures (such as actual lists)
> or doing any unnecessary work (such as creating items farther down the sequence that
> are never used).


#### PROGRAM STRUCTURE


#### FUNCTION

Functional language - functions are **first class citizens**. That means:
* created dynamically
* be passed as arguments to functions
* be returned from other functions
* be stored as values insidde other data structures

* FUNCTION DEFINITIONS

``` clojure
(defn name [arg1 arg2] 
  (+ arg1 arg2))
```

  
> In reality, the **defn macro** expands to a combination of calls to **def** and **fn** , where fn is
> itself another macro and def is a special form. Here, *def creates a var with the speci-
> fied name and is bound to a new function object*. This function has a body as specified
> in the defn form.

``` clojure
(def name
  (fn [arg1 arg2]
    (+ arg 1 arg2)))
```

fn - can be used to define ANONYMOUS FUNCTIONS

#### VARIABLE ARITY
Parameter list of function - not fixed in number
[x y & more] ; in docs

Handles any number of arguments.


#### LET FORM

``` clojure
(defn average-pets []
  (/ (apply + (map :number-pets (vals users))) (count users)))
```

  
The let form allows you to introduce locally named things into your
code by binding a symbol to a value.

``` clojure
(defn average-pets []
  (let [user-data (vals users)
        pet-counts (map :number-pets user-data)
        total (apply + pet-counts)]
  (/ total (count users))))
```

* apply - calls a function with items of a sequence as individual arguments (apply + [1 2]) == (+ 1 2)


Here, user-data , pet-counts , and total are namespace-less symbols that resolve to
specific values but only in the scope of the let . Unlike vars created by def , these bind-
ings can’t be changed, only shadowed by other bindings in nested scopes.
You can introduce a local value computed from previously named values, within the same form, for instance:

``` clojure
(let [x 1
      y 2
      z (+ x y)]
  z)
;=> 3
```

More specifically, the let form accepts as its first argument a vector containing an
even number of forms, followed by zero or more forms that get evaluated when the
let is evaluated. The value of the last expression is returned.


#### UNDERSCORE IDENTIFIER _
It’s worth discussing the situation where you might not care about
the return value of an expression. Typically, such an expression is called purely for its
side effect. A trivial example is calling println , because you don’t care that it returns
nil . If you do this inside a let form for any reason, you’ll need to specify an identifier
in which to hold the return value. The code might look like this:

``` clojure
(defn average-pets []
  (let [user-data (vals users)
        pet-counts (map :number-pets user-data)
        value-from-println (println "total pets:" pet-counts)    ; we dont care about saving this value
        total (apply + pet-counts)]
  (/ total (count users))))
```

In this code, the only reason you create value-from-println is that the let form
needs a name to bind the value of each expression. In such cases where you don’t care
about the value, you can just use a single underscore as the identifier name.

``` clojure
(defn average-pets []
  (let [user-data (vals users)
        pet-counts (map :number-pets user-data)
        _ (println "total pets:" pet-counts)
        total (apply + pet-counts)]
  (/ total (count users))))
```


#### SIDE EFFECTS WITH DO
In a pure functional language, *programs are free of side effects*. The only way to “do
something” is for a function to compute a value and return it. Calling a function
doesn’t alter the state of the world in any way. Consider the following code snippet:

``` clojure
(defn do-many-things []
  (do-first-thing)
  (do-another-thing)
  (return-final-value))
```

In a world without state and side effects, the do-many-things function would be
equivalent to this one:

``` clojure
(defn do-many-things-equivalent []
  (return-final-value))
```

*The calls to do-first-thing and do-another-thing can be eliminated without
change in behavior, even without knowing what they do.* This is because in a stateless
world without side effects, the only thing that “does something” in do-many-things is
the last function call to return-final-value , which presumably computes and returns
a value. In such a world, there’d be no reason to ever call a series of functions (as
shown in the first example), because only the last one would ever do anything useful.

To combine multiple s-expressions into a single form, Clojure provides the **do form**. 
It can be used for any situation as described previously where some side effect is
desired and the higher-order form accepts only a single s-expression. As an example,
consider the if block:

``` clojure
(if (is-something-true?)
  (do
    (log-message "in true branch")
    (store-something-in-db)
    (return-useful-value)))
```
Normally, because the consequent part of the if form accepts only a single s-expression,
without the do as shown here, it would be impossible to get the true case to call all three
functions ( log-message , store-something-in-db , and return-useful-value ). (Similar to LISP)

**The do form is a convenient way to combine multiple s-expressions into one. This
is a common idiom in macros.**


#### READER MACROS
The *Clojure reader converts program text into Clojure data structures*. It does this by
recognizing that characters such as parentheses, braces, and the like are special and
that they form the beginning (and ending) of lists, hash maps, and vectors. These rules
are built into the reader.

Other characters are special also, because they signal to the reader that the form
that follows them should be treated in a special way. In a sense, these characters extend
the capability of the reader, and they’re called **reader macros**. The simplest (and most
traditional) example of a reader macro is the comment character ( ; ). When the
reader encounters a semicolon, it treats the rest of that line of code as a comment and
ignores it.

Quote ( '                             ) Quotes the form following it, same as (quote )
Character ( \                         ) Yields a character literal
Comment ( ;                           ) Single-line comment
Meta ( ^                              ) Associates metadata for the form that follows
Deref ( @                             ) Dereferences the agent or ref that follows
Dispatch ( #                          ) #{} Constructs a set
                                        #"" Constructs a regex pattern
                                        #^ Associates metadata for the form that follows (deprecated by ^ )
                                        #' Resolves the var for the symbol that follows, same as ( var    )
                                        #() Constructs an anonymous function
                                        #_ Skips the following form
Syntax quote ( `                      ) Used in macros to render s-expressions
Unquote ( ~                           ) Unquotes forms inside syntax-quoted forms
Unquote splice ( ~@                   ) Unquotes a list inside a syntax form, bu


Reader macros are implemented as entries in a read table. An entry in this table is
essentially a reader macro character associated with the macro function that describes
how the form that follows is to be treated. Most Lisps expose this read table to the pro-
grammers, allowing them to manipulate it or add new reader macros. Clojure doesn’t
do this, and so you can’t define your own reader macros. Starting with Clojure 1.4,
Clojure does let you define your own data literals.


#### PROGRAM FLOW

#### CONDITIONALS

* IF
If can be a single s-exp (symbolic expression) you must use **do** form for it to do multiple forms.
If is a special form (which means that Clojure implements it internally as special case)

``` clojure
(if test consequent alternative)

(if (> 5 2)
  "yes"
  "no")
```

  
* IF-NOT
(if-not test consequent alternative)

* COND
Flatten nested if conditionals

``` clojure
(cond & clauses)

(def x 1)

(cond
    (> x 0) "greater!"
    (= x 0) "zero!"
    :default "lesser!")      ; :default if no other conditions pass
;=> "greater!"
```

* WHEN
Convinient macro that is and if (without alternative clause) along with an *implicit do*.
This allows multiple s-exp to be passed in as body.

``` clojure
(when (> 5 2)
    (println "five")
    (println "is")
    (println "greater")
    "done")
; five
; is
; greater
;=> "done"
```


* WHEN-NOT

``` clojure
(when (< 5 2)
    (println "two")
    (println "is")
    (println "smaller")
    "done")
; two
; is
; smaller
;=> "done"
```

> Except for the if special form, they’re all implemented as mac-
> ros, which also implies that the programmer is free to implement new ones, suited to
> the domain of the program


#### LOGICAL FUNCTIONS

* AND
and accepts zero or more forms. It evaluates each in turn, and if any returns nil or
false , and returns that value. If none of the forms return false or nil , then and
returns the value of the last form. and short-circuits the arguments by not evaluating
the remaining if any one returns a falsey value. A simple rule to remember the return
value of and is that it returns the “deciding” value, which is the last value it had to
examine, or true if there are no values.

``` clojure
(and)
;=> true

(and :a :b :c)      ; returns last
;=> :c

(and :a nil :c)     ; returns first falsy value
;=> nil

(and :a false :c)
;=> false

(and 0 "")          ; nil and false are logically false everything else is true
;=> ""
```


* OR
It also accepts zero or more forms and evaluates them
one by one. If any returns a logical true, it returns it as the value of the or . If none
return a logical true, then or returns the last value. or also short-circuits its argu-
ments.

``` clojure
(or)
;=> nil

(or :a :b :c)
;=> :a

(or :a nil :c)
;=> :a

(or nil false)
;=> false

(or false nil)
;=> nil
```

* NOT
Logical inversion

``` clojure
(not true)
;=> false

(not 1)
;=> false

(not nil)
;=> true
```

* COMPARISSON
All excected comparisson and equality functions exits. < <= > >= =
Difference is that they can take ANY number of arguments

``` clojure
(< 2 4 6 8)
;=> true

(< 2 4 3 8)
;=> false
```

* =  / ==
=  - Java equal with wider range of obj (nil, numbers, sequences)
== - Only compares numbers

``` clojure
(= 1 1N 1/1)
;=> true

(= 0.5 1/2)
;=> false

(= 0.5M 0.5)
;=> false

(= 0.5M 1/2)
;=> false
```

False values are because numbers are of different classes so you must use == instead.


#### FUNCTIONAL ITERATION

> Most functional languages don’t have traditional iteration constructs like for because
> typical implementations of for require mutation of the loop counter. Instead, they
> use recursion and function application to process lists of things.


* WHILE
Similar to other languages

``` clojure
(while test & body)

(while (request-on-queue?)
  (handle-request (pop-request-queue)))
```

Here, requests will continue to be processed as long as they keep appearing on the
request queue. The while loop will end if request-on-queue? returns a value either
false or nil , presumably because something else happened elsewhere in the system.



* LOOP / RECUR

``` clojure
(defn fact-loop [n]
  (loop [current n fact 1]                       ; current = n, fact = 1 > vector assignemnt
    (if (= current 1)
      fact                                       ; retuns whatever val fact has when current == 1
      (recur (dec current) (* fact current)))))  ; if current isnt 1 repet loop with new params
```


``` clojure
(loop bindings & body)
```
loop sets up bindings that work exactly like the let form does. In this example, [current
n fact 1] works the same way if used with a let form: current gets bound to the value
of n , and fact gets bound to a value of 1 . Then it executes the supplied body inside
the lexical scope of the bindings. In this case, the body is the if form.

``` clojure
(recur bindings)
```
The bindings are computed, and each value is bound to the respective name as
described in the loop form. Execution then returns to the start of the loop body. In
this example, recur has two binding values, (dec current) and (* fact current) ,
which are computed and rebound to current and fact . The if form then executes
again. This continues until the if condition causes the looping to end by not calling
recur anymore.


recur is a special form in Clojure, and despite looking recursive, it doesn’t con-
sume the stack. It’s the preferred way of doing self-recursion, as opposed to a function
calling itself by name. The reason for this is that Clojure currently doesn’t have tail-
call optimization, though it’s possible that this will be added at some point in the
future if the Java virtual machine ( JVM ) were to support it. recur can be used only
from tail positions of code, and if an attempt is made to use it from any other position,
the compiler will complain. For instance, this will cause Clojure to complain:

``` clojure
(defn fact-loop-invalid [n]
  (loop [current n fact 1]
    (if (= current 1)
      fact
      (recur (dec current) (* fact current)))    ; must be on tail (last)
    (println "Done, current value:" current)))
;=> ERROR
```


* DOSEQ / DOTIMES

``` clojure
(defn run-report [user]
  (println "Running report from" user))
  
(defn dispatch-reporting-jobs [all-users]
  (doseq [user all-users]                      ; loop through all-users as user
    (run-report user)))
```

Here, the form of interest is doseq . The simplest form accepts a vector containing two
terms, where the first term is a new symbol, which will be sequentially bound to each
element in the second term (which must be a sequence). The body will be executed
for each element in the sequence and then the entire form will return nil . In this
case, dispatch-reporting-jobs will call run-reports for each user present in the
sequence all-users .

``` clojure
(dotimes [x 5]            ; < from 0 to (n -1) = 4 
  (println "X is" x))
```

Not used very much in practise, alternative higher-level functions are being used instead.


#### MAP, FILTER, REDUCE

* MAP
The simplest use of map accepts a **unary function** and a sequence of
data elements. A unary function is a function that accepts **only one argument**. map
applies this function to each element of the sequence and returns a new sequence
that contains all the returned values:

``` clojure
(map inc [0 1 2 3])    ; unary function
;=> (1 2 3 4)

(map + [0 1 2 3] [0 1 2 3])  ; function that takes any number of arguments, multiple sequences
;=> (0 2 4 6)

(map + [0 1 2 3] [0 1 2])    ; length of return value is length of shortest sequence
```


* FILTER
filter does something similar to map —it collects values. But it accepts a predicate
function and a sequence and returns only those elements of the sequence that return
a logically true value when the predicate function is called on them.

``` clojure
(defn non-zero-expenses [expenses]
  (let [non-zero? (fn [e] (not (zero? e)))]    ; local function with logical negation
    (filter non-zero? expenses)))              ; apply non-zero predicate on expenses parameter
;=> #'user/non-zero-expenses

(non-zero-expenses [-2 -1 0 1 2 3])
;=> (-2 -1 1 2 3)
```

* REMOVE
Opposite of filter - filter uses predicate to decide what to keep, remove uses it to decide what to remove.

``` clojure
(defn non-zero-expenses [expenses]
  (remove zero? expenses))          ; predicate function zero?
;=> #'user/non-zero-expenses

(non-zero-expenses [-2 -1 0 1 2 3])
;=> (-2 -1 1 2 3)
```

* REDUCE
The simplest form of reduce is a high-level function that accepts a function of arity
two and a sequence of data elements. The function is applied to the first two elements
of the sequence, producing the first result. The same function is then called again
with this result and the next element of the sequence. This then repeats with the fol-
lowing element, until the last element is processed.

``` clojure
(defn factorial [n]
  (let [numbers (range 1 (+ n 1))]
    (reduce * numbers)))


;; range function list of numbers from first to second argument
(range 10)                     ; same as (range 1 10) first arg is inclusive
;=> (0 1 2 3 4 5 6 7 8 9)
```

---

* FOR

``` clojure
(for seq-exprs body-expr)
```

seq-exprs is a vector specifying one or more binding-form/collection-expr pairs.
body-expr can use the bindings set up in seq-exprs to construct each element of the
list.

``` clojure
(def chessboard-labels
  (for [alpha "abcdefgh"
        num (range 1 9)]
    (str alpha num)))
;=> ("a1" "a2" "a3" "a4" "a5" ... "h6" "h7" "h8")

(defn prime? [x]
  (let [divisors (range 2 (inc (int (Math/sqrt x))))
        remainders (map (fn [d] (rem x d)) divisors)]  ; rem - remainder of dividing numerator by denominator
    (not (some zero? remainders))))                    ; some - returns first logical true value when predicate is called on collection
    
;; FOR 
(defn primes-less-then [n]
  (for [x (range 2 (inc n))
        :when (prime? x)]    ; :when keyword option - condition
    x))
    
;; All pair of prime number
(defn pairs-of-primes [n]
  (let [z (range 2 (inc n))]
    (for [x z y z :when (prime? (+ x y))]
      (list x y))))
      
(pairs-for-primes 5)
;=> ((2 3) (2 5) (3 2) (3 4) (4 3) (5 2))
```


#### THREADING MACROS


#### THREAD FIRST  ->
Compound interest formula:
final-amount = principle * (1 + rate/100) ^ time-periods

(defn final-amount [principle rate time-periods]
  (* (Math/pow (+ 1 (/ rate 100)) time-periods) principle))   ; read from inside out

(final-amount 100 20 1)
;=> 120.0

;; Rewrite with usage of thread first macro for better readability

``` clojure
(defn final-amount-> [principle rate time-periods]
  (-> rate
      (/ 100)
      (+ 1)
      (Math/pow time-periods)
      (* principle)))
```

What the thread-first macro does is take the first argument supplied and place it in
the second position of the next expression. It’s called thread-first because it moves
code into the position of the first argument of the following form. It then takes the
entire resulting expression and moves it into the second position of the following
expression, and through all of them, until all expressions are exhausted.


#### THREAD LAST ->>
Instead of taking the first expression and moving it into the second position of the next expres-
sion, it moves it into the last place. It then repeats the process for all the expressions
provided to it.

``` clojure
(defn factorial [n]
  (reduce * (range 1 (+ 1 n))))
  
;; Thread last macro rewrite
(defn factorial->> [n]
  (->> n
       (+ 1)
       (range 1)
       (reduce *)))
```

#### THREAD AS as->
The threading macros you’ve seen so far don’t give you any control over the position of the
previous expression: it is either first or last, depending on which threading macro you
use. as-> is more flexible: you supply it a name, and it will bind the result of each suc-
cessive form to that name so you can use it in the next:

``` clojure
(as-> {"a" [1 2 3 4]} <>    ; <> name of form
      (<> "a")              ; can be used anywehre in next form
      (conj <> 10)          ; <> is [1 2 3 4] before this form executes and becomes [1 2 3 4 10]
      (map inc <>))         ; inc increments num by 1
;=> (2 3 4 5 11)

;; What upper example expands to:
(let [<> {"a" [1 2 3 4]}
      <> (<> "a")
      <> (conj <> 10)
      <> (map inc <>)]
<>)
```
The as-> macro is really just a more compact way of chaining a series of let bindings
to the same name.


#### CONDITIONAL THREADING cond->  cond->>
These threading macros are exactly like -> and ->> , except each
form is guarded by a conditional (which is not threaded) and can be skipped if the
conditional is false.

``` clojure
(let [x 1 y 2]
  (cond-> []
          (odd? x)          (conj "x is odd")            ; [] is threaded throught the right form
          (zero? (rem y 3)) (conj "y is divisible by 3") ; condition is false so this is skpped
          (even? y)         (conj "y is even")))
;=> ["x is odd" "y is even"]


;; What it expands to:
(let [x 1 y 2]
  (as-> [] <>
        (if (odd? x)          (conj <> "x is odd")            <>)
        (if (zero? (rem y 3)) (conj <> "y is divisible by 3") <>)
        (if (even? y)         (conj <> "y is even")           <>)))
;=> ["x is odd" "y is even"]
```


## CHAPTER 3


#### METADATA 
Metadata - data about data. Metadata is **always** a map. Metadata doesn't appear in REPL.

Clojure supports **tagging data** (for example, maps, lists, and vectors) with other data without 
changing the value of the tagged data. What this means specifically is that the *same values with 
different metadata will still compare equal*.

*The point of using immutable values instead of mutable objects is that you can eas-
ily compare values by their content instead of their identity.* The two vectors [1 2 3] and
[1 2 3] are the same even if they have different addresses in computer memory, so it
doesn’t matter which one your program uses. But in the real world you often need to
distinguish between otherwise identical things in meaningful ways. For example, one
value may compare equal to another, but it makes a difference if one value came from
an untrusted network source or a file with a specific name. Metadata provides a way to
add **identity** to values when it matters.

``` clojure
(def untrusted (with-meta {:command "delete-table" :subject "users"}
                         {:safe false :io true}))   ; attached metadata

;; Shorthand syntax ^
(def untrusted ^{:safe false :io true} {:command "delete-table"
                                        :subject "users"})
                         
```

You can also define metadata with a shorthand syntax using the reader macro ^{} .
This example is exactly the same as the previous one except the metadata is added at
read time instead of eval time.

* META
Function to examine metadata:

``` clojure
(meta untrusted)
;=> {:safe false :io true}
```

* COPY
When new values are created from those that have metadata, the **metadata is copied
over to the new data**. This is to preserve the identity semantics of metadata, for example:

``` clojure
(def still-untrusted (assoc untrusted :complete? false))
;=> #'user/still-untrusted

still-untrusted
;=> {:complete? false, :command "delete-table", :subject "users"}

(meta still-untrusted)
;=> {:safe false, :io true}
```

* FUNCTIONS / MACROS
Can also be defined with metadata:

``` clojure
(defn ^{:safe true :console true
        :doc "testing metadata for functions"}
  testing-meta
  []
  (println "Hello from meta!"))
  
  
;; Metadata is associated with var testing-meta NOT the function
(meta testing-meta)
;-> nil

;; This is how to get a meta from function
(meta (var testing-meta))
;=> {:ns #<Namespace user>,
    :name testing-meta,
    :file "NO_SOURCE_FILE",
    :line 1, :arglists ([]),
    :console true,
    :safe true,
    :doc "testing metadata for functions"}
```

Clojure internally uses metadata quite a lot; for example, the :doc key is used to
hold the documentation string for functions and macros, the :macro key is set to true
for functions that are macros, and the :file key is used to keep track of what source
file something was defined in.


#### JAVA TYPE HINTS ( ^symbol )
One of the pieces of metadata you may encounter often when making Java method
calls from Clojure is a Java type hint, which is stored in the meta key :tag . It’s used
often enough that it has its own reader macro syntax: ^symbol . Why do you need this?

When you make a Java method call using interop, the Java virtual machine ( JVM )
needs to know what class defines a method name so it can find the implementation of a
method in the class. In Java this is normally not a problem because most types are anno-
tated in the Java code and verified at compile time. Clojure is dynamically typed, how-
ever, so often the type of a variable isn’t known until runtime. In these cases the JVM
needs to use **reflection** to determine the class of an object at runtime and find the cor-
rect method to call. This works fine but can be slow.

``` clojure
(set! *warn-on-reflection* true)
;=> true

(defn string-length [x] (.length x))
;   Reflection warning, reference to field length can't be resolved.
;=> #'user/string-length

(time (reduce + (map string-length (repeat 10000 "12345"))))
"Elapsed time: 45.751 msecs"

(defn fast-string-length [^String x] (.length x))    ; Usage of type hint ^String
;=> #'user/fast-string-length                        ; No reflection warning

(time (reduce + (map fast-string-length (repeat 10000 "12345"))))
"Elapsed time: 5.788 msecs"

;; Getting meta data about type hint
(meta #'fast-string-length)
;=> {:ns #<Namespace user>, :name fast-string-length, :file "NO_SOURCE_FILE",
     :column 1, :line 1, :arglists ([x])}

(meta (first (first (:arglists (meta #'fast-string-length)))))   ; stored in :arglists as first argument
;=> {:tag String}

```

* IDIOMATIC WAY TO HANDLE TYPE HINTING

Clojure’s compiler is pretty smart about inferring types, and all core Clojure func-
tions are already type-hinted where necessary, so it’s not that often that you’ll need to
resort to type hints. The idiomatic approach is to write all your code without type
hints and then (set! *warn-on-reflection* true) and keep reevaluating your
namespace and adding hints one at a time until the reflection warnings go away. If
you concentrate type hinting on function arguments and return values, Clojure will
often figure out all the types in the function body for you.


#### JAVA PRIMITIVE AND ARRAY TYPES
Java has some special types called **primitives** that aren’t full-fledged objects and that get
special treatment by the JVM to increase speed and save memory (http://docs.oracle
.com/javase/tutorial/java/nutsandbolts/datatypes.html). They’re recognized by their
lowercase type names in Java documentation: byte , short , int , long , float , double ,
boolean , and char . These are sometimes called **unboxed types** because they don’t have
an object “box” around them or any object methods. Java arrays are fixed-length
homogenous containers for other types; they’re also primitive types, and there’s even a
different array type for each possible thing the array can hold!

Primitives don’t have a pronounceable class name to refer to them, so it’s not obvi-
ous how to type hint them. Fortunately, Clojure defines aliases for all the primitive
types and for all arrays of primitive types: just use a type hint like **^byte** for the primi-
tive and the plural form **^bytes** for the array-of-primitive.



#### JAVA EXCEPTIONS: TRY THROW


``` clojure
;; No exception handling
(defn average [numbers]
  (let [total (apply + numbers)]
    (/ total (count numbers))))
    
    
;; With exception handling
(defn safe-average [numbers]
  (let [total (apply + numbers)]
    (try
      (/ total (count numbers))
      (catch ArithmeticException e
        (println "Divided by zero!")
        0))))
        
        
;; Finally
(try
  (print "Attempting division... ")
  (/ 1 0)
  (catch RuntimeException e "Runtime exception!")     ; Exceptions are tested in order
  (catch ArithmeticException e "DIVIDE BY ZERO!")
  (catch Throwable e "Unknown exception encountered!")
  (finally                             ; Finally clause is ALWAYS executed
    (println "done.")))
    
;; Throw
(throw (Exception. "this is an error!"))
```

### FUNCTIONS
Functions are threated like first-class citizens:
  * Functions can be created dynamically (at runtime).
  * Functions can be accepted as arguments by other functions.
  * Functions can be returned from functions as return values.
  * Functions can be stored as elements inside other data structures (for example, lists).


#### DEFINING FUNCTIONS

Syntax of defn macro:

``` clojure
(defn function-name
  doc-string?
  metadata-map?
  [parameter-list*]
  conditions-map?
  body-expressions*)
```

Symbols with ? are optional.

There is no explicit *return* keyword in Clojure, instead function bodies have an implicit **do** block
surrounding them meaning that the value of the last expresion in the function body is returned.

``` clojure
(defn total-cost [item-cost number-of-items]
  (* item-cost number-of-items))
```

* DEFN
total-cost is what Clojure calls a var. Note also that the function is in turn created
using the fn macro. Because creating such vars and pointing them to a function is
common, the defn macro was included in the language as a convenience.

``` clojure
;; Previous defn expands to this
(def total-cost (fn [item-cost number-of-items]
                  (* item-cost number-of-items)))
```

* DOCSTRINGS

``` clojure
(defn total-cost
  "return line-item total of the item and quantity provided"
  [item-cost number-of-items]
  (* item-cost number-of-items))
  
;; Show docstring
(doc total-cost)
```

- METADATA-MAP
Dodatna funkcionalnost - jos jedan nacin da se doda metadata na var.
... 


* CONDITIONAL MAP
Optional form for every function.

At runtime, this function runs additional checks as specified by
the hash map with the two keys **:pre** and **:post** . The checks it runs before executing
the body of the function are the ones specified with the :pre key (hence called pre-
conditions). In this case, there are two checks: one that ensures that price is greater
than zero and a second that ensures that quantity is also greater than zero.

:post is check on return value of function. % - represents return value

``` clojure
(defn item-total [price quantity discount-percentage]
  {:pre [(> price 0) (> quantity 0)]
   :post [(> % 0)]}                       ; % return value
  (->> (/ discount-percentage 100)
       (- 1)
       (* price quantity)
       float))
    
;; Valid input
(item-total 100 2 0)
;=> 200.0

;; Valid input
(item-total 100 2 10)
;=> 180.0

;; Wrong input
(item-total 100 -2 10)
; AssertionError Assert failed: (> quantity 0) user/item-total
```

#### MULTIPLE ARITY
The **arity** of a function is the number of parameters it accepts. Clojure functions can
be **overloaded on arity**, meaning that you can execute a different function body
depending on the number of parameters the function was called with.


(defn function-name
  ;; Note that each argument+body pair is enclosed in a list.
  ([arg1]
  body-executed-for-one-argument-call)
  ([arg1 arg2] body-executed-for-two-argument-call)
  ;; More cases may follow.
)


``` clojure
(defn total-cost
  ([item-cost number-of-items]
    (* item-cost number-of-items))
  ([item-cost]
    (total-cost item-cost 1)))
```

#### VARIADIC FUNCTIONS
A **variadic function** is a function with an arity that takes a **variable number of
arguments**. Clojure uses &symbol for it:

(defn name-of-variadic-function [param-1 param-2 & rest-args]
  (body-of-function))

``` clojure
(defn total-all-numbers [& numbers]
  (apply + numbers))
```

Non-variadic arities are called **fixed arities**.

``` clojure
(defn many-arities
  ([]              0)
  ([a]             1)
  ([a b c]         3)
  ([a b c & more] "variadic"))
  
  
(many-arities)
;=> 0

(many-arities "one argument")
;=> 1

;; Error
(many-arities "two" "arguments")
ArityException Wrong number of args ...

(many-arities "three" "argu-" "ments")
;=> 3

(many-arities "many" "more" "argu-" "ments")
;=> "variadic"
```


#### RECURSIVE FUNCTIONS
Recursive functions are those that either **directly or indirectly call themselves**. Clojure
functions can certainly call themselves using their names, but this form of recursion
consumes the **stack**. If enough recursive calls are made, eventually the stack will **overflow**.

``` clojure
(defn count-down [n]
  (when-not (zero? n)
    (when (zero? (rem n 100))
      (println "count-down:" n))
    (count-down (dec n))))

(count-down 1000000)
;; ERROR stack overflow ...
```

* AVOIDING OVERFLOW

``` clojure
;; With recur to not cause overflow
(defn count-downr [n]
  (when-not (zero? n)
    (if (zero? (rem n 100))
      (println "count-down:" n))
    (recur (dec n))))
```
This now works for any argument, without blowing the stack. The change is minimal
because at the end of the function body, recur rebinds the function parameter n to
(dec n) , which then proceeds down the function body. When n finally becomes zero,
the recursion ends. As you can see, writing self-recursive functions is straightforward.



#### MUTUALLY RECURSIVE FUNCTIONS
*Mutually recursive functions are those that either directly or indirectly call each other.*

``` clojure
(declare hat)

(defn cat [n]
  (when-not (zero? n)
    (when (zero? (rem n 100))
      (println "cat:" n))
    (hat (dec n))))

(defn hat [n]
  (when-not (zero? n)
    (if (zero? (rem n 100))
      (println "hat:" n))
    (cat (dec n))))
```

This shows a contrived example of two functions, cat and hat , that call each other. Because cat
calls hat before hat is defined, you need to declare it first. When given a large enough
argument, they’ll throw the same StackOverflowError you saw earlier. Note that the
declare macro calls def on each of its arguments. This is useful in cases where a func-
tion wants to call another function that isn’t defined yet, as is the case with a pair of
mutually recursive functions.


* TRAMPOLINE
Let’s now fix this problem. You can’t use recur because recur is only useful for self-
recursion. Instead, you need to modify the code to use a special Clojure function
called trampoline .

``` clojure
(declare hatt)

(defn catt [n]
  (when-not (zero? n)
    (when (zero? (rem n 100))
      (println "catt:" n))
    (fn [] (hatt (dec n)))))   ; Using anonymous functions to fix muttually recurisve functions

(defn hatt [n]
  (when-not (zero? n)
    (when (zero? (rem n 100))
      (println "hatt:" n))
    (fn [] (catt (dec n)))))   ; Using anonymous functions to fix muttually recurisve functions
```

The difference is so minor that you could almost miss it. Consider the definition of
catt , where instead of making the recursive call to hatt , you now return an anony-
mous function that when called makes the call to hatt.

> Because these functions **no longer perform their recursion directly**, you have to
> use **another function** to call them. A function that accepts another function as an
> argument is called a **higher-order function**.

``` clojure
(trampoline catt 100000)
;=> ALL GOOD...
```

Internals of trampoline:
``` clojure
(defn trampoline
  ([f]
    (let [ret (f)]
      (if (fn? ret)
        (recur ret)     ; uses recur if its a function
        ret)))          ; otherwise return value
  ([f & args]
    (trampoline (fn [] (apply f args)))))
```
Notice that trampoline is a higher-order function that sets up a **local recursion**
point using the **let form**. It executes the function represented by the argument **f**
and calls **recur** whenever the return value is itself a **function**. You could have done
this yourself, but conveniently trampoline is available to you as part of the core set
of Clojure functions.


#### CALLING FUNCTIONS
There are no operators in LISPs, + is just a name of a function defined in CLojure itself.

``` clojure
(+ 1 2 3 4 5)
;=> 15

;; Apply function to sequence of data
(apply + list-of-numbers)
```

#### HIGHER-ORDER FUNCTIONS
As we discussed in the previous chapter, functions in Clojure are **first-class entities**.
Among other things, this means that functions can be treated **similarly to data**: they
can be *passed around as arguments and can be returned from functions.* Functions
that do these things are called **higher-order functions**.

Examples: map, reduce, filter, some, every? ...

Higher-order functions aren’t just convenient ways
of doing things such as processing lists of data but are also the core of a programming
technique known as **function composition**.


* EVERY

``` clojure
;; EVERY
;; Accepts a testing function that returns a bool (aka hpredicate functions)
;; and a sequence. Returns True if ALL pass predicate otherwize false.


(def bools [true true true false false])
;=> #'user/bools

(every? true? bools)
;=> false
```

* SOME

``` clojure
;; SOME
;; Accepts predicate and sequence. Calls predicate on each element in sequence
;; and returns FIRST logically true value it gets.

(some (fn [p] (= "rob" p)) ["kyle" "siva" "rob" "celeste"])
;=> true returns true
```


* CONSTANTLY

``` clojure
;; CONSTANTLY
;; Accepts value V and returns VARIADIC function that always returns the same value V
;; no matter the arguments.


(def two (constantly 2)) ; same as (def two (fn [& more] 2))
                         ; or      (defn two [& more] 2)
;=> #'user/two

(two 1)
;=> 2

(two :a :b :c)
;=> 2
```

two is a function that returns 2 , no matter what or how many arguments it’s called with.
constantly is useful when a function requires another function but you just want a
constant value.

* COMPLEMENT

``` clojure
;; COMPLEMENT
;; Accepts function and returns a new one that takes the same number of arguments,
;; does the same things as original function, but returns logically oposite value


(defn greater? [x y]
(> x y))

(greater? 10 5)
;=> true

(greater? 10 20)
;=> false


;; Complement in action
(def smaller? (complement greater?))

(smaller? 10 5)
;=> false

(smaller? 10 20)
;=> true
```

* COMP

``` clojure
;; COMP
;; Short for composition.
;; Accepts multiple functions and returns a new function thats a composition of those funcitons.
;; 
;; The computation
;; goes from **RIGHT TO LEFT**—that is, the new function applies its arguments to the right-
;; most of the original constituent functions, then applies the result to the one left of it,
;; and so on, until all functions have been called

(def opp-zero-str (comp str not zero?))

(opp-zero-str 0)
;=> "false"

(opp-zero-str 1)
;=> "true"
```
Here, opp-zero-str when called with 1 first applies the function zero? to it, which
returns false ; it then applies not , which returns true , and then applies str , which con-
verts it to a string "true"


* PARTIAL
Short for PARTIAL COMPOSITION.
Accepts function F and few arguments to F but FEWER then the number F normaly takes.
Partial then returns a NEW function that accepts the REMAINING arguments to F.

``` clojure
(defn above-threshold? [threshold number]
  (> number threshold))


;; To use it to filter a list, you might do this:
(filter (fn [x] (above-threshold? 5 x)) [ 1 2 3 4 5 6 7 8 9])
;=> (6 7 8 9)


;; With partial , you could generate a new function and use that instead:
(filter (partial above-threshold? 5) [ 1 2 3 4 5 6 7 8 9])
;=> (6 7 8 9)
```
The idea behind partial is to adapt functions that accept n arguments to situations
where you need a function of fewer arguments, say n-k , and where the first k argu-
ments can be fixed.


* MEMOIZE
Memoization is a technique that prevents functions from computing results for argu-
ments that have already been processed. Instead, return values are looked up from a
cache

``` clojure
(defn slow-calc [n m]
  (Thread/sleep 1000)
  (* n m))


(time (slow-calc 5 7))
"Elapsed time: 1000.097 msecs"
;=> 35


(def fast-calc (memoize slow-calc))

;; For memoize to do its thing, you call fast-calc once with a set of arguments (say 5
;; and 7 ). You’ll notice that this run appears as slow as before, only this time the result
;; has been cached. Now, you call it once more via a call to time :
(time (fast-calc 5 7))
"Elapsed time: 0.035 msecs"
;=> 35
```

But there’s a big caveat to memoize : the cache that backs memoize doesn’t have a
bounded size and caches input and results forever. Therefore, memoize should only be
used with functions with a small number of possible inputs or else you’ll eventually
run out of memory


#### WRITING  HIGHER-ORDER FUNCTIONS

``` clojure
(def users
  [{:username     "kyle"
    :firstname    "Kyle"
    :lastname     "Smith"
    :balance      175.00M      ; Use BigDecimals for money!
    :member-since "2009-04-16"}
  {:username      "zak"
   :firstname     "Zackary"
   :lastname      "Jones"
   :balance       12.95M
   :member-since  "2009-02-01"}
  {:username     "rob"
   :firstname    "Robert"
   :lastname     "Jones"
   :balance      98.50M
   :member-since "2009-03-30"}])

(defn sorter-using [ordering-fn]
  (fn [collection]
    (sort-by ordering-fn collection)))

(defn lastname-firstname [user]
  [(user :lastname) (user :firstname)])

(defn balance [user] (user :balance))

(defn username [user] (user :username))

(def poorest-first (sorter-using balance))

(def alphabetically (sorter-using username))

(def last-then-firstname (sorter-using lastname-firstname))
```

- users = vector of hash-maps

* SORT-BY
it’s a higher-order function the first argument
of which is a key function. The key function must accept one of the items you’re sort-
ing and return a key to sort it by.

``` clojure
(map username users)
;=> ("kyle" "zak" "rob")

(sort *1)                  ; *1 last result from REPL
;=> ("kyle" "rob" "zak")

;; This is the list that sort-by will see when sorting items. sort-by will sort the items as if it
;; were instead sorting a list created by (map key-function items)

(sort-by username users)
;=> ({:member-since "2009-04-16", :username "kyle", ...}
     {:member-since "2009-03-30", :username "rob", ...}
     {:member-since "2009-02-01", :username "zak", ...})
```

* STEP FURTHER WITH SORTINTG
But what if you want to create functions that always sort in a specific order, without
having to specify an ordering function? That’s what the sorter-using function does.

``` clojure
(defn sorter-using [ordering-fn]
  (fn [collection]
    (sort-by ordering-fn collection)))
```

It accepts a key function called ordering-fn and returns a function that accepts a col-
lection that it will always sort using sort-by and the original ordering-fn.

``` clojure
(def poorest-first (sorter-using balance))

;; This is the same as

(defn poorest-first [users] (sort-by balance users))


;; Now acctually call the function:
(poorest-first users)
;=> ({:username "zak", :balance 12.95M, ...}
     {:username "rob", :balance 98.50M, ...}
     {:username "kyle", :balance 175.00M, ...})
```

* DOUBLE SORTING
But suppose you wanted to sort by two criteria: first by each user’s last name and then
by first name if they share a first name with another user.

You can do this by supplying an ordered collection as the sorting key for an item: 
sequences are sorted by comparing each of their members in order. 

For example, the lastname-firstname function returns a vector of the last then first name of a user:
``` clojure
(map lastname-firstname users)
;=> (["Smith" "Kyle"] ["Jones" "Zackary"] ["Jones" "Robert"])

(sort *1)
;=> (["Jones" "Robert"] ["Jones" "Zackary"] ["Smith" "Kyle"])

;; So you can use this to sort the full user records with the last-then-firstname function:
(last-then-firstname users)
```


#### ANONYMOUS FUNCTIONS

``` clojure
(def total-cost
  (fn [item-cost number-of-items]
    (* item-cost number-of-items)))
```

As we discussed earlier, this code assigns a value to the **total-cost var**, which is the func-
tion created by the **fn** macro. To be more specific, the *function by itself doesn’t have a
name*; instead, you use the **var** with the *name total-cost to refer to the function*. The
function itself is **anonymous**. To sum up, anonymous functions can be created using the **fn form**.


``` clojure
(map (fn [user] (user :member-since)) users)
;=> ("2009-04-16" "2009-02-01" "2009-03-30")
```

#### SHORTCUT FOR ANONYMOUS FUNCTIONS #()

``` clojure
(map #(% :member-since) users)
;=> ("2009-04-16" "2009-02-01" "2009-03-30")
```

The **#()** , with the body of the anonymous function appearing within the parenthe-
ses, creates an anonymous function. The **%** symbol represents a single argument. If
the function needs to accept more than one argument, then %1 , %2 , ... can be used.
%& for rest of the arguments.

``` clojure
(#(vector %&) 1 2 3 4 5)
;=> [(1 2 3 4 5)]

(#(vector % %&) 1 2 3 4 5)
;=> [1 (2 3 4 5)]

(#(vector %1 %2 %&) 1 2 3 4 5)
;=> [1 2 (3 4 5)]

(#(vector %1 %2 %&) 1 2)
;=> [1 2 nil]
```


#### KEYWORDS AND SYMBOLS

``` clojure
(def person {:username "zak"
             :balance 12.95
             :member-since "2009-02-01"})
             
;; Hash map
(person :username)
;=> "zak"

;; Keywords behave as functions as well
(:username person)
;=> "zak"
```

``` clojure
(map #(% :member-since) users)
;=> ("2009-04-16" "2009-02-01" "2009-03-30")


;; Simpler rewrite that uses keyword as function 
(map :member-since users)
;=> ("2009-04-16" "2009-02-01" "2009-03-30")


(:login person)
;=> nil

(:login person :not-found)
;=> :not-found
```

Idiomatic way of working with maps in these situations.


#### SYMBOLS
*A symbol is a name as a value.* To use the analogy of a dictionary,
the *word* in a dictionary entry is the symbol but the *definition* of the word is a binding of
that word to a particular meaning. A *word and its definition aren’t the same* — for
example, a word could have multiple definitions, or it could have a different defini-
tion at a different time. The same principle applies to symbols: the symbol user is
always the same as another symbol user , but they could point to (that is, be bound to)
different values.

* SYMBOL AS A VALUE - QUOTING
Normally when the Clojure runtime sees a symbol like users , it automatically eval-
uates it and uses the value that the symbol represents. But you may wish to use symbols
as is. You may desire to use symbols themselves as values, for instance, as keys in a map
(or indeed in some kind of symbolic computation). To do this, you’d quote the sym-
bols.

``` clojure
(def expense {'name "Snow Leopard" 'cost 29.95M})
;=> #'user/expense

(expense 'name)
;=> "Snow Leopard"

('name expense)
;=> "Snow Leopard"

('vendor expense)
;=> nil

('vendor expense :absent)
;=> :absent
```

**Various examples where it shows that  vectors, hashmaps are also functions**

The fact that vectors and hash maps are functions is useful when code is designed
with function composition in mind. Instead of writing wrapper functions, these data
structures can themselves be passed around as functions. This results in cleaner,
shorter code.


#### SCOPE
**Scope, as it’s generally known, is the enclosing context where names resolve to associated values.**

Clojure has 2 scopes: 
1. Static (or lexical)
2. Dynamic

##### LEXICAL SCOPE
A lexically scoped variable is visible only inside the textual block that it’s defined in 
(*justifying the term lexical*) and can be determined at compile time (*justifying the term static*).


#### VARS AND BINDING
Vars is similar to **global** in other languages. Vars are defined
at the top level of any namespace, using the def special form.

``` clojure
(def MAX-CONNECTIONS 10)
```
Remember, def always creates the var at the level of the enclosing namespace no mat-
ter where it’s called. For instance, even if you call def from inside a function, it will
create the var at the namespace level.

##### BINDING
The value of a var is determined by its *binding*. In this example, MAX-CONNECTION is bound to the 
number 10, and such an initial binding is called a **root binding**. A var can be defined without
any initial binding at all, in the following form. Here RABBITMQ-CONNECTION is said to be unbound.

``` clojure
(def RABBITMQ-CONNECTION)
```

If another part of the code tries to
use its value, an exception will be thrown saying that the var is unbound. To set a value
for an unbound var, or to change the value bound to a var, Clojure provides the binding
form. Unfortunately, as defined previously, calling binding will throw an exception
complaining that you can’t dynamically bind a non-dynamic var. To rebind vars, they
need to be dynamic, which is done using the following metadata declaration:

``` clojure
(def ^:dynamic RABBITMQ-CONNECTION)      ; ^:dynamic ***

(binding [RABBITMQ-CONNECTION (new-connection)]
    (
      ;; do something here with RABBITMQ-CONNECTION
    ))
```

The general structure of the binding form is that it begins with the symbol binding ,
followed by a vector of an even number of expressions. The first of every pair in the
vector is a var, and it gets bound to the value of the expression specified by the second
element of the pair. Binding forms can be nested, which allows new bindings to be
created within each other.

By the way, if you do try to rebind a var that wasn’t declared dynamic, you’ll see this
exception:
;; java.lang.IllegalStateException:
;; Can't dynamically bind non-dynamic var: user/RABBITMQ-CONNECTION


##### SPECIAL VARIABLES
There’s one thing to note about vars: when declared with the ^:dynamic metadata,
they become dynamically scoped.

``` clojure
(def ^:dynamic *db-host* "localhost")
```

If you now call a function like expense-report , which internally uses *db-host* to
connect to a database, you’ll see numbers retrieved from the local database. For now,
test this with a function that prints the binding to the console:

``` clojure
(defn expense-report [start-date end-date]
  (println *db-host*)) ;; can do real work
```

Now, once you’ve tested things to your satisfaction, you can have the same code con-
nect to the production database by setting up an appropriate binding:

``` clojure
(binding [*db-host* "production"]
  (expense-report "2010-01-01" "2010-01-07"))
```

Note here that you managed to change what the expense-report function does,
without changing the parameters passed to it (the function connects to a database
specified by the binding of the *db-host* var). This is called **action at a distance**, and it
must be done with **caution**. The reason is that it can be similar to programming with
global variables that can change out from underneath you. But used with caution, it
can be a convenient way to alter the behavior of a function.

Such vars that need to be bound appropriately before use are called special vari-
ables. A naming convention is used to make this intent clearer: these var names begin
and end with an asterisk. *db-host* example


#### DYNAMIC SCOPE
We’ll now explore the earlier statement that vars aren’t governed by lex-
ical scoping rules. We’ll implement a simple form of **aspect-oriented programming**,
specifically a way to add a log statement to functions when they’re called. You’ll see
that in Clojure this ends up being quite straightforward, thanks to dynamic scope.

* LEXICAL
Lexical scope rules are simple to understand; you
can tell the visibility of all lexically scoped variables by looking at the program text
(hence the term lexical)

* DYNAMIC
Dynamic scope doesn’t depend on the lexical structure of code; instead, the value
of a var depends on the execution path taken by the program.

If a function rebinds a var using a binding form, then the value of the var is changed for all code that exe-
cutes within that binding form, including other functions that may be called. This
works in a nested manner, too. If a function were to then use another binding form
later on in the call stack, then from that point on all code would see this second value
of the var. When the second binding form completes (execution exits), the previous
binding takes over again, for all code that executes from that point onward.

``` clojure
(def ^:dynamic *eval-me* 10)

(defn print-the-var [label]
  (println label *eval-me*))

(print-the-var "A:")

(binding [*eval-me* 20]     ;; the first binding
  (print-the-var "B:")
  (binding [*eval-me* 30]   ;; the second binding
    (print-the-var "C:"))
  (print-the-var "D:"))
(print-the-var "E:")

;; RESULTS
A: 10
B: 20
C: 30
D: 20
E: 10
```

``` clojure
(defn ^:dynamic twice [x]
  (println "original function")
  (* 2 x))

(defn call-twice [y]
  (twice y))

(defn with-log [function-to-call log-statement]
  (fn [& args]
    (println log-statement)
    (apply function-to-call args)))

(call-twice 10)

(binding [twice (with-log twice "Calling the twice function")]
  (call-twice 20))

(call-twice 30)

;; ------------------------
original function
20
Calling the twice function
original function
40
original function
60

```



## CHAPTER 4

#### POLYMORPHISM

Polymorphism is the ability to use multiple types as though they were the same—
that is, you can write the same code to operate on many different types. This kind
of abstraction allows you to substitute different types or implementations without
having to change all code that touches objects of those types.

There are multiple ways to achieve polymorphism, but three are common to many languages: 
1. parametric 
2. ad hoc
3. subtype polymorphism.


#### Parametric Polymorphism

You’ve actually already come in contact with polymorphism in Clojure. As you saw in
chapter 2, functions such as *get , conj , assoc , map , into , reduce* , and so on accept
many different types in their arguments but always do the correct thing. Clojure col-
lections are also polymorphic because they can hold items of any type. This kind of
polymorphism is called **parametric polymorphism** because such code mentions only
*parameters and not types*. It’s common in dynamically typed languages because such lan-
guages by their nature don’t often mention types explicitly. But it’s also present in
some statically typed programming languages, both object-oriented languages such as
Java and C# (where it’s called generics) and functional programming ( OO ) languages
such as ML and Haskell.


This kind of polymorphism is usually invisible in Clojure: you just use the built-in
function and collection types, and the Clojure runtime works out what should happen.


#### AD HOC Polymorphism
Ad hoc polymorphism is simply enumerating each possible type a function can use and
writing an implementation for each one.

``` clojure
(defn ad-hoc-type-namer [thing]
  (condp = (type thing)                         ; check type
    java.lang.String              "string"
    clojure.lang.PersistentVector "vector"))

(ad-hoc-type-namer "I'm a string")
;=> "string"

(ad-hoc-type-name [])
;=> "vector"

(ad-hoc-type-namer {})
;=> Error
```

> AD HOC POLYMORPHISM AS “FUNCTION OVERLOADING”
>
> Other languages often call ad hoc polymorphism function overloading and have some
> special syntax to support this type of polymorphism. For example, in Java you can
> repeat the method but annotate the argument’s types differently; the Java virtual
> machine (JVM) will then perform the dispatch to the right method invisibly at compile
> time. Here’s a short example equivalent to the ad-hoc-type-namer Clojure function:
``` java 
public class TypeNamer extends Object {
 // ...
    public String typeName(String thing) { return "string"; }
    public String typeName(PersistentVector thing) {
      return "vector";
    }
 }
```


* CLOSED DISPATCH
Notice that this example of ad hoc polymorphism doesn’t allow calling code to “train”
the ad-hoc-type-namer function to understand new types—you can’t add new clauses
to the condp expression without rewriting the function. This property is called closed
dispatch because the list of available implementations (that is, to which you can dis-
patch) can’t be changed from the outside. But you can implement open dispatch by
keeping the implementations outside your type-naming function:

``` clojure
;; OPEN DISPATCH Example

(def type-namer-implementations
  {java.lang.String              (fn [thing] "string")
   clojure.lang.PersistentVector (fn [thing] "vector")})

(defn open-ad-hoc-type-namer [thing]
  (let [dispatch-value (type thing)]
    (if-let [implementation
             (get type-namer-implementations dispatch-value)]
     (implementation thing)
     (throw (IllegalArgumentException.
              (str "No implementation found for " dispatch-value))))))
              
              
(open-ad-hoc-type-namer "I'm a string")
;=> "string"

(open-ad-hoc-type-namer [])
;=> "vector"

(open-ad-hoc-type-namer {})
;=> ERROR


;; Redefine hashmap implementation
(def type-namer-implementations
  (assoc type-namer-implementations
    clojure.lang.PersistentArrayMap (fn [thing] "map")))
    
(open-ad-hoc-type-namer {});
;=> "map"
```


#### SUBTYPE POLYMORPHISM
So far we’ve been concentrating on the functions that are polymorphic, but the types
can also be polymorphic. Subtype polymorphism is a kind of polymorphism where one
type says it can be substituted for another so that any function that can use one type
can safely use the other.

*Subtype polymorphism is the dominant kind of polymorphism in OO languages
and it’s expressed as class or interface hierarchies.* For example, if a Person class
inherits from (or extends) an Animal class, then any method that works with Animal
objects should automatically work with Person objects too. Some dynamic OO lan-
guages also allow a form of **subtype polymorphism** (called **structural subtyping**) that
doesn’t use an explicit hierarchy or inheritance. Instead methods are designed to
work with any objects that have the necessary structure, such as *properties* or *methods*
with the correct names. Python, Ruby, and JavaScript all allow this form of subtyping,
which they call **duck typing**.

The reason is that subtyping, while powerful because it allows you to write fewer implementations
of functions, can also be restraining if applied too broadly because there’s often no
one, single, universally applicable arrangement of types.

More importantly, Clojure is focused on data and values, not types: the
programming-language type used to contain some information (such as a map, list, or
vector) is independent of the problem-domain type (such as animal, vegetable, or min-
eral), and it’s the latter that should be the focus.

Multimethods provide features to express both ad hoc and subtype polymorphism
and even to express multiple different kinds of subtype polymorphism at the same
time.


#### MULTIMETHODS
Consider the situation where the expense-tracking service you’ve written has become
popular. You’ve started an affiliate program where you pay referrers if they get users to
sign up for your service. Different affiliates have different fees. Let’s begin with the
case where you have two affiliates: mint.com and google.com.

You’d like to create a function that calculates the fee you pay to the affiliate. For the
sake of illustration, let’s decide you’ll pay your affiliates a percentage of the annual sal-
ary the user makes. You’ll pay Google 0.01%, Mint 0.03%, and everyone else 0.02%.
You’ll write this without polymorphism first (you’ll accept percentage values as straight
numbers and you’ll translate them by multiplying by 0.01 within the function):

``` clojure
(def example-user {:login "rob" :referrer "mint.com" :salary 100000})
;=> #'user/example-user

(defn fee-amount [percentage user]
  (with-precision 16 :rounding HALF_EVEN
    (* 0.01M percentage (:salary user))))         ; BigDecimal
;=> #'user/fee-amount

(defn affiliate-fee [user]
  (case (:referrer user)
    "google.com" (fee-amount 0.01M user)
    "mint.com" (fee-amount 0.03M user)
    (fee-amount 0.02M user)))
;=> #'user/affiliate-fee

(affiliate-fee example-user)
;=> 30.0000M
```
You should be able to recognize affiliate-fee as an example of **closed dispatch**, *ad
hoc polymorphism*, which uses :referrer as a dispatch function. It also has a default
implementation to use if no match is found.

The biggest problem with the affiliate-fee function is the closed dispatch: you
can’t add new affiliates without rewriting affiliate-fee . Now let’s look at how you’d
solve this problem with multimethods.


##### AD-HOC POLYMORPHISM USING MULTIMETHODS
defmulti macro defines a multimethod.
defmethod defines an implementation for specific dispatch value.
It behaves similar to **case**.

``` clojure
(defmulti affilicate-fee (fn [user] (:referrer user)))    ; :referrer

(defmethod affiliate-fee "mint.com" [user]                ; :refferer user = "mint.com"
  (fee-amount 0.03M user))
  
(defmethod affilicate-fee "google.com" [user]
  (free-amount 0.01M user))
  
(defmethod affiliate-fee :default [user]
  (fee-amount 0.02M user))
  
(affiliate-fee example-user)
;=> 30.0000M
```

* DEFMULTI

``` clojure
(defmulti name docstring? attr-map? dispatch-fn & options)
```
- name = name of multimethod
- docstring? attr-map? optional documentation and metadata arguments
- dispatch-fn = regular clojure function that accepts the same arguments that are passed in when multimethod is called
                The return value of dispatch-fn is used to select implementation
- options = key-value pairs to provide optinal specifications. There are only 2 options :default - for dispatch value, 
            :hierarchy - to use custom dispatch value hierarchy
            
            
``` clojure
(defmulti affiliate-fee :referrer :default "*")  ; it returns nil means it wasnt redefined
;=> nil

(ns-unmap 'user 'affiliate-fee)

(defmulti affiliate-fee :referrer :default "*")  ; option :default "*"

(defmethod affiliate-fee "*" [user]              ; now the default case
  (fee-amount 0.02M user))

(affiliate-fee example-user)                     ; default case "mint.com" is gone because of redefined affiliate-fee
;=> 20.0000M
```
First, notice that you can use a plain keyword :referrer
as a *dispatch function*: *this is a common idiom where the argument to the defmethod
is always a map and you want to dispatch by the value of one of its keys.* Second, because
you had to redefine the defmulti , all the existing defmethod s were lost: see the follow-
ing sidebar for more discussion of the issues surrounding redefining a defmulti . We’ll
talk about the :hierarchy option later: it’s used for subtype polymorphism.

> REDEFINING A DEFMULTI
> A big gotcha in Clojure is that the defmulti macro uses defonce to define the var
> that holds the multimethod. This means that if you try to change a multimethod’s dis-
> patch function or options, your reasserted defmulti will have no effect and you’ll
> continue to use the old multimethod. This rarely happens in normal running code but
> can cause considerable hair pulling at the read-evaluate-print loop (REPL) where
> exploratory change and revision is common.
>
> A redefined defmulti will return nil instead of a var like #'user/mymulti. If you
> notice the nil (or if you don’t but your multimethod feels like it’s not accepting
> your changes), use (ns-unmap 'namespace 'defmultiname) before reasserting the
> defmulti form as demonstrated in the previous code example.
>
> Another related gotcha is that a redef-ed multimethod will lose all of its defmethod
> implementations. This is because multimethods are a rare example of mutation in
> Clojure: each defmethod is actually mutating the original object created by defmulti
> by adding new items to its dispatch table. (This is why defmulti uses defonce in
> the first place: so mutations to its dispatch table aren’t lost by accidental redefini-
> tion.) By creating a new multimethod all the old methods were cleared out. You can
> see and manipulate this dispatch table using methods and remove-method, which
> we’ll discuss shortly.


* DEFMETHOD

``` clojure
(defmethod multifn dispatch-value & fn-tail)
```
This creates a concrete implementation for a previously defined multimethod. The
multifn identifier should match the name in the previous call to defmulti . The
dispatch-value will be compared with the return value of the dispatch-fn from ear-
lier to determine which method will execute. The fn-tail is the body of the imple-
mentation and accepts anything you’d put inside a (fn ...) form, including argument
destructuring


(defmethod my-multi :default [arg] "body")
(defmethod my-many-arity-multi :default
  ([] "no arguments")
  ([x] "one argument")
  ([x & etc] "many arguments"))


#### MULTIPLE DISPATCH
Upgradeing the system:

``` clojure
(def user-1 {:login "rob" :referrer "mint.com"    :salary 100000
             :rating :rating/bronze})

(def user-2 {:login "gordon" :referrer "mint.com" :salary 80000
             :rating :rating/silver})

(def user-3 {:login "kyle" :referrer "google.com" :salary 90000
             :rating :rating/gold})

(def user-4 {:login "celeste" :referrer "yahoo.com" :salary 70000
             :rating :rating/platinum})
```

Affiliate   |  Profit rating  |  Fee (% of salary)
mint.com    |   Bronze        |   0.03
mint.com    |   Silver        |   0.04
mint.com    |   Gold/platinum |   0.05
google.com  |   Gold/platinum |   0.03


``` clojure
(defn fee-category [user]
  [(:referrer user) (:rating user)])

(map fee-category [user-1 user-2 user-3 user-4])
;=> (["mint.com" :rating/bronze]
    ["mint.com" :rating/silver]
    ["google.com" :rating/gold]
    ["yahoo.com" :rating/platinum])
    
    
;; Multiple Dispatch Multimethod
(defmulti profit-based-affiliate-fee fee-category)    ; fee-category function is udes to create dispatch values

(defmethod profit-based-affiliate-fee ["mint.com" :rating/bronze]
  [user] (fee-amount 0.03M user))

(defmethod profit-based-affiliate-fee ["mint.com" :rating/silver]
  [user] (fee-amount 0.04M user))

(defmethod profit-based-affiliate-fee ["mint.com" :rating/gold]
  [user] (fee-amount 0.05M user))

(defmethod profit-based-affiliate-fee ["mint.com" :rating/platinum]
  [user] (fee-amount 0.05M user))

(defmethod profit-based-affiliate-fee ["google.com" :rating/gold]
  [user] (fee-amount 0.03M user))

(defmethod profit-based-affiliate-fee ["google.com" :rating/platinum]
  [user] (fee-amount 0.03M user))

(defmethod profit-based-affiliate-fee :default  ; even with multiple dispatch d:default value is always single not :default :default
  [user] (fee-amount 0.02M user))
```

``` clojure
;; Example in practise with map
(map profit-based-affiliate-fee [user-1 user-2 user-3 user-4])
;=> (30.0000M
    32.0000M
    27.0000M
    14.0000M)
```

But notice that you did have to duplicate some code: the business rules treat gold and platinum profit
ratings the same, but you still had to write a separate method (with the same imple-
mentation) for each. Duplicate defmethod s that differ only by dispatch value are a
strong hint that those dispatch values are the same kind of thing. This isn’t something
ad hoc polymorphism can help you with, but subtype polymorphism is great for
removing redundant implementations.


#### SUBTYPE POLYMORPHISM USING MULTIMETHODS


##### DERIVE
The derive function is used to establish a “kind of” or subtype relationship between
two types. Multimethods represent types as keywords or Java classes.

The ordinary form of derive takes two arguments: a keyword representing a type
and another keyword representing the parent. (You can remember this argument
order by thinking “derive x from y,” “x is a kind of y,” or “x is a child of y.”) In the two-
argument form, derive will mutate a global hierarchy to establish the type relation-
ship. Because you’re mutating a global hierarchy, derive requires that your keywords
have a namespace to reduce the chance of name collisions.

``` clojure
(derive :rating/bronze    :rating/basic)
(derive :rating/silver    :rating/basic)
(derive :rating/gold      :rating/premier)
(derive :rating/platinum  :rating/premier)
(derive :rating/basic     :rating/ANY)     ; :rating/ANY root type for all ratings
(derive :rating/premier   :rating/ANY)
```

##### ISA?
Now that you’ve created your hierarchy, how do you see it? The most important
function is isa? : this is the function multimethods use internally to determine what
method to select for a dispatch value. You can also use parents , ancestors , and
descendants to inspect the hierarchy more directly.

``` clojure
(isa? :rating/gold :rating/premier)
;=> true

(isa? :rating/gold :rating/ANY)    ; understands *transitive* relationships
;=> true

(isa? :rating/ANY :rating/premier)
;=> false

(isa? :rating/gold :rating/gold)  ; types are always kinds of themselves
;=> true

(parents :rating/premier)        ; parents returns a *set*
;=> #{:rating/ANY}

(ancestors :rating/gold)         ; ancestors returns parents and their parents
;=> #{:rating/ANY :rating/premier}

(descendants :rating/ANY)        ; descendants returns children and thier children
;=> #{:rating/basic :rating/bronze :rating/gold :rating/premier :rating/
      silver :rating/platinum}
```

See it in practise:

``` clojure
(defmulti greet-user :rating)

(defmethod greet-user :rating/basic [user]
  (str "Hello " (:login user) \.))

(defmethod greet-user :rating/premier [user]
  (str "Welcome, " (:login user) ", valued affiliate member!"))

(map greet-user [user-1 user-2 user-3 user-4])
;=> ("Hello rob." "Hello gordon." "Welcome, kyle, valued affiliate member!"
      "Welcome, celeste, valued affiliate member!")
```


#### SUBTYPES AND MULTIPLE-DISPATCH
Multimethods let you combine multiple-dispatch and type hierarchies. If you return a
vector from your dispatch function, the multimethod will consider each item in the
vector separately with isa? when trying to find a matching implementation. So you
can finally clean up your profit-based-affiliate-fee code to remove those annoy-
ing duplicates:

``` clojure
(remove-method profit-based-affiliate-fee ["mint.com"   :rating/gold])
(remove-method profit-based-affiliate-fee ["mint.com"   :rating/platinum])
(remove-method profit-based-affiliate-fee ["google.com" :rating/gold])
(remove-method profit-based-affiliate-fee ["google.com" :rating/platinum])

(defmethod profit-based-affiliate-fee ["mint.com" :rating/premier]
  [user] (fee-amount 0.05M user))

(defmethod profit-based-affiliate-fee ["google.com" :rating/premier]
  [user] (fee-amount 0.03M user))

(map profit-based-affiliate-fee [user-1 user-2 user-3 user-4])
```

> MULTIPLE-DISPATCH AND :DEFAULT
>
> A subtlety of the :default case is that it’s used only when an entire dispatch fails;
> it’s not substituted for individual values of a multiple-dispatch vector. The reason is
> that (isa? x :default) is never true for any x (except :default itself), so you can’t
> specify a dispatch value like ["mint.com" :default] and expect it to match when
> no other more-specific rating matches. Instead, you must explicitly link the value to
> some base type, as you did with :rating/ANY, and use it as your fallback case in
> multiple dispatch.
>
> This also means that you can’t specify a default case for the :referrer slot (for
> example, "mint.com") because you’re using strings. You need to call (derive
> "mint.com" :referrer/ANY) to create a default case but derive only works with key-
> words, symbols, and classes, not strings. A workaround is either to create a keyword
> dynamically in the dispatch function (for example, (keyword "site" (:referrer
> user))) and match on that instead or to have the :default implementation invoke
> (get-method profit-based-affiliate-fee [:site/ANY (:rating user)]) to
> force a specific dispatch value and call the matching method it returns.


#### RESOLVING METHOD AMBIGUITIES

There’s a downside to using subtyping with multiple dispatch: it introduces the possi-
bility of ambiguity. Example:


``` clojure
(defmulti size-up (fn [observer observed]
  [(:rating observer) (:rating observed)]))

(defmethod size-up [:rating/platinum :rating/ANY] [_ observed]
  (str (:login observed) " seems scrawny."))

(defmethod size-up [:rating/ANY :rating/platinum] [_ observed]    ; Platinum user should look imposing to everyone
  (str (:login observed) " shimmers with an unearthly light."))

(size-up {:rating :rating/platinum} user-4)       ; What happens when platinum user looks at another platinum user?
;=> ERROR!
```

The problem here is that the type system, combined with multiple dispatch, intro-
duces an ambiguity about which method implementation should be used: Do platinum
users look scrawny to other platinum users, or do they shimmer with an unearthly
light like themselves?


* DUPLICATION
Suppose you decide that platinum users should shimmer to other platinum users,
not look scrawny. There are a few ways you could remove the ambiguity. First, you
could avoid the use of :rating/ANY in the scrawny defmethod and instead enumerate
every rating that isn’t platinum. But this means you’ve duplicated code again: you
need a defmethod for at least :rating/basic and :rating/gold , and they both will
have the same body. Further, you need to remember to add more cases if more profit
ratings are added in the future. A second possibility is to add an explicit [:rating/
platinum :rating/platinum] method, but this also means you’ll need to duplicate
the “shimmers” code.

* PREFER-METHOD
This function takes the multimethod and
a pair of dispatch values and instructs the multimethod to prefer the first dispatch
value over the second when there’s an ambiguity between the two.

``` clojure
(prefer-method size-up [:rating/ANY :rating/platinum]
  [:rating/platinum :rating/ANY])

(size-up {:rating :rating/platinum} user-4)

(prefers size-up)
```


#### USER-DEFINED HIERARCHIES
All the multimethods we’ve defined and the hierarchies you’ve derived so far have
been inspecting and mutating a **single, program-wide global hierarchy**.

Clojure’s multimethods also allow you to create your own blank hierar-
chy and use it explicitly instead of using the invisible global hierarchy.

* MAKE-HIERARCHY

``` clojure
(def myhier (make-hierarchy))

myhier                        ; Hierarhy are just ordinary maps
;=> {:parents {}, :descendants {}, :ancestors {}}

(derive myhier :a :letter)            ; 3 arg derive doens't return nil
;=> {:parents {:a #{:letter}}, :ancestors {:a #{:letter}},
     :descendants {:letter #{:a}}}

myhier                    ;  unlike 2-arg derive it doesnt mutate
;=> {:parents {}, :descendants {}, :ancestors {}}

(def myhier (-> myhier
                (derive :a :letter)      ; keywords without namesapces
                (derive :b :letter)
                (derive :c :letter)))
;=> #'user/myhier

(isa? myhier :a :letter)
;=> true

(parents myhier :a)
;=> #{:letter}

(defmulti letter? identity :hierarchy #'myhier)   ; defmulti has :hierarchy option that takes var
;=> #'user/letter?

(defmethod letter? :letter [_] true)
;=> #<MultiFn clojure.lang.MultiFn@17c26ef7>

(letter? :d)
;=> IllegalArgumentException No method in multimethod 'letter?' for dispatch
    value: :d clojure.lang.MultiFn.getFn (MultiFn.java:160)

(def myhier (derive myhier :d :letter))   ; taking a var allows mutation
;=> #'user/myhier

(letter? :d)
;=> true
```

There are a few important differences between this code with an explicit hierarchy
and the code you’ve been writing up until now using the global hierarchy. 

1. you call make-hierarchy to create a new (empty) hierarchy. A hierarchy is really just a
map with three familiar-looking keys—it’s not anything special. 

2. derive and underive have three-argument forms that accept a hierarchy, but these don’t return
nil like their two-argument forms. Instead, they return a new hierarchy map and don’t
mutate the existing map. 

3. notice that the three-argument form of derive doesn’t require namespaced keywords; 
because the hierarchy is empty and isolated there isn’t as much concern about type name collisions 
as there is in the global namespace. 

4.  a multimethod is create to use a custom hierarchy by calling
defmulti with the :hierarchy keyword option. You must pass the hierarchy as a var,
not a plain map. It would be impossible to change the type hierarchy later if you were
to pass a plain (immutable) map. Instead you pass a var containing the hierarchy map,
and the multimethod deref s it every time it needs to call isa? . This is demonstrated
in the previous code by adding the :d type to the hierarchy after defining the method:
note that the change is seen by letter? .

#### WHY WOULD YOU EVER WANT TO CREATE YOUR OWN HIERARCHY? 
There are two primary reasons. The first is **hierarchy isolation**: if you use an independent hierarchy, 
you’re never in danger of someone’s code accidentally changing your type relationships while manip-
ulating the global hierarchy. In fact, it’s even possible to make your type hierarchy com-
pletely nonextensible simply by making it private to your library and inaccessible to
other namespaces (for example, by using ^:private or a closure). This allows other
code to add their own methods but not their own types, which may occasionally be desir-
able. Because hierarchies are isolated, derive ’s three-argument form also relaxes the
requirement that your types have a namespace, so you can name your types bare key-
words like :letter . Note, however, that your own hierarchies are not isolated from the
Java type hierarchy! (isa? (make-hierarchy) java.lang.String java.lang.Object)
still returns true !

The second reason you might want your own hierarchy is if multiple methods share
the same types but dispatch according to different (possibly contradictory) type hierar-
chies. A classic example is *circles and ellipses*: an area function may be able to say that a
:circle is a kind of :ellipse and use the same formula (2  wh) to calculate the area for
both, but a stretch function may need to make the opposite assumption and imple-
ment ellipse-stretching as a special case of circle-stretching. 1 Separate multimethod hier-
archies are used very rarely in Clojure, but they’re available if you need them.



## CHAPTER 6: State and the concurrent world

#### STATE
State is the current set of values associated with things in program.
There is no problem iwth state per se or even with mutating state. The problem
occurs when concurrent(multithreaded) programs share this sort of state among different threads and then
attempt to make updates to it. When the illusion of single-threaded execution breaks down, the code 
encounters all manner of _inconsistent_ data.

#### COMMON PROBLEMS WITH SHARED STATE
Most of the problems with multithreaded programs happen because changes to shared data arent correctly protected.

1. LOST OR BURIDED UPDATES
Lost updates occur when two threads update the same data one after the other.
For example: 2 threads incremeneting a counter, current value is 10.
Because execution of threads is interleaved **both** threads can do a read on the counter and think the value is 10.
Then both increment it to 11. The problem is that the final value should have been 12, one update got lost.

2. DIRTY AND UNREPEATABLE READS
Dirty read - happens when a thread reads data that another thread is in the process of updating. That threads reads incomplete/inconsistent dirty data.
Unrepeatable read - when a thread reads a particular data set, but because other treads are updating it, the thread can never do another read that results in it seeing the same data again.

3. PHANTOM READS
Happens when a thread reads data thats been deleted. (or more data is added)

#### TRADITIONAL SOLUTION

##### LOCKING
The most obvious solution to these problems is to impose a level of control on those
parts of the code that deal with such mutable, shared data. This is done using locks,
which are constructs that control the execution of sections of code, ensuring that only
a single thread runs a lock-protected section of code at a time.

##### DISADVANTAGES OF LOCKING
The most obvious disadvantage of locking is that code is less multithreaded than it was
before the introduction of locks. When one thread obtains and holds a lock, no other
thread can execute that code, causing other threads to wait. This can be wasteful, and
it reduces throughput of multithreaded applications.

Further, locks are an excessive solution. Consider the case where a thread only
wants to read some piece of mutable data. To ensure that no other thread makes
changes while it’s doing its work, the reader thread must lock all concerned mutable
data. This causes not only writers to block but other readers too. This is unnecessar-
ily wasteful.

Lastly, another disadvantage of locking is that you, the programmer, must remember
to lock, and lock the right things, and in the right order. If someone introduces a bug
that involves a forgotten lock, it can be difficult to track down and fix. There are no
automatic mechanisms to flag this situation and no compile-time or runtime warnings
associated with such situations, other than the fact that the program behaves in an
unexpected manner!

#### NEW PROBLEMS WITH LOCKING
When a single thread needs to change more than one piece of mutable data, it needs
to obtain locks for all of them. This is the only way for a lock-based solution to ensure
coordinated changes to multiple items. The fact that threads need to obtain locks to
do their work causes contention for these locks.

* **Deadlock**
This is the case where two or more threads wait for the other to release locks that they
need. This cyclic dependency results in all concerned threads being unable to proceed.
Starvation This happens when a thread isn’t allocated enough resources to do its job, causing it to
starve and never complete.

* **Starvation**
This happens when a thread isn't allocated enough resources to do its job, causing it to starve and never compelte.

* **Livelock**
This is a special case of starvation, and it happens when two threads continue executing
(that is, changing their states) but make no progress toward their final goal. Imagine
two people meeting in a hallway and each trying to pass the other. If they both wait
for the other to move, it results in a deadlock. If they both keep moving toward the
other, they still end up blocking each other from passing. This situation results in a
livelock, because they’re both doing work and changing states but are still unable
to proceed.

* **Race condition**
This is a general situation where the interleaving of execution of threads causes an unde-
sired computational result. Such bugs are difficult to debug because race conditions hap-
pen in relatively rare scenarios.



#### SEPARATING IDENTIES AND VALUES
