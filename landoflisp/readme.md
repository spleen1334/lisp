# LAND OF LISP



### CHAPTER 2 (GUESS THE NUMBER)

* defparamter - definicija globalne var
  *  Konvencija je da se globalne imaju *ime* u nazivu
  ``(defparamter *small* 123)``
  
* defvar - definicija globalne var koja nece overwritovati prethodnu vrednost globalne var (ovo dodje kao neka konstanta)
  ``(defvar *foo* 55)``

* defun - globalna funkcija
  ``(defun function_name (arguments)
      ...body)``
      
* setf - promeni vrednost globalne variable

* let/flet - lokalne var/funkcije (u okviru deklaracije moguce je definisati vise var/func)
  ``(let (variable declarations)
      ...body)``
  ``(flet (function_name (arguments)
      ...function body))
    ...body)``

* labels - slicno kao flet ali dozvoljava da funkcija zove samu sebe (rekurzija)
  ``(labels ((a (n)
              (+ n 5))
            (b (n)
              (+ (a n) 6))) ;; ovde se poziva A funkcija to bi bilo nemoguce u flet
      (b 10))``

-------------------------------------------------------------------------------

### CHAPTER 3

* Sav kod u lispu je organizovan u liste uz pomoc ()

#### Osnovni elementi

* SYMBOLS
Stand alone word. Letters, numbers, chars -+<>.... Case INSENSITIVE.

``(eq 'fooo 'FooO) ;; true`` 

* NUMBERS
Integers, float. 1 != 1.0

``(expt 53 53) ;; exponent``
``(+ 1 1.0) ;; = 2.0``
``(/ 4 6) ;; = 2/3 vraca rational number``
``(/ 4.0 6) ;; = 0.666667 fraction`` 

* STRINGS
Double quote je valid string. \ za escaping
``(princ "Test string")``

#### LISP MODES
Lisp koristi 2 moda za razlikovanje koda i podataka - CODE mode i DATA mode.

* CODE MODE
Svaki kod se unosi kao lista. Specijaln tip liste je FORM.

(foo bla bla bla bla) 
foo = commanda
bla bla ... = form

Form simply a list with a special command at the beggining usually name of function.

* DATA MODE
To znaci da se taj deo koda nece izvrsiti.

'(expt 2 3) ;; ovo sada ne izvrsava kod vec pravi novu listu

#### QUOTING 
placing a quote **'** in front of list so they wont be evaluated as command

#### LISTS

Liste su najbitniji deo Lisp-a. One spajaju code + data.

(expt 2 3)
Contains symbol (expt) and 2 numbers tied toghter as list indicated by parentheses.

#### CONS CELLS
Ono sto spaja liste, symbole, stringove, brojeve.

> It’s made of two little connected boxes, both of which can point at other
> things. A cons cell can point to another cons cell or another type of Lisp data.
> By being able to point to two different things, it’s possible to link cons cells
> together into lists. In fact, lists in Lisp are just an abstract illusion—all of them
> are actually composed of cons cells.


'(1 2 3)

[1] [] ;; points to 2
    [2] [] ;; points to 3
        [3] [nil]

> It’s created using three cons cells. Each cell points to a number, as well as
> the next cons cell for the list. The final cons cell then points at nil , to terminate
> the list.

#### SEXP
Ovo je ustvari ono sto se zove Symbolic Expression ili SEXP.
[Wikipedia S-Exp
](https://en.wikipedia.org/wiki/S-expression)

![(* 2 (+ 3 4))](https://en.wikipedia.org/wiki/S-expression)


#### 3 BASIC FUNCTIONS FOR MANIPULATING CON CELLS/LISTS

* **cons**
Povezuje 2 podatka bilo kog tipa.

``(cons 'chiken 'cat) 
;; (CHICKEN . CAT)``
> As you can see, cons returns a single object, the cons cell, represented by
> parentheses and a dot between the two connected items. Don’t confuse this
> with a regular list. The dot in the middle makes this a cons cell, just linking
> those two items together.

Specijalan slucaj sa nil:
``(cons 'chicken 'nil) 
;; (CHICKEN)``
> Unlike with our cat , the nil does not show in the output this time. There’s a
> simple reason for this: nil is a special symbol that is used to terminate a list in
> Lisp. That said, the Lisp REPL is taking a shortcut and just saying that we
> created a list with one item, our chicken . It could have displayed the result by
> explicitly showing our cons cell and printing (CHICKEN . NIL) . However, because
> this result is coincidentally also a list, it instead will show the list notation.

> The lesson here is that Lisp will always go out of its way to “hide” the cons
> cells from you. When it can, it will show your results using lists. It will show a
> cons cell (with the dot between the objects) only if there isn’t a way to show
> your result using lists.

Empty list () se moze koristi umesto nil - interchangeable

Sta se desava u ovom slucaju npr:
``(cons 'pork (cons 'beef (cons 'chicken ()))) 
;; (PORK BEEF CHICKEN)``

* **car**
Uzmi prvi element iz cons cells / liste
``(car '(pork beef chicken))
;; PORK``

* **cdr**
Uzmi drugi elment ili ostatak liste
``(cdr '(pork beef chicken))
;; BEEF CHICKEN``

* **cadr** / **cdar** / **cadadr**
Varijacije na car cdr. Npr cadr = car + cdr
``(car (cdr `(prok beef chicken)))``
``(cadr `(prok beef chicken))``

#### LIST FUNCTIONS

* **list**
Obavlja prljav posao kreirajuci sve neophodne cons cells

``` common-lisp
;; Sve su iste
(cons 'pork (cons 'beef (cons 'chicken)'))
(list 'pork 'beef 'chicken)
'(pork beef chicken)
```

* Nested liste
List mogu da sadrze u sebi druge liste:
``` '(cat (duck bat) ant) ```

* __c**r__
car/cdr imaju razne built-in kombinacije. Slova A i D oznacavaju car/cdr komande.
REdosled izvrsavanje je nested - cdar > cdr (car) - odnosno s desna na levo (unutra ka spolja)

PRIMERI:
``` common-lisp
(cddr '((peas carrots tomatoes) (pork beef chicken) duck))
=(DUCK)
(caddr '((peas carrots tomatoes) (pork beef chicken) duck))
=DUCK
(cddar '((peas carrots tomatoes) (pork beef chicken) duck))
=(TOMATOES)
(cadadr '((peas carrots tomatoes) (pork beef chicken) duck))
=BEEF
```

-------------------------------------------------------------------------------

### CHAPTER 4

Simetrija je osnovni princip u LISP.

* TRUE/FALSE
Prazne liste su false (ovo se cesto koristi) liste sa nekom vrednoscu su true.
nil = () = false

``` common-lisp
(defun my-length (list)
  (if list
      (1+ (my-length (cdr list))) ;; rekuzija cesto se podstice
      0))

(my-length '(list with four symbols))
= 4
```

* FALSE
**Jedina** false vrednost u LISP je empty lista.
Bilo koja vrednost koja nije jednaka praznoj listi je true.

(eq '() nil)  ==> T
(eq '() ())   ==> T
(eq '() 'nil) ==> T

Ovde se ujedno spominju neki koncepti vezani za nacin na koji LISP evaluira ovaj kod:
> The first two examples are particularly puzzling. They are missing the
> quotation mark that tells the Lisp environment, “Hey, this item is a piece of
> data, not code!”

> The bottom line is that Common Lisp is architected behind the scenes to
> make sure all four of these values look like an empty list when you use them
> in your program, allowing most Lisp conditionals to be written with an elegant
> brevity. For instance, there is a constant named nil that evaluates to itself and
> allows you to omit the quotation mark in the first case . The second case
> is a natural by-product of how Common Lisp parses an empty form. The third
> case is due to a requirement in the Common Lisp spec that says that () and
> nil should be treated the same.

#### CONDITIONALS

##### IF
If ima neke specificne propertije u LISP.
1. Only one expresion after the if is evaluated
2. We can only do one thing in an if statement

``` common-lisp
(if (oddp 5)
    'odd-number
    (/ 1 0)) ;; ovo se ne izvrsava
```

SPECIAL FORM
> Usually, when a function is executed in Lisp, all the expressions after the
> function name are evaluated, before the function itself is evaluated. However,
> if does not follow these rules.
> But if is not just a function. It’s a special form, which gives it special privi-
> leges, such as the right to not evaluate all its parameters in the normal way.

U slucaju da zelimo da izvrsimo vise od jedne stvari postoji komanda **progn**

``` common-lisp
(defvar *number-was-odd* nil)

(if (oddp 5)
    (progn (setf *number-was-odd* t) ;; izvrsava update globalne var
            'odd-number)
    'even-number)
```


##### WHEN & UNLESS
Da se ne bi stalno explicitno pozivao prgn kada zelimo da izvrsimo vise stvari,
odredjene komande u LISPu sadrze __implicitni progn__.

``` common-lisp
(defvar *number-is-odd* nil)

(when (oddp 5)
      (setf *number-is-odd* t)
      'odd-number)

(unless (oddp 4)
        (setf *number-is-odd* nil)
        'even-number)
```

##### COND
LISP kondicional/branching komanda - idiosinkratican lisp way.

``` common-lisp
(defvar *arch-enemy* nil)

(defun pudding-eater (person)
      (cond ((eq person 'henry) (setf *arch-enemy* 'stupid-lisp-alien)
                                '(curse you lisp alien – you ate my pudding))
            ((eq person 'johnny) (setf *arch-enemy* 'useless-old-johnny)
                                '(i hope you choked on my pudding johnny))
            (t                  '(why you eat my pudding stranger ?)))) ;; ovo se uvek izvrsava true == true
```

##### CASE
Koristi eq za poredjenje, i koristi se samo za branching symbol vrednosti. NEMOZE se koristiti za stringove.

``` common-lisp
(defun pudding-eater (person)
      (case person
            ((henry) (setf *arch-enemy* 'stupid-lisp-alien)
                      '(curse you lisp alien – you ate my pudding))
            ((johnny) (setf *arch-enemy* 'useless-old-johnny)
                      '(i hope you choked on my pudding johnny))
            (otherwise '(why you eat my pudding stranger ?))))
```

#### AND OR

SHORTCUT BOOLEAN EVALUATION

> This means that once Lisp determines that an earlier statement in a list of or values
> is true, it simply returns true and doesn’t bother evaluating the remaining
> statements. Similarly, once it determines that an earlier statement in a list of and
> values is false, it stops without bothering to evaluate the rest of the statements.

``` common-lisp
(and (oddp 5) (oddp 7) (oddp 9))
(or  (oddp 4) (oddp 7) (oddp 8))

;; primer za shotcut boolean evaluation

(defparameter *is-it-even* nil)

(or (oddp 4) (setf *is-it-even* t))
(or (oddp 5) (setf *is-it-even* t))
```

``` common-lisp
(if *file-modified*
    (if (ask-user-about-saving)
        (save-file)))

;; isto - cleaner style, symmetry, lisp way

(and *file-modified* (ask-user-about-saving) (save-file))

;; 3rd way: 
;; Many experienced Lispers will consider this version a bit clearer than the
;; previous two versions, because only expressions that are expressly designed
;; to return a Boolean value are treated as part of the condition.

(if (and *file-modified*
          (ask-user-aboutsaving))
    (save-file))
```

##### MORE THEN TRUE

> Now let’s look at another benefit of Lisp’s simple way of thinking about true and
> false. As we’ve already discussed, any value in Common Lisp (except for the
> different variations on nil ) is true. This means that functions that are com-
> monly used in conditions have the option of returning **more than just the truth**.

Primer za ovo je member funkcija:

``` common-lisp
(if (member 1 '(3 4 1 5))
    'one-is-in-the-list
    'one-is-not-in-the-list)
;; = 'ONE-IS-IN-THE-LIST

;; Ovde prikazujemo da member ne vraca samo 1, vec i 5
(member 1 '(3 4 1 5))

;; = (1 5)
```

> Remember from Chapter 3 that the list '(3 4 1 5) is the same as the nested contraption
> (cons 3 (cons 4 (cons 1 (cons 5 nil)))) . This should make it clear why the value
> (cons 1 (cons 5 nil)) is an easy thing for the member function to return.

Ovo se desava zbog jednog edge case (i principa u LISP) koji se tice **nil**

``` common-lisp
(if (member nil '(3 4 nil 5))
    'nil-is-in-the-list
    'nil-is-not-in-the-list)
```

> As you can see in this example, the member function still gives the correct
> answer, even when we search for nil as the member! If the member function
> had actually returned nil (in other words, the original value we were search-
> ing for), it would have evaluated as false, and the example would have incor-
> rectly stated that nil isn’t in the list. However, since the member function returns
> the tail of the list at the point of the found item, it can be guaranteed to always
> be a true value. A successful discovery of the desired value will always return a
> list with at least one value, which we know always evaluates as true.

##### FIND-IF
Jedna od funkcija koja ima koristi od ovakvog pristupa return vrednosti.

> The find-if function actually takes another function, in this case oddp , as
> a parameter. **find-if will find the first value in the list for which oddp returns
> true.
**
``` common-lisp
(find-if #'oddp '(2 4 5 6))
;; = 5

(if (find-if #'oddp '(2 4 5 6))
    'there-is-an-odd-number
    'there-is-no-odd-number)
;; = 'there-is-an-odd-number
```

> You can see clearly how find-if can fill dual roles: either as a retriever of
> values matching some constraint or as a true/false value inside a condition.


EDGE Case za find-if:

> The null function, which returns true for any of the nil values, correctly
> finds the nil . Unfortunately, in this one annoying case, we would not want to
> use find-if inside a conditional statement, because a correctly found value
> still returns a result that evaluates as false. The symmetry has been broken.

``` common-lisp
(find-if #'null '(2 4 nil 6))
;; = nil
```

##### COMPARING EQUALITY
LISP ima zamrsen nacin za poredjenje razlicitih tipova verdnosti za jednakost.

* eq
Symboli se trebaju uvek porediti uz pomoc eq

* equal
Ukoliko se neradi sa simbolima najprostije je koristiti equal
equal govori da li su 2 vrednosti **isomorphic** - 'look the same'

``` common-lisp
;; symbols
(equal 'apple 'apple)

;; lists
(equal (list 1 2 3) (list 1 2 3))

;; identical list created in diffrent ways
(equal '(1 2 3) (cons 1 (cons 2 (cons 3))))

;; int
(equal 5 5)

;; float
(equal 2.5 2.5)

;; strings
(equal "foo" "foo")

;; characters
(equal #\a #\a)
```

Postoji jos nekoliko varijacija koji se odnose na razlicite data-types ali eq/equal su glavni.

* eql
Slicno sa eq ali radi i sa brojevima i characters

* equalp
Isto sto i equal ali sa par extra dodataka:
- strings sa razlicitiom capitalizacijom
- int i float

* =
Za rad sa brojevima

* string-equal 
Strings

* char-equal
Characters

-------------------------------------------------------------------------------

### CHAPTER 5: TEXT GAME

#### ALIST
Prvo pravimo *nodes* var koji prestavlja **association list** ili *alist*. key => value
Koristimo liste i simbole umesto stringa.


#### ASSOC
ucitaj item uz pomoc key-a
(assoc 'garden *nodes*)

> Why don’t we just reference the *nodes* variable directly from the describe-
> location function? Because this function is written in the functional programming
> style. In this style, a function will reference only parameters or variables
> declared in the function itself, and it will do nothing besides return a value,
> which is the description of the location in this case.


#### QUASIQUOTING
Nesto kao str interpolation. Enableu je se sa ` (backquote) NE sa '(single quote).
` vrsi prebacivanje kod u data mode, a zarez (,) ga unquotuje vraca u code mode

(defun describe-path (edge)
  '(there is a ,(caddr edge) going ,(cadr edge) from here.))


#### ADVANCED 

``` common-lisp
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))
```

DESCRIBE PATHS:
1. Find the relevant edges

``` common-lisp
(cdr (assoc 'liging-room *edges*))
=> ((GARDEN WEST DOOR) (ATTIC UPSTAIRS LADDER))
```

2. Convert the edges to descriptions
Koristi se **mapcar** to je map funkcija.

``` common-lisp
(mapcar #'describe-path '((GARDEN WEST DOOR) (ATTIC UPSTAIRS LADDER)))

=> ((THERE IS A DOOR GOING WEST FROM HERE.) (THERE IS A LADDER GOING UPSTAIRS FROM HERE.))
```

#### HIGH ORDER FUNCTIONS
Funkcije koje uzimaju druge funkcije za parametre. (npr mapcar)

#### #' - Function operator
Ovo pretstavlja shorthand za **function** operator >> **#'**.
Ustvari ovde se desava pretvaranje shorthanda u function.
Common LISP zahteva koriscenje **function** operatora kada se ista koristi kao vrednost.

Common LISP ima u sebi multiple namespaces, izmedju ostalog jedan za functions drugi za variables.

RAZLIKA: COMMON VS SCHEME LISP
> In other words, Scheme has only one namespace for both functions and
> variables. For instance, in Scheme, you can just write (map sqrt '(1 2 3 4 5)) to
> generate the square roots of the numbers 1 through 5 without generating an
> error ( map is the Scheme version of mapcar ). As a result of this design, in Scheme,
> a variable and a separate function can’t be available in the same block of
> code. That design decision is one of the great benefits (or curses) of Scheme,
> depending on your point of view. Because of this difference in the number of
> namespaces, Scheme is sometimes called a Lisp-1, whereas Common Lisp is
> sometimes referred to as a Lisp-2.


3. Joining the descriptions
Za tu svrhu koristimo **append**.

``` common-lisp
(append '(mary had) '(a) '(little lamb))
```

Problem sa append:
> We use the append function to cram the list of path descriptions into one
> list that describes the whole enchilada, in one swoop. The problem is that append
> needs all of the lists handed to it as separate parameters. In describe-paths , we
> have our lists in one big list, not as separate objects we can pass as parameters.

Resenje problema je u koriscenju  **apply**.
Apply prihvata funckiju i listu, i onda on listu tretira kao pojedinacne objekte nad kojima poziva funkciju.


#### REVIEW

``` common-lisp
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))
```
> The function takes two parameters: the current player’s location, as well
> as an alist of edges/paths for the game map. First, it uses assoc to look up the
> correct location from the edge alist. Since assoc returns both the key and the
> value from the alist, we call cdr to retrieve only the value. Next, we use mapcar
> to map the describe-path function against each edge that we found. Finally, we
> concatenate the lists for describing all the paths into one long list by applying
> append against the list.

#### OBJECT IN GAMEWORLD

``` common-lisp
(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
                     (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))
```
1. Deklaracija nove funkcije at-loc-p sa **labels** (definise lokalne funkcije)

2.The at-loc-p function takes the symbol for an object and returns t or nil ,
depending on whether that object exists at the location loc . It does this by
looking up the object in the obj-locs alist. Then, it uses eq to see whether the
location it finds matches the location in question.

Konvencija u LISP je da se kada funckcija vraca true/false dodaje (p) na kraju.
(npr oddp 5). Naziv za te funkcije je **predicates**

3. **remove-if-not** funkcija uklanja sve stvari iz liste za koje funkcija at-loc-p ne vraca true.

#### DESCRIBE VISIBLE OBJ

``` common-lisp
(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
              `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))
```

> In this listing, describe-objects first creates the describe-obj function (1) This
> function generates a pretty sentence stating that a given object is on the floor,
> using quasiquoting (2) The main part of the function consists of calling objects-
> at to find the objects at the current location, mapping describe-obj across this list
> of objects, and finally appending the descriptions into a single list (3)

#### LOOK
look funkcija nije u skladu sa principima funkcionalnog programiranja.
Razlog za to je zbog toga sto se lokacija igraca dinamicki menja i ono sto igrac vidi 
zavisi od trenutne lokacije. Ovo jednostavno nije moglo da se odradi na funkcionalan nacin.

#### WALK
Ovo nije napisano u funkcionalnom stilu.

``` common-lisp
(defun walk (direction)
  (let ((next (find direction
                    (cdr (assoc *location* *edges*))
                    :key #'cadr)))
  (if next
      (progn (setf *location* (car next)) ;; podsetnik progn omogucava izvrsavanje vise od jedne komande
              (look))
      '(you cannot go that way.))))
```

> First, this function looks up the available walking paths in the *edges* table,
> using the current location (2) This is used by the find function to locate the
> path marked with the appropriate direction (1) ( find searches a list for an
> item, then returns that found item.) The direction (such as west , upstairs , and
> so on) will be in the cadr of each path, so we need to tell find to match the
> direction against the cadr of all the paths in the list.
> We can do this by passing find a keyword parameter (3)

#### KEYWORD PARAMETAR
U Lispu mnoge funkcije imaju built-in features kojima se moze pristupiti passovanjem
specijalnih parametara na kraju poziva funkcije.

``` common-lisp
;; pronadji prvi item koji ima y na cadr lokaciji
(find 'y '((5 x) (3 y) (7 z)) :key #'cadr)
;;=> (3 Y)
```

Keyword parametar ima 2 dela:
1. ima koje pocinje sa : - npr :key
2. vrednost - npr #'cadr
(podsetnik #' je function operator shorthand = function)

> Once we have the correct path, we store the result in the variable next (1)
> The if expression then checks whether next has a value (the next variable
> isn’t nil )(4) If next has a value, if adjusts the player’s position because this is a
> valid direction (5) The call to look retrieves the description for the new
> location and returns it as a value. If the player chooses an invalid direction,
> look will generate an admonishment instead of a new description (7)

#### PICKUP OBJECT

``` common-lisp
(defun pickup (object)
  (cond ((member object
                 (objects-at *location* *objects* *object-locations*))
          (push (list object 'body) *object-locations*)
          '(you are now carrying the ,object))
          (t '(you cannot get that.))))
```

> The pickup function uses the member (1) function to see if the object is indeed
> on the floor of the current location. (The member function checks to see if a
> particular item is found in a list of items.) We use the objects-at command (2)
> to generate the lists of objects at the current location.
> If the object is at the current location, we use the push command (3) to
> push a new item onto the *object-locations* list, consisting of the item and its
> new location. The new location will just be body , for the player’s body.
> The push command (3) simply adds a new item to the front of a list variable’s
> list.

#### PUSH

> This push command is basically a convenience function built on top of setf .
> For example, we could have replaced the preceding push command with (setf *foo* (cons 7 *foo*)) 
> and obtained the same result. It’s just easier to use push.

#### PROBLEM

> Pushing a new location for an object onto our *object-locations* alist does
> seem a bit odd. Since we’re never removing old locations for objects, just
> pushing new ones, it means that *object-locations* may contain multiple
> entries for a single object, and that this list now has two stored locations for
> the object in question. Fortunately, the assoc command, which we use to find
> objects in a given location (within the objects-at command), always returns
> the first item it finds in a list. Therefore, using the push command makes the
> assoc command behave as if the value in the list for a given key has been
> replaced altogether.

> Using the push and assoc commands together in this way allows us to pre-
> tend that values in an alist are changing, while still preserving old values. Old
> values are simply suppressed by newer values, thus preserving a history of all
> old values. The **push / assoc** idiom is a common technique used by Lispers.


#### INVENTORY CHECK
Nista specijalno koristimo ponovo objects-at ali 'hardcoded' 'body simbolom, ona vraca sve iteme
koji su vezani za 'body


-------------------------------------------------------------------------------

### CHAPTER 6: INTERACTING WITH THE WORLD

#### PRINT & PRIN1
Stampa vrednost na novoj liniji. Prin1 stampa na istoj

``` common-lisp
(print "foo")

(progn (prin1 "this")
       (prin1 "is")
       (prin1 "a")
       (prin1 "test"))
```

#### READ
Funkcija za uzimanje inputa od korisnika.

``` common-lisp
(defun say-hello()
    (print "Please type your name:")
    (let ((name (read)))
        (print "Nice to meet you, ")
        (print name)))
```

Jos primera:

``` common-lisp
(defun add-five ()
    (print "please enter a number:")
    (let ((num (read)))
        (print "when i add 5 i get")
        (print (+ num 5)))) ;; print broj bez '
```

``` common-lisp
(print '3)     ;; int
(print '3.14)  ;; float
(print 'foo)   ;; symbol
(print '"foo") ;; string
(print '#\a)   ;; character (literal character)
```

SPECIAL LITERALS: #\newline, #\tab, #\space

STRINGS: case sensitive
SYMBOLS: case INsensitive
Za symbole postoji mogucnost da se ucine case-sensitive upotrebom vertical pipe |.
|CaseSensitveSymbol|


#### PRINTC
Human readable print.


#### PRINT/READ
Specifican nacin na koji funkcionisu. Ono sto se sa njima procita je u computer friendly formi koja omogucava da se iono sto se printuje npr ucita nazad kao odgovarajuci objekat.

> Almost any conceivable type of data in Lisp (with the exception of
> actual functions and some advanced data structures) can be printed and
> read using these commands, without the slightest bit of loss along the way.

> By its nature, **princ** could be used to print any arbitrary output of charac-
> ters you want. This is **fundamentally different from print** . As we’ve discussed,
> the cool thing about the print command is that it prints objects in such a way
> that they can always be “read” back into their internal representation. How-
> ever, this means print can’t be used to generate any arbitrary bit of text.

``` common-lisp
(defun say-hello ()
    (princ "Please type your name:")
    (let ((name (read-line))) ;; hvata sav unet tekst do ENTER
        (princ "Nice to meet you, ")
        (princ name)))
```

> This version of the say-hello function is similar to our first version. How-
> ever, when the computer asks users for their name , it now does so without
> printing quotes around the text string. The same holds true to when we print
> the greeting
> . Also, users can now enter in any name (including a name
> with spaces and quotes), since the read-line command captures and returns
> all the text entered until the ENTER key is pressed, without any fuss.


### HOMOICONIC

> You have seen that Lisp has very elegant and symmetrical facilities for trans-
> lating raw string data from the outside world and converting it to and from
> Lisp syntax expressions. But Lisp has an even deeper symmetry.** It can treat
> program code and data interchangeably. A programming language that uses
> the same data structures to store data and program code is called homoiconic.**

``` common-lisp
'(+ 1 2) ; data mode
(+ 1 2)  ; code mode
```

#### EVAL
Kako da se izvrsi egzekucija koda koji se nalazi u *foo* varijabli.
To se postize uz pomoc **eval**

``` common-lisp
(defparameter *foo* '(+ 1 2))
;;
(eval *foo*)
```

> You want to write a program with self-modifying
> code? Then eval will be your best friend. In fact, this is probably the main
> reason why the artificial intelligence (AI) freaks back in the day loved Lisp so
> much.

> However, an experienced Lisper will only **rarely** use eval .
> Often, a beginning Lisper will use the eval command instead of defining a Lisp macro.

HOMOICONICITY: 
LISP ima nekoliko mogucnosti kao sto su quoting, quasiquoting, eval, macros...


#### REPL (Read Eval Print Loop)
Zanimljiv primer gde se izvrsava operacija bas u tom redosledu:

``` common-lisp
(defun game-repl ()
    (loop (print (eval (read))))) ;; 1. read 2. eval 3. print 4. loop
    
;; => (game-repl)
;; >> (look)
;; -- izvrsava se (look) funkcija
```

#### LOOP
Petlja koja se izvrsava beskonacno

#### REFACTOR REPL-a

(defun game-repl ()
    (let ((cmd (game-read)))
        (unless (eq (car cmd) 'quit)
            (game-print (game-eval cmd)
            (game-repl)))))

> In this version, we first capture the command the player types using a
> local variable, cmd (1) This way, we can intercept any attempt to call quit and
> use it to exit our game-repl. In other words, we want to continue running our
> REPL unless the user typed quit (2) Otherwise, the function eval s and print s
> (3) but using our custom versions of these functions, which we’ll write shortly.
> Finally, the game-repl function calls itself recursively (4), causing it to loop back,
> as long as we had not decided to quit earlier.


#### CUSTOM-READ
2 razloga zasto pravimo custom read komandu.
  1. LISP force-uje () oko komandi (za eval)
  2. ' ispred svakog podatka npr walk 'east

``` common-lisp
(defun game-read ()
    (let ((cmd (read-from-string
                    (concatenate 'string "(" (read-line) ")" ))))
    (flet ((quote-it (x)
                  (list 'quote x)))
       (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))
```

> The **read-from-string** command (1) works just like the read command, but
> lets us read a **syntax expression** (or any other basic Lisp datatype) from a
> string instead of directly from the console.

> The string we use for this is a tweaked version of a string we get from
> read-line (2). We tweak it by adding quotes around it using the **concatenate**
> command, which can be used for concatenating strings together, as well as
> some parentheses. The result is that the cmd variable will be set to the player’s
> requested command and converted into a Lisp **syntax expression**. For exam-
> ple, if the player types in walk east , the cmd variable will be set to the expres-
> sion (walk east) , which is a list containing two symbols.

> Next, we define a local function called quote-it (3), which we can use to
> quote any arguments the player has in a command. How exactly does it manage
> to quote a parameter? Well, it turns out that the single quote is just short-
> hand for a Lisp command called **quote** . This means that 'foo and (quote foo)
> are the same. We can quote a raw parameter by simply putting the parameter
> in a list with the quote command in front.

> Remember that local functions can be defined with **labels** or **flet** . Since we
> are not using any recursion in the quote-it function , we can use the simpler
> flet command. The final line in the game-read function applies quote-it to
> every argument in the player’s command. It does this by mapping quote-it
> across the cdr of the cmd variable (4) (and then attaching the first word in the
> command back on front with car ).


#### GAME-EVAL
Eval izvrsava bilo koju LISP komandu, sa ovim zelimo da ogranicimo eval na 
listu nasih komandi.

``` common-lisp
(defparamter *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
    (if (member (car sexp) *allowed-comamands*)
        (eval sexp)
        '(i do not know that command.)))
```

> The game-eval function checks if the first word in the entered command is
> in the list of allowed commands, using the member function (1) If it is, we then
> use the standard eval to execute the player’s command (2)


#### GAME-PRINT


``` common-lisp
(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst)) ;;1
          (rest (cdr lst)))
      (cond ((eq item #\space) (cons item (tweak-text rest caps lit))) ;;2
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit))) ;;3
            (eq item #\") (tweak-text rest caps (not lit))) ;;4
        (lit (cons item (tweak-text rest nil lit)))
      ((or caps lit) (cons (char-upcase item) (tweak text rest nil lit))) ;;5
      (t (cons (char-downcase item) (tweak text rest nil nil)))))) ;;6

(defun game-print (lst)
  princ (coerce (tweak-text (coerce (string-trim "()" ;;7
                                                 (prin1-to-string lst)) ;;8
                                    'list)
                            t
                            nil) ;;9
                'string))
  (fresh-line))
```

#### RAZNI UPOTREBLJENE F()
* **member** - da li pripada listi
* **not** - negacija !
* **char-upcase**, **char-downcase** - upcase/downcase 1 charactera
* **coerce** - spaja 2 objekta poslenji parametar je *type specifier* za rezultat ('string i 'list)
* **fresh-line** - nova linija
* **string-trip** - skida sve character sequence iz prvog argumenta "()" sa drugog argumenta (targeta)

> The game-print function and its helper function are a bit more complicated
> than the other functions we’ve looked at so far. The first important part of
> the code that is executed is in game-print , where it converts the **symbol list**
> (containing the text whose layout we want to fix) into a **string** with **prin1-to-
> string** (8), one of Lisp’s many print variants. The **to-string** part means this
> function doesn’t dump the result to the screen, but just **returns it as a string**.
> The **1** means it will stay on a single line. The standard print command precedes
> its output with a newline character and also follows it with a space. The func-
> tions prin1 and prin1-to-string variants don’t add these extra characters.

> Next, game-print converts the **string to a list of characters** with the **coerce**
> function (7) By coercing our string into a list, we can reduce the bigger goal
> of the function into a list-processing problem. This is smack-dab in the Lisp
> comfort zone. In this case, we’re creating a list of the characters making up
> the text we want to fix.

> We can now send the data to the list-eater function tweak-text (7) Notice
> that some of the arguments used in the code of the game-print function are
> printed on their own line for clarity. You can easily see which arguments are
> meant for which commands by looking at the **indentation**. For instance, the t
> and nil arguments belong to tweak-text .

> The tweak-text function looks at each character in the list and modifies it
> as needed. At the top of this function, we define two local variables, **item and
> rest** , which we get by chewing off an item from the front of the sentence we’re
> tweaking (1) Then, the tweak-text function uses a **cond** to check the character
> at the top of the list for different conditions (2)

> The **first condition** it checks for is whether the character is a **space
> character** (2) If so, it just leaves the space unchanged and processes the next
> character in the list. 
> If the character is a **period, question mark, or exclamation point **(3), 
> we turn on the **cap parameter** for the rest of the string (by using
> the value t as an argument in the recursive call) to indicate that the next sym-
> bol is at the beginning of a sentence and needs a **capital** letter.

> We also track whether we’ve encountered a **quotation mark** (4) We do
> this because, infrequently, a symbol list is not adequate for encoding English
> text. Examples include having a **comma** **(commas are not allowed in standard
> Common Lisp symbols)** or product names with **nonstandard capitalization**.
> In these cases, we can just fall back on using text strings. Here’s an example:
``` common-lisp
 (game-print '(not only does this sentence have a "comma," it also mentions the "iPad."))
 ;; => Not only does this sentence have a comma, it also mentions the iPad.
```

> Our sample game doesn’t actually need the fallback facility. Nonetheless, 
> this feature allows the game-print function to handle many basic exceptional 
> text situations that you may encounter if you try to expand the game on your own. 
> We tell the function to treat the capitalization as shown literally
> by turning on the **lit** variable in the recursive call. As long as this value is set,
> the tweak-text function prevents the capitalization rules (which start at (5))
> from being reached.

> The next thing the tweak-text function checks is whether the **next character
> is supposed to be capitalized**. If it is, we use the char-upcase function to change
> the current character to uppercase (if it isn’t already) before processing the
> next item in the list (5)

> If none of the other conditions were met, we know that the current char-
> acter should be lowercase (6), and we can convert it using the char-downcase
> function. (LISP default fallback za cond t)

> After tweak-text is finished correcting the text in the character list, the
> game-print function **coerces** it back into a proper **string** and **princ** s it . The
> **fresh-line** function at the end of game-print makes sure that the next item
> appearing on the screen will start on a fresh line.


-------------------------------------------------------------------------------


### CHAPTER 6.5 LAMBDA

Lambda ili anonimne funkcije.
Lambda lets you create a function without giving it a name.
Funkcije u LISPu su **first-class values.**

``` common-lisp
(defun half (n)
    (/ n 2))

#'half ;; function operater ucitava funkciju

;; lambda skracuje ova 2 koraka na jedan

(lambda (n) (/ n 2))
```

Primer:
``` common-lisp
(mapcar (lambda (n) (/ n 2)) '(2 4 6))
;; => (1 2 3)
```

##### SPECIFICNOSTI LAMBDA

> Because not all parameters of the lambda command are **evaluated**, lambda
> itself is not actually a **true function**. It is something called a **macro**. Remember
> from Chapter 2 that all parameters to a Lisp function are **evaluated before**
> the function itself is evaluated. Macros, on the other hand, have special powers 
> and are allowed to break those rules.

> Also, to confuse matters a bit, the actual value that lambda returns is a
> **regular Lisp function**—in this case, a function that cuts a number in half.
> When Lispers talk about lambda functions—which they pretty much do for
> breakfast, lunch, and dinner—they’re talking about functions created using
> lambda . They’re not talking about the lambda macro itself, which is not a func-
> tion. Got that?

> While most programming languages try to keep the worlds of functions
> and values separate, Lisp lets you bridge these worlds as desired.

> You will see that most Lisp programs use this command very heavily.


#### WHY IS LAMBDA IMPORTANT

> *The ability to pass around functions as if they were just plain old pieces of
> data is incredibly valuable.* Once you get used to doing this, you open up all
> kinds of conceptual possibilities in the design of your programs. Eventually,
> your programs will start looking very different from programs in more (dare
> I say) pedestrian languages, such as Java or C. The name for the style of pro-
> gramming that relies heavily on passing functions as values is called **higher-order
> functional programming.**

> **An even more important reason why Lispers go gaga over lambda is that,
> as it turns out, in a purely mathematical sense, lambda is actually the only Lisp
> command there is!**

##### LAMBDA CALCULUS

> Recall that Lisp is unusual among programming languages in that it was
> derived directly from a mathematical concept called the lambda calculus. In
> short, the lambda calculus is a theoretical programming language that contains
> only one command: the lambda command. By having only this single command
> and using special code transformations, it’s possible to create a fully function-
> ing (though perhaps not practical) programming language.

> **The take-home point is that the lambda special form is the most funda-
> mental command in a Lisp system, and the central concept from which other
> functions in Lisp derive. In fact, it is the central concept from which the very
> idea of Lisp itself originated**


-------------------------------------------------------------------------------


### CHAPTER 7

Svaka lista u LISPu je napravljena od **cons** cellova. Desna slot u cellu sadrzi **nil** i ono oznacava kraj liste.
Shortcut forma za liste je prikazana u primeru. U sustini to je samo estetska promena u pozadini se uvek liste kreiraju kroz cons cellove.

*(cons 1 (cons 2 (cons 3 nil))) === (1 2 3)*

#### DOTTED LISTS
Sta se desava kad se ne ispostuje LISP sintaksa prilikom generisanja listi. (fali nil)

``` common-lisp
(cons 1 (cons 2 3))
;; => (1 2 . 3)
```

> To indicate that the final item in the list wasn’t found in the proper loca-
> tion for a nil -terminated list, Lisp places a dot in front of this final item.

> A list in Lisp that ends in something other than a nil is referred to as a
> **dotted list**. Dotted lists are kind of an oddity in the Land of Lisp. In and of
> themselves, they are not that useful a tool for Lisp programming. It would be
> quite unusual for a Lisp programmer to store data in dotted lists as a regular
> practice. However, given the pervasiveness of cons cells in Lisp, you will fre-
> quently encounter a non- nil value at the end of a chain of cons cells. That’s
> why you should become familiar with dotted lists, even if you may never use
> them directly.

* GIMMICK:
> Another way of thinking about this dot notation is to consider it as sim-
> ply an **alternate syntax** for the cons command, used in **data mode**. In fact, if
> we wanted to make life hard for ourselves, we could even create regular, proper
> lists using the dot notation, like this:

``` common-lisp
'(1 . (2 . (3 . nil)))
```

> Using this line of thinking, the dot appears in a dotted list simply because
> Lisp is **forced** to show the final cons cell in order to maintain the consistency
> of its list-printing mechanism


#### PAIRS
Jedna od cestih i prakticnih primena dotted lista je kreiranje PAROVA.
Koristi se cesto.
``` common-lisp
(cons 2 3)
;; => (2 . 3)
```


#### CIRCULAR LISTS


* UOBICAJENA LISTA:
'(1 2 3)

[1] [] ;; points to 2
    [2] [] ;; points to 3
        [3] [nil]
        
* CIRKULARNA LISTA:
[1] [] ;; points to 2
    [2] [] ;; points to 3
        [3] [1] ;; points **back** to 1

> Every cons cell in a list theoretically exists as a **separate object** in memory.
> Since the car and cdr slots in a cell can point to any other object in memory, a
> cons cell can point to an **upstream cons cell** of a list. We call this a **circular list**.

#### CIRCULAR LISTS SETUP
(setf *print-circle* t)

Ova komanda je bitna prilikom igranja sa cirkularnim listama, da bi se izbegli razne greske,
los output, infinte loops itd..

#### DEFINICIJA CIRKULARNIH LISTI
Najkraci nacin je uz upotrebu setf gde se stavlja extra stuff za prvi parametar:

``` common-lisp
(defparamter foo '(1 2 3))
;=> FOO

(setf (cdddr foo) foo)
;=> #1=(1 2 3 . #1#)
```

> In this example, we’ve created an infinite list of '(1 2 3 1 2 3 1 2 3 ...) by
> replacing the nil at the end of a simple list with a reference to the list itself.


#### ASSOCIATION LISTS (ALIST)

``` common-lisp
(defparamter *drink-order* '((bill . double-espresso)
                             (lisa . small-drip-coffee)
                             (john . medium-latte)))

(assoc 'lisa *drink-order*)
;=> (LISA . SMALL-DRIP-COFFEE)

(push '(lisa . large-mocha-with-whipped-cream) *drink-order*)
;=> ... ovo dodaje novi lisa item na pocetku liste
```

**assoc** vrsi pretragu liste od pocetka za trazeni key i vraca key/value (**prvi** koji pronadje)
**push** dodaje novi item na pocetak postojece liste

One su korisne jer svi podaci ostaju u listi. U nasem primeru mi smo dodali novi *lisa* item ali jedan
stari ostao u memoriji. Zbog toga sto assoc vraca prvi item koji pronadje stari *lisa* item i dalje ostaje u history.


* NEGATIVNE STRANE: 
> However, alists do have one serious limitation: They are not a very effi-
> cient way to store and retrieve data, unless you’re dealing with very short lists
> (under a dozen items). Because of this inefficiency, although alists are often
> one of the first tools in the Lisp programmer’s toolbox, they may be replaced
> by other types of data structures as a program matures.


#### TREE-LIKE DATA


``` common-lisp
(defparameter *house* '((walls (mortar (cement)
                                       (water)
                                       (sand)
                               (bricks))
                        (windows (glass)
                                 (frame)
                                 (curtains))
                        (roof (shingles)
                              (chimeny)))))
```

> This data structure very elegantly captures the hierarchical nature of the
> parts that make up a house. Since it is structured as a Lisp syntax expression,
> we can see the lists that make up the levels of the hierarchy. Also, it follows
> the convention of a syntax expression by putting a symbol at the front of
> each list. For instance, we can see how the list describing the windows first
> contains the Lisp symbol windows (1), which is then followed by three items,
> representing the glass, frame, and finally the curtains (2)

#### VISUALAZING GRAPHS

``` common-lisp
(defparameter *wizard-nodes* '((living-room (you are in the living room. a wizard is snoring loudly on the couch.))
                               (garden (you are in a beatiful garden. there is a well in front of you.))
                               (attic (you are in the attic. there is a giant welding toch in the corner.))))

(defparameter *wizard-edges* '((living-room (garden west door)
                                     (attic upstairs ladder)
                               (garden (living-room east door))
                               (attic (living-room downstairs ladder)))))
```


#### KREIRANJE GRAPHWIZ PROGRAMA

``` common-lisp
(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))
```

* Convert node identifiers to valid DOT indentifiers
Node in DOT format can contain only letters, digits, and underscore.
Our methods changes all **forbidden** characters to underscore.

#### substitute-if
substitute-if function substitutes values based on the result of a **test** function.
The test function here is **digit-char-p**, tells us if a char in a string is numerical digit.


#### PREDICATES
> Test functions like this, which **accept a value** and
> **determine truth based on that value**, are often referred to as **predicates**.

``` common-lisp
(substitue-if #\e #'digit-char-p "I'm a l33t hack3r!")
;=> "I'm a leet hacker!"
```

#### oddp
Checks if item is odd or not

substitute-if can also be used on lists:
``` common-lisp
(substitute-if 0 #'oddp '(1 2 3 4 5 6 7 8))
;=> (0 2 0 4 0 6 0 8)
```


#### GENERIC FUNCTION
Function that can accept multiple datatypes as parameters and handle them appropriately.

#### alphanumericp
Predicate function that tells us if character is alphanumeric.

#### complement
Higher-order function that creates opposite (or complement)

> ;; Common Lisp - DOCS
> (complement f)
> Takes a fn f and returns a fn that takes the same arguments as f,
> has the same effects, if any, and returns the opposite truth value.



#### ADDING LABELS TO GRAPH NODES

The label will consist of the node name
and the data linked to the node in the node alist. But we also need to make
sure that we are not trying to put too much text in the label.

``` common-lisp
(defparameter *max-label-length* 30)

(defun dot-label (exp)
  if exp
    (let ((s (write-to-string exp :pretty nil)))
      (if (> (length s) *max-label-length*)
          (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
          s))
    ""))
```

* write-to-string - bez extra charactera slicno kao prin1-to-string
  * :pretty - keyword paramter value ovde je nil
* length - duzina stringa
* concatenate - spajanje stringa
* subseq - kreira sequence ogranicen start/ed
  (subseq sequence start &optional end)


> *max-label-length* (1) is a global variable that determines the maximum
> number of characters for the label. If a node label is larger than the limit (3), it
> gets cropped, and an ellipsis is added to indicate that fact (4). The write-to-
> string function (2) is similar to the prin1-to-string function we used earlier—it
> writes an expression to a string.

> The **:pretty** parameter is an example of a **keyword parameter**, which is used
> by certain Lisp functions to let you choose which parameters you want to pass
> in. In the case of write-to-string , it tells Lisp not to alter the string to make
> it pretty. Without this, Lisp would place new lines or tabs into our converted
> string to make it look more pleasing to the eye. **By setting the :pretty keyword
> parameter to nil , we are telling Lisp to output the expression without any
> decorations. **(Having new lines in a label can confuse Graphviz, so we don’t
> want to give Lisp any ideas.)


#### GENERATING DOT INFO FOR NODES

``` common-lisp
(defun nodes->dot (nodes)
  (mapc (lambda (node)
          (fresh-line)
          (princ (dot-name (car node)))
          (princ "[label=\"")
          (princ (dot-label node))
          (princ "\";"))
        nodes))
```

* princ      - direktno stampa output koji nemoze da se vrati u data format (kao sto moze kod print)
* mapc       - slicno sto i mapcar ali efikasnije jer ne vraca transformisanu listu
* lambda     - definicija anonimne funkcije
* fresh-line - samo nova linija

> This function uses **mapc** to go through every node in the list of nodes (1),
> and **princ** prints each node in the DOT format directly to the screen. **mapc** is a
> slightly more efficient variant of **mapcar**; the difference is that it does not return
> the transformed list. The **nodes->dot** function uses the **dot-name**(2) and **dot-label**(3)
> functions we created to convert the data.


#### CONVERTING EDGES INTO DOT FORMAT

``` common-lisp
(defun edges->dot (edges)
  (mapc (lambda (node)
          (mapc (lambda (edge)
                  (fresh-line)
                  (princ (dot-name (car node)))
                  (princ "->")
                  (princ (dot-name (car edge)))
                  (princ "[label=\"")
                  (princ (dot-label (cdr edge)))
                  (princ "\"];"))
                (cdr node)))
        edges))
```

#### GENERATING ALL DOT DATA

``` common-lisp
(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
    (princ "}"))
```

> This function ties everything together by defining our graph as a direc-
> tional graph (1), and then calling our nodes->dot(2) and edges->dot(3) functions.


#### GENERATE PNG OF GRAPH

``` common-lisp
(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
                   fname
                   :direction :output
                   :if-exists :supersede)
    (funcall thunk))
  (ext:shell (concatenate 'string "dot -Tpng -O " fname)))
```

* thunk - stream specijalan tip podataka
* with-open-file
  - *standard-output* (globalna dinamicka promenljiva)
  - fname
  - :direction :output (keyword sa argumentom)
  - :if-exists :supersede (keyword sa argumentom)
* funcall - ovo vrsi poziv funkcije / execute funkciju ?
* ext:shell - izvrsava shell komandu

#### Nullary functions / THUNK

> It is common in Lisp to create **small functions that have zero arguments**. These
> functions are officially called **nullary functions**. However, Lispers will often
> create such functions in order to describe a *computation that they don’t want
> to run until later*. In this scenario, a function without arguments is com-
> monly called a **thunk** or a **suspension**.

> **Why is a thunk useful in our dot->png function?**
> Remember that the easiest way for us to write and debug graph->dot and other DOT file functions is to
> have them print their results directly to the console. When we call **graph->dot**,
> it doesn’t return its results as a **value**, but, instead, **prints** them at the console
> as a **side effect**. Therefore, we can’t just pass the value of graph->dot to dot->png .
> Instead, we pass in graph->dot as a **thunk**. Then dot->png is responsible for call-
> ing graph->dot , capturing the results, and sending them to a file.

> Since it is so common to generate textual data with a computer program,
> this particular technique is used a lot in Lisp code: First, we print stuff right
> to the console; next, we wrap it in a thunk; finally, we redirect the results to
> some other location.

#### STREAMS
Detaljnije objasnjenje with-open-file i streamova sa kojima ova funkcija radi.

``` common-lisp
(with-open-file (my-stream
                 "testfile.txt"
                 :direction :output
                 :if-exists :supersede)
  (princ "Hello File!" my-stream))
```

Primer koji objasnjava da je stream dostupan u okviru with-open-file nalik na varijablu u let.

``` common-lisp
(with-open-file (my-stream ...)
  ...body has my-stream defined..)

(let ((my-variable ...))
  ...body has my-variable defined..)
```

> So if we pass the name my-stream in at the front of the first list to with-open-
> file (1), this is analogous to defining my-variable at the start of a let (3). A vari-
> able named my-stream will be available to us in the body of with-open-file(2) , in
> the same way that my-variable will be available to us in the body of the let(4) .

Ukratko stream je datatype koji moze da se poveze sa fajlom i mozemo da mu passujemo funkciju (princ)
koja moze da upisuje podatke u stream (povezan fajl).


#### WITH-OPEN-FILE
Ima dosta :keyword parametara.

:direction :output (Write file a ne read)
:if-exits :superseded (ukoliko vec postoji fajl unisti prethodnu verziju)

``` common-lisp
(with-open-file (stream filespec options*) declaration* form*)
;=> results

;stream -- a variable.
;filespec---a pathname designator.
;options -- forms; evaluated.
;declaration---a declare expression; not evaluated.
;forms---an implicit progn. (ovo je funcall thunk kod nas)
;results---the values returned by the forms.
```

#### KEYWORD SYMBOL
Simboli se u lispu mogu da refer-uju na nesto drugo. Zbog toga postoje i **keyword symbol**
koji se ponasa kao neka vrsta konstante koja uvek refer-uje na samu sebe.

On ima neke benefite za kompajler u vidu optimizacije, ali je najkorisniji za smanjenje gresaka u kodu.

``` common-lisp
(let ((cigar 5))
   cigar)
;=> 5

:cigar
(let ((:cigar 5))
  :cigar)
;=> Error
```

> As you can see, the keyword symbol :cigar can be evaluated right at the
> REPL and already has a value (1). Its value is, conveniently, :cigar . If we try to
> redefine :cigar to something else, Common Lisp won’t let us (2).

#### CAPTURING CONSOLE OUTPUT

``` common-lisp
(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
                   fname
                   :direction :output
                   :if-exists :supersede)
    (funcall thunk))
  (ext:shell (concatenate 'string "dot -Tpng -O " fname)))
```

*standard-output* je specijalna globalna promenljiva (dinamicka)

> Remember that the let command usually creates a **lexical, or local, variable**.
> As we’ve discussed, the stream variable created by with-open-file is analogous
> to using let to create a variable. Hence, it usually leads to the creation of a
> lexical stream variable for us.

> However, if a **dynamic variable already exists with the same name**, let will
> instead, temporarily, **override** the value of the dynamic variable to the new
> value. *standard-output* is such a dynamic variable. This means that we can
> temporarily override the value of *standard-output* to a new value by passing it
> into our with-open-file command (1).

> In the body of the with-open-file, where we call our thunk (2), any values
> printed to the console will now be automagically routed to our file, instead.
> The surprising thing (enabled by the design of lexical and dynamic variables
> in Common Lisp) is that this is also true for the princ statements in our graph->dot 
> function, even though they are called indirectly from dot->png .


#### CREATING A PICTURE
graph->png funkcija koja povezuje sve.
    
``` common-lisp
(defun graph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (graph->dot nodes edges))))
```

> This function takes the name of a DOT file (as the variable **fname** ), as well
> as the graph’s **nodes and edges**(1) , and uses them to generate the graph. To
> do this, it calls **dot->png**(2) and creates the appropriate **thunk** -—a lambda func-
> tion (3). As is usual for a thunk, it takes no parameters.

> The graph->dot function is called inside the thunk (4) as a **delayed computation**.
> Specifically, if we had called graph->dot directly, its output would **just show up
> in the console**. However, when inside the thunk, it will be called at the leisure 
> of the dot->png function, and the output will be used to generate the
> DOT file with the filename passed in as the first parameter to graph->png .

Uglavnom mora da se upakuje u thunk (nullary function) inace ce se printati direktno na ekran.
Kada je upakovano u thunk onda se izvrsavanje odredjuje od strane dot->png funkcije.


#### UNDIRECTED GRAPH
Graph koji ima strelice na svojim edges se zove **directed graph**.
**Undirected graph** nema strelice i omogucava da se putuje u oba pravca.


``` common-lisp
(defun uedges->dot (edges)
  (maplist (lambda (lst) ; 1
             (mapc (lambda (edge)
                     (unless (assoc (car edge) (cdr lst)) ; 2
                       (fresh-line)
                       (princ (dot-name (caar lst)))
                       (princ "--")
                       (princ (dot-name (car edge)))
                       (princ "[label=\"")
                       (princ (dot-label (cdr edge)))
                       (princ "\";")))
                   (cdar lst)))
           edges))

(defun ugraph->dot (nodes edges)
  (princ "graph{") ; 4
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

(defun ugraph->png (fname nodes edges) ; 5
  (dot->png fname
            (lambda ()
              (ugraph->dot nodes edges))))
```

* maplist - isto sto i mapcar ali funkcija prima ceo ostatak liste a ne samo current item npr:
``` common-lisp
(mapcar #'print '(a b c))
A
B
C

(maplist #'print '(a b c))
(A B C)
(B C)
(C)
```

> The **maplist** function sends the print function everything in the list from
> the current item until the end. uedges->dot(1) then uses the information about
> future nodes it gets from maplist to *check whether the destination of the node
> appears later in the edge list*. The actual checking is done with the **assoc** func-
> tion, *looking for the current edge in the list of remaining edges*, calculated
> as (cdr lst) (2). In this case, *it skips the edge so that only one of any pair of
> edges will be printed.*

> The ugraph->dot function is similar to the graph->dot function, except
> that it describes the graph as just a graph (4) when generating the DOT data,
> instead of a digraph. 

> The ugraph->png (5)function is essentially identical to the
> graph->png function, except that it calls ugraph->dot instead of graph->dot .



-------------------------------------------------------------------------------


### CHAPTER 8 - GRAND THEFT WUMPUS

Igra koja koristi nas graph kod.


``` common-lisp
(load "graph") ;; graph.lisp

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)
```

> The map of Congestion City will be an *undirected graph* with data associated
> with each node stored in the variable ***congestion-city-nodes*** . The possible data
> at each node will include the presence of the *Wumpus, a Glowworm team,
> and various danger signs*.

> A set of edges stored in ***congestion-city-edges*** will connect the nodes, and
> data linked to these edges will alert us to the presence of any *police road-blocks*.

---


``` common-lisp
(defun random-node()
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  (unless (eql a b)
    (list (cons a b) (cons b a))))

(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num*
                        collect (edge-pair (random-node) (random-node)))))
```

* random - vraca random broj manji od parametra
* apply - primenjuje funkciju na arg/listu ovde append + loop i poziva funkciju nad svakim elementom
* collect - pretpostavljam da pravi jednu listu od parametara, sve gura u jednu listu iz CELE petlje (vidi se u repl)
* 1+  - uvecava rezultat za 1
* loop - petlja
  - repeat - ponavlja petlju za paramtar (*edge-num* = 45 puta)
* cons - ovde oni *generisu pairs / dotted lists* (cons 1 2) => (1 . 2)

> First, we declare the **random-node** function (1), which returns a random
> node identifier. It uses the *random* function, which returns a random natural
> number less than the integer you pass to it. Since we’re going to be showing
> the node identifiers in our user interface, we use the 1+ function to number
> our nodes 1 through 30 (the upper limit because the *node-num* variable is set
> to 30 ), instead of 0 through 29.

> The **make-edge-list** function (3) generates the actual list of random edges.
> It uses the loop command to loop ***edge-num*** times(4) , and then collects the
> requisite number of edges (5) . We’ll take a closer look at the loop command in
> the next section. *The graph of the city is undirected*, so this function uses a
> helper function, **edge-pair** (2), to *create two directed edges between the randomly
> selected nodes.* This extra step makes sense once you remember that an
> undirected graph is the same as a directed graph, with two opposing directed
> edges mirroring each undirected edge. (When we build our edges into an
> alist later in this chapter, this step will ensure that the list is properly formed.)


#### LOOP
Objasnjeno par varijanti loop. Koristi se repeat ili for za counter.
collect sakuplja svaki prolaz kroz petlju u listu

``` common-lisp
(loop repeat 10
    collect 1)
;=> (1  1 1 1 1 1 1 1 1 1)

(loop for n from 1 to 10
    collect n)
;=> (1 2 3 4 5 6 7 8 9 10)

(loop for n from 1 to 10
    collect (+ 100 n))
;=> (101 102 103 104 105 106 107 ...)
```

#### PREVENTING ISLANDS
Kod koji sprecava da se prilikom random generisanja node/edge se naprave mesta koja nisu povezana ni sa jednim drugim mestom.


``` common-lisp
(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x)
                   (eql (car x) node))
                 edge-list))

(defun get-connected (node edge-list)
  (let ((visited nil))
    (labels ((traverse (node)
               (unless (member node visited)
                 (push node visited)
                 (mapc (lambda (edge)
                         (traverse (cdr edge)))
                       (direct-edges node edge-list)))))
      (traverse node))
    visited))

(defun find-islands (nodes edge-list)
  (let ((islands nil))
    (labels ((find-island (nodes)
               (let* ((connected (get-connected (car nodes) edge-list))
                      (unconnected (set-difference nodes connected)))
                 (push connected islands)
                 (when unconnected
                   (find-islands unconnected)))))
      (find-island nodes))
    islands))

(defun connect-with-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands (caadr islands))
                       (connect-with-bridges (cdr islands))))))

(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))
```

* remove-if-not - vraca sequence iz kojeg su uklonjeni svi elementi koji prolaze test (remove-if-not test items)
  postojijos i remove, remove-if, delete/if/if-not
* labels -  lokalno definisanje funkcije (kao flet) ali dozvoljava rekurziju
* push - ubacuje novi elemenat na pocetak liste
* traverse - ovako se zove nasa lokalna funkcija
* member - proverava da li je item clan liste (member item list)
* set-difference - uzima 2 liste i vraca sve one iteme koji se nalaze samo u prvoj
* let* - isto sto i let ali za vise promenljivih/funkcija

> First, we declare a utility function called **direct-edges** (1), which finds all
> the edges in an edge list that start from a given node. It does this by creating
> a new list with all edges removed (using remove-if-not (2)) that don’t have the
> current node in the car position.

> To find islands, we write the **get-connected** function (3). This function takes
> an edge list and a source node and builds a list of all nodes connected to that
> node, even if it requires walking across multiple edges.

> The usual way to find connected nodes is to start a *visited* list (4), and
> then perform a search along connected nodes, starting with the source node.
> Newly found nodes are added to the visited list with the push command (5).
> We also traverse all the children of this found node, using mapc (6).

> If, on the other hand, we encounter a node that has already been visited,
> we know we can ignore it. Once the search is complete, the visited list will
> consist of all connected nodes.

> Now that we have a function for finding nodes that are connected, we
> can use it to create a function that will find all the islands in our graph. The
> find-islands function first defines a local function, called **find-island** (7). This
> function checks which nodes are connected to the first node in our list of
> nodes using the **connected** function. It then subtracts these nodes from the full
> list of nodes using the set-difference function. ( **set-difference** takes two lists,
> and returns all items that are in the first list but not the second.)

> Any remaining nodes are deemed unconnected. If any unconnected
> node exists (8), we call the **find-islands** function again **recursively** to find addi-
> tional islands.

> Once we’ve found all the islands, we need a way of bridging them
> together. This is the job of the **connect-with-bridges** function. It returns a list of
> additional edges that join all the islands together. *To do this, it takes the list
> of islands and checks if there is a cdr in this list . If there is, it means there
> are at least two land masses, which can be connected with a bridge.* It uses the
> **edge-pair** function to create this bridge, and then calls **itself recursively** on the
> tail of the island list, in case additional bridges are needed.

> Finally, we tie all of our island prevention functions together using the
> function **connect-all-islands** (10). It uses **find-islands** to find all the land masses,
> and then calls **connect-with-bridges** to build appropriate bridges. It then **appends**
> these bridges to the initial list of edges to produce a final, fully connected
> land mass.


#### FINAL EDGE


``` common-lisp
(defun make-city-edges ()
  (let* ((nodes (loop for i from 1 to *node-num*
                      collect i))
         (edge-list (connect-all-islands nodes (make-edge-list)))
         (cops (remove-if-not (lambda (x)
                                (zerop (random *cop-odds*)))
                              edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))
```

* let* - visestruka definicija, omogucava koriscenje prethodno definisanih vars
* loop - petlja
  - collect - sve iz petlje se sakuplja u listu
* zerop - vraca true ako je broj 0
* random - vraca random od parametra

> First, the **make-city-edges** function creates a list of nodes, using a loop (1). (This
> is simply a list of numbers from 1 to *node-num* .) Next, it creates a random
> (but fully connected) edge list by calling the **make-edge-list and connect-edge-
> list** functions . This result is stored in the *edge-list variable*. It then creates
> a random list of edges that contains cops (3). We define these variables with
> the **let\*** command, which *allows us to reference previously defined variables*.

> For our purposes,using let* allows our definition of cops to contain a reference to edge-list .

> Once we’ve created the edge list and determined where the cops are, we
> need to convert our edge list into an **alist** and add the cops to it . The edges
> are converted to an alist with the **edges-to-alist** function, and the cops are
> added with the **add-cops** function.


#### LET VS LET*
Let ne omogucava pristup prethodno definisanoj varijabli.

``` common-lisp
(let ((a 5)
      (b (+ a 2)))
  b)

;=> EVAL: variable A has no value

(let* ((a 5)
       (b (+ a 2)))
  b)
;=> 7
```


#### EDGES-TO-ALIST

``` common-lisp
(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1)
            (cons node1
                  (mapcar (lambda (edge)
                            (list (cdr edge)))
                          (remove-duplicates (direct-edges node1 edge-list)
                                             :test #'equal))))
          (remove-duplicates (mapcar #'car edge-list))))
```

* cons - kreiranje cons cellova
* mapcar - izvrsava funkciju nad listom i cuva rezultate izmena
* remove-duplicates - uklanja duplikate iz liste koristeci test
  - :test keyword parametar omogucava podesavanje test uslova
  

#### Primer za 3 grada:

1 <==> 2 <==> 3

List: `'((1 . 2) (2 . 1) (2 . 3) (3 . 2))`
Alist: `'((1 (2)) (2 (1) (3)) (3 (2)))`

> To build this *alist*, the **edges-to-list** function *first mapcar* s (5) over the
> nodes found in the edge list. To build the list of nodes, we use the **remove-duplicates** 
> function, which removes duplicate items from a list. By default,
> remove-duplicates uses the **eql** function to check for equality, though it also
> allows you to choose a different test function using the **:test keyword
> parameter**. Since we’re checking for equality of cons pairs in our **make-city-edges** 
> function, we set :test to #'equal (7).

> Within this *outer mapcar* , we use another mapcar to map across all the
> *direct-edges* to this node. Together, these nested mapcar functions allow **edges-to-alist** 
> to convert the edges of a city into an **alist**.


#### ADD-COPS

``` common-lisp
(defun add-cops (edge-alist edges-with-cops)
  (mapcar (lambda (x)
            (let ((node1 (car x))
                  (node1-edges (cdr x)))
              (cons node1
                    (mapcar (lambda (edge)
                              (let ((node2 (car edge)))
                                (if (intersection (edge-pair node1 node2)
                                                edges-with-cops
                                                :test #'equal)
                                  (list node2 'cops)
                                edge)))
                            node1-edges))))
            edge-alist))
```

* intersection - vraca listu koja sadrzi svaki elemenat koji se pojavljuje u oba parametra (liste)
  Ukoliko se doda keyword parametar :test vrsi se provera u ovom slucaju se poziva equal f().
  (intersection list1 list2 :test #'equal)

> When we wrote the **make-city-edges** function, we randomly marked some of
> the edges to show that they have cops on them (4). We are now going to use
> this list of cop edges to mark the edges in our alist that contain cops. This is
> the job of the **add-cops** function.

> To do this, we use *nested mapcar commands* to map across the edges within
> each node (8) (9). We then check whether there are any cops on a given edge,
> using the **intersection** function . (The intersection function tells us which
> items are shared between two lists.)

Primer generisane liste add-cops:
`((1 (2) (2 (1) (3 COPS)) (3 (2 COPS))))`

;; ???
(cdr (assoc node1 edges))                     => za prvi u listi 1 = (2)
(cdr (assoc node2 (cdr (assoc node1 edges)))) => za drugi u list 2 = (1) (3 COPS) > (3 COPS) > COPS


> This is actually a **nested alist**. The outer alist is organized based on the first
> node (node1), and the inner alists are organized based on the second node (node2).

> With the edges in this format, we can easily find all edges connected to a
> given node by calling **(cdr (assoc node1 edges))** . To see if a given edge contains
> cops, we can call **(cdr (assoc node2 (cdr (assoc node1 edges))))** , which goes down
> two levels to grab the actual data linked to a specific edge between two nodes.
> (One additional benefit of using this nested alist format is that it is fully com-
> patible with our graph libraries—a feature that we’ll take advantage of shortly.)


#### INIT GAME

``` common-lisp
(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (sef *player-pos* (find-empty-node))
  (sef *visited-nodes* (list *player-pos*))
  (draw-city))
```

* setf - promeni vrednost globalne var


Ovde je nova jedino **find-empty-node** funkcija koja pronalazi prazno mesto za igraca.


``` common-lisp
(defun find-empty-node ()
  (let ((x (random-node)))
    (if (cdr (assoc x *congestion-city-nodes*))
        (find-empty-node)
        x)))
```
Lep primer funkcionalnog resenja. Prvo definisemo **x** koji je rezultat poziva **random-node** funkcije.
Ta funkcija vraca random broj koji se nakon toga koristi u *if*. Ukoliko postoji key u toj **alist** (congestion-city-nodes) onda znaci da moramo da pozovemo f() ponovo (rekurzija). Ukoliko ne onda smo pronasli prazan node i vracamo x.

#### DRAW CITY

``` common-lisp
(defun known-city-nodes ()
  (mapcar (lambda (node)
            (if (member node *visited-nodes*)
                (let ((n (assoc node  *congestion-city-nodes*)))
                  (if (eql node *player-pos*)
                      (append n '(*))
                      n)
                  (list node '?)))
          (remove-duplicates
            (append *visited-nodes*
                    (mapcan (lambda (node)
                              (mapcar #'car
                                      (cdr (assoc node
                                                  *congestion-city-edges*))))
                            *visited-nodes*))))))
```
* mapcan - varijanta na mapcar koja podrazumeva da ce se vrednosti generisano od strane funkcije biti listi koje ce se appendovati zajedno

> At the bottom of **known-city-nodes** , we need to figure out which nodes we
> can “see” based on where we’ve been. We’ll be able to see all *visited nodes* (5),
> but we also want to track all nodes within one node of a visited node (6). (We
> will discuss the *mapcan* function shortly.) We calculate who is “within one” using
> code similar to the previously discussed *within-one* function.

> Next, we mapcar over this list of relevant nodes, processing each (1). If the
> current node is occupied by the player, we mark it with an asterisk (3). If the
> node hasn’t been visited yet(2) , we mark it with a question mark (4).


#### CITY EDGES

``` common-lisp
(defun know-city-edges ()
  (mapcar (lambda (node)
            (cons node (mapcar (lambda (x)
                                 (if (member (car x) *visited-nodes*)
                                     x
                                     (list (car x))))
                               (cdr (assoc node *congestion-city-edges*)))))
          *visited-nodes*))
```

> This function is similar to the known-city-nodes function. The noteworthy
> line of code is here (1) where we strip the cdr from the edge list for edges so
> that cops are indicated on the map only if we’ve visited the nodes on both
> ends of an edge containing cops.


#### MAPCAN

> However, unlike mapcar, mapcan assumes that the values generated by the mapping func-
> tion are all lists that should be appended together. This is useful when there
> isn’t a one-to-one relationship between the items in a list and the result you
> want to generate.

Ukratko sve trpa u jednu listu

``` common-lisp
(defun ingredients (order)
  (mapcan (lambda (burger)
            (case burger
              (single '(patty))
              (double '(patty patty))
              (double-cheese '(patty patty cheese))))
          order))
          
;; (ingredients '(single double-cheese double))
;;=> (PATTY PATTY PATTY CHEESE PATTY PATTY)

;; Da je bio mapcar vratio bi:
;;=> (( PATTY ) ( PATTY PATTY CHEESE ) ( PATTY PATTY ))
```

* case - slicno kao i u drugim jezicima



#### DRAW KNOWN CITY

``` common-lisp
(defun draw-known-city ()
  (ugraph->png "known-city" (know-city-nodes) (know-city-edges)))
```

#### WALKING AROUND TOWN

``` common-lisp
(defun walk (pos)
  (handle-direction pos nil))

(defun charge (pos)
  (handle-direction pos t))
```

> We’ll need two functions for traveling between the nodes in our city: a regular
> walk function and one for when we think we’ve found the Wumpus, and we
> want to charge that location with our final bullet. Since these two functions
> are very similar, we’ll have both of them delegate the bulk of the work to a
> common handle-direction function


``` common-lisp
(defun handle-direction (pos charging)
  (let ((edge (assoc pos
                     (cdr (assoc *player-pos* *congestion-city-edges*)))))
    (if edge
        (handle-new-place edge pos charging)
        (princ "That location does not exist!"))))
```

> First, this function looks up the legal directions players can move to from
> their current location (1). It then uses the pos the player wants to move to and
> looks it up in that list of possible directions. Once we’ve determined that a
> direction is legal (that is, a node with that number shares an edge with the
> player’s current position), we need to find out what surprises are waiting as
> the player travels to this new place, using the handle-new-place function, which
> we’ll create next . Otherwise, we display a helpful error message .


``` common-lisp
(defun handle-new-place (edge pos charging)
  (let* ((node (assoc pos *congestion-city-nodes*))
         (has-worm (and (member 'glow-worm node)
                        (not (member pos *visited-nodes*)))))
    (pushnew pos *visited-nodes*)
    (setf *player-pos* pos)
    (draw-know-city)
    (cond ((member 'cops edge) (princ "You ran into the cops. Game Over."))
          ((member 'wumpus node) (if charing
                                     (princ "You found the Wumpus!")
                                     (princ "You ran into the Wumpus")))
          (charing (princ "You wasted your last bullet. Game Over."))
          (has-worm (let ((new-pos (random-node)))
                      (princ "You ran into the Glow Worm Gang! You're now at ")
                      (princ new-pos)
                      (handle-new-place nil new-pos nil))))))
```

> First, we retrieve the node the player is traveling to from the alist of nodes (1).
> Next, we figure out if the node contains a Glowworm gang (2). We ignore the
> gang if they’re in a node already visited, because they’ll only attack once.

> Next, the handle-new-place function updates *visited-nodes* (3)(adding the
> new position to the list) and *player-pos* (4). Then it calls draw-known-city (5)
> again, since we now have a new place we know about.

> Next, it checks to see if there are any cops on the edge (6), and then whether
> the Wumpus is at that location (7). If the player encounters the Wumpus, our
> handle-new-place function needs to know whether we were charging the loca-
> tion. If we are charging at the Wumpus, we win the game. Otherwise, the
> Wumpus kills us and the game is over.

> If, on the other hand, we charge at a location that does not contain the
> Wumpus, we waste our single bullet and we lose the game as well . Finally,
> if the location has a previously unencountered Glowworm gang, jump to a
> random new location, calling handle-new-place recursively (9).



-------------------------------------------------------------------------------


## CHAPTER 9 ADVANCED DATATYPES AND GENERIC PROGRAMMING


#### ARRAYS

> The Common Lisp array is very similar to a list. The main advantage of using
> arrays is that they require only a constant amount of time to access a value at
> any specific location.


#### CREATE ARRAY
Funkcija za kreiranje novog array je **make-array**. Ona kreira array sa length paramtra.
Hash # je dodat na vrednost da bi oznacio da je u pitanju array a ne lista.

``` common-lisp
(make-array 3)
;=> #(NIL NIL NIL)
```

#### GET/SET ARRAY

Funkcija **aref** se koristi za dodavanje i za citanje vrednosti array. (get/set)

``` common-lisp
(defparameter x (make-array 3))
;=> #(NIL NIL NIL)

(setf (aref x 1) 'foo)
;=> FOO

x
;=> #(NIL FOO NIL)

(aref x 1)
;=> FOO
```

> This ability to use the setf and aref commands together
> shows off a feature in Common Lisp: its support for **generic programming**.


#### GENERIC SETTERS

Ovo znaci da je kod za get-ovanje vrednosti (array, list, string itd..) isti kao i kod za ubacivanje vrednosti.
(Primer sa setf + aref)

``` common-lisp
(setf foo '(a b c))
;=> (A B C)

(second foo)
;=> B

(setf (second foo) 'z)
;=> Z

foo
;=> (A Z C)
```

> As you would expect, the expression *(second foo)* returns B . But, when we
> pass *(second foo)* to the **setf** command (1), it somehow knows where the B
> came from, and it is able to treat the expression (second foo) as if it were a
> *regular variable*. Basically, the setf command asks itself the question, “Where
> did the item in my first argument originally come from?” In this case, the
> value came from the second item in the list named foo . Therefore, if we try to
> setf this location, the source variable, foo , is modified in response.


#### GENERALIZED REFERENCE

Special sublanguage of Common Lisp - generalized reference. Not every Lisp commnd is allowed
in a generalized reference.

Fali tu vise objasnjenja o konceptu generalized reference.

``` common-lisp
(setf foo (make-array 4))
;=> #(NIL NIL NIL NIL)

(setf (aref foo 2) '(x y z))
;=> (X Y Z)

foo
;=> #(NIL NIL (X Y Z) NIL)

(setf (car (aref foo 2)) (make-hash-table))
;=> #S(HASH-TABLE)

(setf (gethash 'zoink (car (aref foo 2))) 5)
;=> 5

foo
;=> #(NIL NIL (#S (HASH-TABLE (ZOINK . 5)) Y Z) NIL)
```

> **This example demonstrates the true power of setf in Common Lisp.** In
> the first use, we put the **list (x y z)** into an **array** as the **third item** . If we
> now print foo , we can see that it worked . In the second use, we replace the
> **first item** in this list inside the foo array with a **hash table** . Hash tables are
> another advanced data type we’ll be learning about shortly, on page 157. It is
> surprisingly easy to do this with setf , because the **generalized reference** in
> the first argument to setf can be arbitrarily complicated.
> Finally, we go all out and insert the value 5 into this hash table with the key
> of **zoink** . The **gethash** function lets you get a value out of a hash table, as
> we’ll see shortly. Here, with the help of setf , we are putting the number 5
> into the hash table instead.


#### ARRAYS VS LISTS

Skoro sve sto moze da se uradi listama moze da se uradi i putem array. 
Razlika je u perfomansama - kada se pristupa tacno odredjenom elementu.

``` common-lisp
(nth 1 '(foo bar baz))
;=> BAR
```

NTH - komanda koja ucitava odredjeni element iz liste

> However, it makes sense to use the nth function only with very small lists.
> If, for example, list X had thousands of items in it, running the command
> (nth 1000 x) would be excruciatingly slow, because Lisp lists are made out of
> chains of cons cells. Hence, the only way Lisp can find the thousandth item
> in a list is to churn through the 999 preceding objects first.

Array moze da pristupi direktno odredjenom itemu bez listanja svih prethodnih.


#### HASH TABLES
Hash tables slicno sto i aslists, osim sto takodje imaju bolje perfomanse kad se pristupa odredjenom elementu.

#### CREATE HASH TABLES
FUnkcija **make-hash-table**. Ucitaj vrednost hash-tabele **gethash**.

``` common-lisp
(make-hash-table)
;=> #S(HASH_TABLE ...)
```

``` common-lisp
(defparameter x (make-hash-table))
;=> #S(HASH-TABLE ...)

(gethash 'yup x)
;=> NIL ;
;=> NIL
```

> So far, our hash table remains empty. This means that when we look up
> any key in the hash table, such as 'yup in this example, we receive NIL as an
> answer (1). Actually, we receive two NIL s (1) (2)— the gethash command **returns mul-
> tiple values, which you can do in Common Lisp** (discussed in the next section).
> The *first returned value is the actual value stored in the hash table*, and the
> *second indicates whether the key was found in the table* (in this case, it wasn’t)


``` common-lisp
(defparameter x (make-hash-table))
;=> #S(HASH-TABLE ...)

(setf (gethash 'yup x) '25')
;=> 25

(gethash 'yup x)
;=> 25 ;
;=> T
```

Rewritovan primer iz ranijih glava (tamo se koristio alist):

``` common-lisp
(defparameter *drink-order* (make-hash-table))
;=> #S(HASH-TABLE ...)

(setf (gethash 'bill *drink-order*) 'double-espresso)
;=> DOUBLE-ESPRESSO
(setf (gethash 'lisa *drink-order*) 'small-drip-coffee)
;=> SMALL-DRIP-COFFEE
(setf (gethash 'john *drink-order*) 'medium-latte)
;=> MEDIUM-LATTE

(gethash 'lisa *drink-order*)
;=> 'small-drip-coffee ;
;=> T
```

#### RETURNING MULTIPLE VALUES
Ovo je dozvoljeno i korisceno u Common Lispu.

``` common-lisp
(round 2.4)
;=> 2 ;
;=> 0.4
```

Samostalno kreiranje funkcije koja vraca multiple return values.

``` common-lisp
(defun foo ()
  (values 3 7))
  
(foo)
;=> 3 ;
;=> 7
```

Lisp vraca obe vrednosti ali smatra PRVU vaznijom i ona ce se koristiti u kalkulacijama. (po defaultu)

``` common-lisp
(+ (foo) 5)
;=> 8 ;; 3 + 5
```

Nekada je potrebno da koristimo i dodatnu return value (2nd). Za to se koristi **multiple-value-bind**.

``` common-lisp
(multiple-value-bind (a b) (foo)
                     (* a b))
```

U sustini umesto ovoga je jednostavno moguce vratiti listu. Noviji dijalkti kao sto su ARC i CLOJURE ovu 
funkcionalnost ne podrzavaju. Umesto toga vracaju liste. Postoje neki specificni slucajevi kada je Common Lisp resenje dobro.




#### STRUCTURE

Advanced datatype.

Strukture se mogu koristiti da predstave objekat sa properties (slicno kao u OOP) **defstruct**

``` common-lisp
(defstruct person
           name
           age
           waist-size
           favorite-color)
;=> PERSON
```

Properties se u LISP jos zovu **SLOTS**.

Kreiramo novu instancu uz upotrebu **make-imestructure**.

``` common-lisp
(defparamter *bob* (make-person :name "Bob"
                                :age 35
                                :waist-size 32
                                :favorite-color "blue"))
;=> *BOB*


*bob*
;=> #S(PERSON :NAME "Bob" :AGE 35 :WAIST-SIZE 32 :FAVORITE-COLOR "blue")
```

Pozivanje properties se vrsi uz pomoc auto-generisanih funkcija:
``` common-lisp
(person-age *bob*)
;=> 36
```

Promena vrednosti properity:
``` common-lisp
(setf (person-age *bob*) 39)
;=> 39
```

The Lisp reader can also create a person directly from the printed repre-
sentation of the person, another great example of the print/read symmetry
in Lisp:

``` common-lisp
(defparameter *that-guy* #S(person :name "Bob" :age 35 :waist-size 32 :favorite-color "blue"))

(person-age *that-guy*)
;=> 35
```

> Here, we’re creating a new variable called *that-guy* , and we set its value
> using **only the printed representation**  #S(person ...)  of the person (1). This variable now has
> a real person structure in it, just as if we had used the **make-person** function (2).


#### WHEN TO USE STRUCTURES

``` common-lisp
(defun make-person (name age waist-size favorite-color)
    (list name age waist-size favorite-color))
MAKE-PERSON

(defun person-age (person)
    (cadr person))
PERSON-AGE

(defparameter *bob* (make-person "bob" 35 32 "blue"))
*BOB*

*bob*
("bob" 35 32 "blue")

(person-age *bob*)
35
```

> Although this approach will work, it has several downsides. First, in order
> to check a person’s age or other properties, we would need to write a lot of
> error-prone functions that pull properties out of the list from the correct
> locations. Also, the printed version of our ad hoc object is very hard to
> understand. How do we know BOB is a person? Is Bob’s age 35 or 32? Regular
> lists just don’t lend themselves well to encoding objects with multiple properties.

> Another problem with using lists to represent an object in the real world
> is that the properties of an object (like a person object) may change over time.
> Lists in Lisp work best when you are dealing with information that never changes
> once the list is created. When Bob turns 36, however, we need to change his
> age property.

> Having part of a data structure change over time is called a mutation by
> computer scientists. It’s easy to change the value of a specific property (mutate
> the property) in a structure created with defstruct , so these structures are
> very suitable for handling data that needs to be mutable. Therefore, it makes
> sense to store a person (or any other object that changes over time) in a structure


#### HANDLING DATA IN A GENERIC WAY
Zbog dosta razlicitih tipova podataka (data types) common lisp ima razne alate koji omogucavaju pisanje generickog koda (npr skladistimo podatke u liste i array oni se koriste da bi izbegli pisanje razlicitih funkcija za istu operaciju ali koje gadjaju razlicite tipove podataka)

* Generic library functions
* Type predicates
* defmethod
* generic accessor

#### SEQUENCE FUNCTIONS

> The easiest way to write code that works with any type of argument is to hand
> the type-checking work to someone else. The Common Lisp libraries are
> packed with functions that can generically handle data of varying types in their
> arguments, the most commonly used of which are the **sequence functions**. The
> sequence functions work generically across the three main ways of sequencing
> objects in Lisp: *lists, arrays, and strings*.

Jedan primer za sequence functions je **length**.

``` common-lisp
(length '(a b c))
3

(length "blub")
4

(length (make-array 5))
5
```

Jos primera:

* **find-if** - prva vrednost koja zadovoljava predikat
* **count** koliko se cesto odredjeni item pojavljuje u sekvenci
* **position** lokacija itema u sekvecni
* **some** / **every** govore da li neka / ili svaka vrednostu u sekvenci postuje odredjeni predikat

``` common-lisp
(find-if #'numberp '(a b 5 d))
5

(count #\s "mississippi")
4

(position #\4 "2kewl4skewl")
5

(some #'numberp '(a b 5 d))
T

(every #'numberp '(a b 5 d))
NIL
```

* numberp - proverava da li je vrednost broj


#### SEQUENCE FUNCTIONS FOR ITERATING ACCROSS SEQUENCE

#### REDUCE
Prolazi kroz sekvencu i sve dok je ne svede na 1 item. Primenjuje funkciju nad elementom i cuva rezultat koji
potom primenjuje na sledeci elemenat itd..

Prvi item se koristi kao inicijalna vrednost.

``` common-lisp
(reduce #'+ '(3 4 6 5 2))
20
```


Komplikovaniji primer:

``` common-lisp
(reduce (lambda (best item)
          (if (and (evenp item) (> item best))
              item
              best))
        '(7 4 6 5 2)
        :initial-value 0) ;; bez ovoga reduce vraca greskom 7
6
```

* evenp - proverava da li je item paran

> Our reduction function, which we pass to reduce to distill down our answer
> from the list, has *two arguments* (1). The *first argument* is the **best** value we’ve
> found so far—in other words, the *largest even number we’ve found so far*.
> The *second argument* is the *next number* from the list.

> Our reduce function needs to return as a result the new best number.
> Therefore, if the latest number is better than the previous best (2), we return
> it (3). Otherwise, we return the previous best (4).

> Remember that the first number in the list we’re reducing will be used as a
> starting value. If this is a problem, we can instead pass an explicit initial value to
> the reduce function by passing in a keyword parameter named :initial-value (5).


Primer za sabiranje raznih tipova - generic.

``` common-lisp
(defun sum (lst)
    (reduce #'+ lst))
    
(sum '(1 2 3))
6

(sum (make-array 5 :inital-contents '(1 2 3 4 5)))
15

(sum "blablabla")
Error ;; sabiranje ne radi na characters
```

- :initial-contents - polazna vrednost za array


#### MAP
U Lispu radi slicno kao i mapcar ali je genericka (moze da se koristi nad vise razlicitih data types a ne samo na listama).

``` common-lisp
(map 'list                              ;; type of return (list)
    (lambda (x)
        (if (eq x #\s)
            #\S
            x))
    "this is a string")

;; (#\t #\h #\i #\S #\ #\i #\S #\ #\a #\ #\S #\t #\r #\i #\n #\g)
```

#### SUBSEQ
Uzima sub-sequence iz vece sequence navodjenjem start - end tacaka.

``` common-lisp
(subseq "russia" 2 6)
;; "ssia"
```

#### SORT

``` common-lisp
(sort '(5 8 2 4 9 3 6) #'<)
;; (2 3 4 5 6 8 9)
```


#### CUSTOM GENERIC FUNCTIONS WITH TYPE PREDICATES
Builtin funkcije za proveru tipova podataka - **type predicates**

* numberp
* arrayp
* characterp
* consp
* functionp
* hash-table-p
* listp
* stringp
* symbolp

``` common-lisp
(defun add (a b)
   (cond ((and (numberp a) (numberp b)) (+ a b))
         ((and (listp a) (listp b)) (append a b))))
;ADD

(add 3 4)
;7

(add '(a b) '(c d))
;(A B C D)
```

Gornji primer nije nacin na koji LISP-ers resavaju problem:
Negativne strane:
* Single monolithic function for all types (radi samo za 2 tipa)
* Modifications required to accommodate new cases (moramo da radimo promenu kad god zelimo da podrzimo novi tip)
* Hard to understand
* Perfomance


#### DEFMETHOD
Resenje za gornje probleme. Nesto slicno kao i method overriding u Javi.
Multiple function definitions (same function / name) that handle different data types.

> The proper term for having a compiler/interpreter choose among different 
> versions of a function based on argument types is **type dispatching**.

``` common-lisp
(defmethod add ((a number) (b number))
    (+ a b))
    

(defmethod add ((a list) (b list))
    (append a b))
    
(add 3 4)
7

(add '(a b) '(c d))
(A B C D)
```


### ORC BATTLE GAME


``` common-lisp
(defparameter *player-health* nil)
(defparameter *player-agility* nil)
(defparameter *player-strength* nil)


(defparameter *monsters* nil)
(defparameter *monster-builders* nil)
(defparameter *monster-num* 12)

```

> We’ll want to track three player stats: **health, agility, and strength**. When a
> player’s health reaches zero, that player will die. Agility will control how many
> attacks a player can perform in a single round of battle, and strength will
> control the ferocity of the attacks. As the game progresses, each of these will
> change and affect gameplay and strategy in subtle ways.

> We’ll store our monsters in an array called *monsters* . This array will be
> heterogeneous, meaning it can contain different types of monsters, be they orcs,
> hydras, or anything else. We’ll create our monster types with defstruct . Of
> course, we still need to figure out how to handle each type in the list in a
> meaningful way—that’s where we’ll use Lisp’s generic features.

> We’ll also define a list of functions for building monsters that we’ll store
> in the variable *monster-builders* . As we write the code for each type of monster,
> we’ll create a function that builds a monster of each type. We’ll then push
> each of these monster builders onto this list. Having all the builder functions in
> this list will make it easy for us to create random monsters at will for our game.

> Finally, we’ll create the variable *monster-num* to control how many oppo-
> nents our knight must fight. Change this variable to increase (or decrease)
> the difficulty level of Orc Battle.


``` common-lisp
(defun orc-battle ()
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead)
    (princ "You have been killed. Game Over"))
  (when (monsters-dead)
    (princ "Congratulations! You have won the game")))
```

> At the top, we call the initialization functions for the monsters and the
> player (1). Then we start the main game loop (2). The game loop will keep
> running until either the player or the monsters are dead. We’ll print a game-
> ending message depending on whether the player (3) or monsters (4) died.


``` common-lisp
(defun game-loop ()
  (unless (or (player-dead) (monsters-dead))
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      (unless (monsters-dead)
        (show-monsters)
        (player-attack)))
    (fresh-line)
    (map 'list
        (lambda (m)
          (or (monster-dead m) (monster-attack m)))
        *monsters*)
    (game-loop)))
```

* dotimes - ponovi odredjen kod n puta (ovde 15)
(dotimes (i 3)
 (...))

> The game-loop function handles the repeated cycles of monster and player
> attacks. As long as both parties in the fight are still alive, the function will first
> show some information about the player in the REPL (1).

> Next, we allow the player to attack the monsters. The game-loop function
> uses the player’s agility to modulate how many attacks can be launched in a
> single round of battle, using some fudge factors to transform the agility to a
> small, appropriate number (2). When the game begins, the player will have
> three attacks per round. Later stages of battle could cause this number to
> drop to a single attack per round.

> The calculated agility factor for our player attack loop (2)is passed into
> the dotimes command, which takes a variable name and a number n, and runs
> a chunk of code n times:

> After the player has attacked, we allow the monsters to attack. We do this
> by iterating through our list of monsters with the map function (3). Every type
> of monster has a special monster-attack command, which we’ll call as long as
> the monster is still alive (4).

> Finally, the game-loop function calls itself recursively, so that the battle
> can continue until one side or the other has been vanquished (5).

#### PLAYER MANAGEMENT FUNCTIONS

``` common-lisp
(defun init-player ()
  (setf *player-health* 30)
  (setf *player-agility* 30)
  (setf *player-strength* 30))

(defun player-dead ()
  (<= *player-health* 0))

(defun show-player ()
  (fresh-line)
  (princ "You are a valiant knight with a health of ")
  (prince *player-health*)
  (princ ", an agility of ")
  (princ *player-agility*)
  (princ ", and a strength of ")
  (princ *player-strength*))

(defun player-attack ()
  (fresh-line)
  (princ "Attack style: [s]tab [d]ouble swing [r]oundhouse:")
  (case (read)
    (s (monster-hit (pick-monster)
                    (+ 2 (randval (ash *player-strength* -1)))))
    (d (let ((x (randval (truncate (/ *player-strenght* 6)))))
         (princ "Your double swing has a strenght of ")
         (princ x)
         (fresh-line)
         (monster-hit (pick-monster) x)
         (unless (monsters-dead)
           (monster-hit (pick-monster) x))))
    (otherwise (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
                 (unless (monster-dead)
                   (monster-hit (random-monster) 1))))))
```

* otherwise - default za case
* randval - custom funkcija
* truncate - deli broj sa divisorom vraca quotient i remainder (ima vise funkcija u ovoj vrsti)
 produce a quotient that has been truncated towards zero; that is, the quotient represents the mathematical integer of the same sign as the mathematical quotient, and that has the greatest integral magnitude not greater than that of the mathematical quotient.  (truncate .5) => 0, 0.5 ; (truncate 1) => 1, 0

> First, this function prints out some different types of attacks from which
> the player can choose . As you can see, the player is offered three possible
> attacks: a stab, a double swing, and a roundhouse swing. We read in the player’s
> selection, and then handle each type of attack in a case statement .

> The stab attack is the most ferocious attack and can be delivered against
> a single foe. Since a stab is performed against a single enemy, we will first
> call the pick-monster function to let the player choose whom to attack . The
> attack strength is calculated from the *player-strength* , using a random
> factor and some other little tweaks to generate a nice, but never too power-
> ful, attack strength . Once the player has chosen a monster to attack and
> the attack strength has been calculated, we call the monster-hit function to
> apply the attack .

> Unlike the stab attack, the double swing is weaker, but allows two enemies
> to be attacked at once. An additional benefit of the attack is that the knight
> can tell, as the swing begins, how strong it will be—information that can then
> be used to choose the best enemies to attack midswing. This extra feature of
> the double swing adds strategic depth to the game. Otherwise, the double-
> swing code is similar to the stab code, printing a message and allowing the
> player to choose whom to attack. In this case, however, two monsters can be
> chosen

> The final attack, the roundhouse swing, is a wild, chaotic attack that does
> not discriminate among the enemies. We run through a dotimes loop based
> on the player’s strength and then attack random foes multiple times. How-
> ever, each attack is very weak, with a strength of only 1



``` common-lisp
(defun randval (n)
  (1+ (random (max 1 n))))
```

> The randval function returns a random number from one to n, while mak-
> ing sure that no matter how small n is, at least the number 1 will be returned.


``` common-lisp
(defun random-monster ()
  (let ((m (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead m)
        (random-monster)
        m)))
```

> The random-monster function first picks a random monster out of the array
> of monsters and stores it in the variable m . Since we want to pick a living
> monster to attack, we recursively try the function again if we inadvertently
> picked a dead monster . Otherwise, we return the chosen monster


``` common-lisp
(defun pick-monster ()
  (fresh-line)
  (princ "Monster #:")
  (let ((x (read)))
    (if (not (and (integerp x) (>= x 1) (<= x *monster-num*)))
        (progn (princ "That is not valid monster number.")
               (pick-monster))
        (let ((m (aref *monsters* (1- x))))
          (if (monster-dead m)
              (progn (princ "That monster is already dead.")
                     (pick-monster))
              m)))))
```

* read - input korisnika
* integerp - provera da li je podatak tip int
* progn - if moze da primi samo jednu komandu uz progn omogucavamo vise

> In order to let the player pick a monster to attack, we first need to display
> a prompt (1) and read in the player’s choice (2). Then we need to make sure
> the player chose an integer that isn’t too big or too small (3). If this has hap-
> pened, we print a message and call pick-monster again to let the player choose
> again. Otherwise, we can safely place the chosen monster in the variable m (4).

> Another error the player could make is to attack a monster that is already
> dead. We check for this possibility next and, once again, allow the player to
> make another selection (5). Otherwise, the player has successfully made a
> choice, and we return the selected monster as a result (6).


#### MONSTER MANAGEMENT

``` common-lisp
(defun init-monsters ()
  (setf *monsters*
        (map 'vector
             (lambda (x)
               (funcall (nth (random (length *monster-builders*))
                             *monster-builders*)))
             (make-array *monster-num*))))
```

* funcall - applies function to args - u sustini zove funkciju nad nekim podacima - u ovom kontekstu ovo se primenjuje nad x parameterom lambda funkcije
* nth -  locates nth element of list (nth 0 '(foo bar baz)) => FOO
* make-array - napravi niz parametar je broj elemenata

> First, the init-monsters function builds an *empty array* to hold the mon-
> sters . Then it map s across this array to fill it up . In the lambda function, you
> can see how random monsters are created by **funcall** ing random functions in
> our list of monster builders .


``` common-lisp
(defun monster-dead (m)
  (<= (monster-health m) 0))

(defun monster-dead ()
  (every #'monster-dead *monsters*))
```

Helper funkcije za monstere.



``` common-lisp
(defun show-monsters ()
  (fresh-line)
  (princ "Your foes:")
  (let ((x 0))
    (map 'list
         (lambda (m)
           (fresh-line)
           (princ "   ")
           (princ (incf x))
           (princ ". ")
           (if (monster-dead m)
               (princ "**dead**")
               (progn (princ "(Health=")
                      (princ (monster-health m))
                      (princ ") ")
                      (monster-show m))))
         *monsters*)))
```

* incf - inkrementira varijablu

> Since our player will need to choose monsters with a number, we will
> maintain a count as we loop through monsters in our list, in the variable **x** .
> Then we map through our *monster list*, calling a lambda function on each mon-
> ster, which will print out some pretty text for each monster . We use our **x**
> variable to print out the *number for each monster* in our numbered list .
> As we do this, we use the **incf** function, which will increment **x** as we work
> through the list.

> For *dead monsters*, we won’t print much about them, just a message
> showing that they are dead . For *living monsters*, we call *generic monster
> functions*, calculating the health and generating the monster description
> in a specialized way for each different type of foe.



``` common-lisp
(defmethod monster-hit (m x)
  (decf (monster-health m) x)
  (if (monster-dead m)
      (progn (princ "You killed the ")
             (princ (type-of m))
             (princ "! "))
      (progn (princ "You hit the ")
             (princ (type-of m))
             (princ ", knocking off ")
             (princ x)
             (princ " health points! "))))
```

* defmethod - multiple function with same name but different parameters (overwriting)
* decf - slicno sto i setf ali oduzima vrednost od var
* type-of - pronadji tip bilo koje LISP value



``` common-lisp
(defmethod monster-show (m)
  (princ "A fierce ")
  (princ (type-of m)))

(defmethod monster-attack (m))
```

> The monster-attack function doesn’t actually do anything. This is because
> all our monster attacks will be so unique that there’s no point in defining a
> generic attack. This function is simply a placeholder.


#### WICKED ORC

``` common-lisp
(defstruct (orc (:include monster)) (club-level (randval 8)))
(push #'make-orc *monster-builders*)
```

> The orc is a simple foe. He can deliver a strong attack with his club, but other-
> wise he is pretty harmless. Every orc has a club with a unique attack level. Orcs
> are best ignored, unless there are orcs with an unusually powerful club attack
> that you want to cull from the herd at the beginning of a battle.

> *To create the orc, we define an orc datatype with defstruct . Here, we will
> use another advanced feature of defstruct to declare that the orc includes all
> the fields of monster .*

> *By including the fields from our monster type in our orc type, the orc will
> be able to inherit the fields that apply to all monsters, such as the health field.*
> This is similar to what you can accomplish in popular languages such as C++
> or Java by defining a generic class and then creating other, more specialized,
> classes that inherit from this generic class.

> Once the structure is declared, we push the **make-orc** function (**automati-
> cally generated by the defstruct** ) onto our list of *monster-builders* :


## NAPOMENA
> Notice how powerful this approach is. We can create as many new monster types as we
> want, yet we’ll never need to change our basic Orc Battle code. This is possible only in
> languages like Lisp, which are dynamically typed and support functions as first-class
> values. In statically typed programming languages, the main Orc Battle code would
> need some hardwired way of calling the constructor for each new type of monster. With
> first-class functions, we don’t need to worry about this.


``` common-lisp
(defmethod monster-show ((m orc))
  (princ "A wicked orc with a level ")
  (princ (orc-club-level m))
  (princ " club"))

(defmethod monster-attack((m orc))
  (let ((x (randval (orc-club-level m)))
        (princ "An orc swings his club at you and knocks off ")
        (princ x)
        (princ " of you health points. ")
        (decf *player-health* x))))
```

* orc-club-level - automatsski se generise

> The one unique thing about our orc type is that each orc has an orc-club-level field. 
> These orc-specific versions of monster-show and monster-attack take
> this field into account. In the monster-show function, we display this club level ,
> so that the player can gauge the danger posed by each orc.
> In the monster-attack function, we use the level of the club to decide how
> badly the player is hit by the club .


#### HYDRA

``` common-lisp
(defstruct (hydra (:include monster)))
(push #'make-hydra *monster-builders*)

(defmethod monster-show ((m hydra))
  (princ "A malicious hydra with ")
  (princ (monster-health m))
  (princ " heads."))

(defmethod monster-hit ((m hydra) x)
  (decf (monster-health m) x)
  (if (monster-dead m)
      (princ "The coprse of fully decapitated and decapacitated hydra falls to the floor!")
      (progn (princ "You lop off ")
             (princ x)
             (princ " of the hydra's heads! "))))

(defmethod monster-attack ((m hydra))
  (let ((x (randval (ash (monster-health m) -1))))
    (princ "A hydra attacks you with ")
    (princ x)
    (princ "of its heads! It also grows back one more head! ")
    (incf (monster-health m))
    (decf *player-health* x)))
```

* ash - arithmetic shift operation on the binary representation of integer (ash 16 -1) => 8, (ash 16 1) => 32
* incf -  inkrement
* decf - dekrement

> The code for handling the hydra is similar to the code for handling the
> orc. The main difference is that a hydra’s health also acts as a stand-in for the
> number of hydra heads. In other words, a hydra with three health points will
> have three heads, as well. Therefore, when we write our hydra-specific monster-
> show function, we use the monster’s health to print a pretty message about the
> number of heads on the hydra .

> Another difference between the orc and the hydra is that an orc doesn’t
> do anything particularly interesting when it is hit by the player. Because of
> this, we didn’t need to write a custom monster-hit function for the orc; the
> orc simply used the generic monster-hit function we created for a generic mon-
> ster .

> A hydra, on the other hand, does something interesting when it is hit: It
> loses heads! We therefore create a hydra-specific monster-hit function, where
> heads are removed with every blow, which amounts to lowering the hydra’s
> health . Also, we can now print a dramatic message about how the knight
> lopped off said heads .

> The hydra’s monster-attack function is again similar to that for the orc.
> The one interesting difference is that we increment the health with every
> attack, so that the hydra grows a new head every turn .



#### SLIME-MOLD

``` common-lisp
(defstruct (slime-mold (:include monster)) (sliminess (randval 5)))
(push #'make-slime-mold *monster-builders*)

(defmethod monster-show ((m slime-mold))
  (princ "A slime mold with a sliminess of ")
  (princ (slime-mold-sliminess m)))

(defmethod monster-attack ((m slime-mold))
  (let ((x (randval (slime-mold-sliminess m))))
    (princ "A slime mold wraps around your legs and decreases your agility by ")
    (princ x)
    (princ "! ")
    (decf *player-agility* x)
    (when (zerop (random 2))
      (princ "It also squirts in your face, taking away a health point! ")
      (decf *player-health*))))
```

> The monster-attack function for the slime mold must do some special
> things, which allow it to immobilize the player. First, it uses the slime mold’s
> sliminess (which is generated when each slime mold is built) to generate a
> random attack against the player, stored in the variable x . Unlike most other
> attacks in the game, this slime mold attack affects the agility of players, rather
> than their health .

> However, it would be pointless if the slime mold couldn’t attack the player’s
> health at least a little, or the battle could end awkwardly, with the player and
> slime mold frozen in place for all time. Therefore, the slime mold also has a
> superwimpy squirt attack that happens during half of all attacks , but sub-
> tracts only a single health point from the player .



### CUNNING BRIGAND

``` common-lisp
(defstruct (brigand (:include monster)))
(push #'make-brigand *monster-builders*)

(defmethod monster-attack ((m brigand))
  (let ((x (max *player-health* *player-agility* *player-strength*)))
    (cond ((= x *player-health*)
           (princ "A brigand hits you with his slingshot, taking off 2 health
points! ")
           (decf *player-health* 2))
          ((= x *player-agility*)
           (princ "A brigand catches your leg with his whip, taking off 2
agility points! ")
           (decf *player-agility* 2))
          ((= x *player-strength*)
           (princ "A brigand cuts your arm with his whip, taking off 2
strength points! ")
           (decf *player-strength* 2)))))
```

* max - max vrednost od prosledjenih argumenata


> The first thing the wily brigand does when performing an attack is to
> look at the player’s health, agility, and strength, and choose the max of those
> three as the focus of his attack . If several of the attributes are equally large,
> the brigand will choose health over agility and agility over strength as the
> focus of attack. If health is the largest value, the player is hit with a slingshot
> . If agility is the largest, the brigand will whip the player’s leg . If strength
> is the largest, the brigand will whip the player’s arm.


## CHAPTER 10: LOOP

#### Loop macro

``` common-lisp
(loop for i
      below 5
      sum i)
```

#### Tokens

* for - declare variable that iterates 
* below - halt when it reaches value
* sum - add toghter all values

* FROM, TO
Range od - do
``` common-lisp
(loop for i
      from 5
      to 10
    sum i)
; 45
```

* IN
Loop kroz iteme iz liste
``` common-lisp
(loop for i
      in '(100 20 3)
    sum i)
; 123
```

* DO
Radi nesto unutar petlje.
``` common-lisp
(loop for i
      below 5
    do (print i))
; 0
; 1
; 2
; 3
; 4
; 5
```

* WHEN
Uslovi za petlju

``` common-lisp
(loop for i
      below 10
    when (oddp i) ; oddp neparan broj
    sum i)
; 25
```

* RETURN
Breakovanje iz ptelje

``` common-lisp
(loop for i
      from 0
    do (print i)
    when (= i 5)
    return 'falafel)
; 0
; 1
; 2
; 3
; 4
; 5
; FALAFEL
```

* COLLECT
Sakuplja rezultate petlje u listu (koja moze da sadrzi vise elementata).
Ukratko vrati vise od jednog itema.

``` common-lisp
(loop for i
      in '(2 3 4 5 6)
      collect (* i i))
; (4 9 16 25 36)
```

* MULTIPLE CLAUSES
Visestruki for clause

``` common-lisp
(loop for x below 10
      for y below 10
      collect (+ x y))
; (0 2 4 6 8 10 12 14 16 18)
```

Vrsi se iteracija kroz x i y istovremeno i vraca 10 vrednosti.

* CARTESIAN PRODUCT
Loop treba da prolazi makar jednom kroz svaku mogucu kombinaciju 2 ranges.
Ovo se postize ugnjezdenim petljama.

``` common-lisp
(loop for x below 10
      collect (loop for y below 10
                    collect (+ x y))
;((0 1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9 10) (2 3 4 5 6 7 8 9 10 11)
(3 4 5 6 7 8 9 10 11 12) (4 5 6 7 8 9 10 11 12 13) (5 6 7 8 9 10 11 12 13 14)
(6 7 8 9 10 11 12 13 14 15) (7 8 9 10 11 12 13 14 15 16)
(8 9 10 11 12 13 14 15 16 17) (9 10 11 12 13 14 15 16 17 18))
```

10 x 10

* INDEX

``` common-lisp
(loop for i
      from 0
      for day
      in '(monday tuesday wednesday thursday friday saturday sunday)
      collect (cons i day))
;((0 . MONDAY) (1 . TUESDAY) (2 . WEDNESDAY) (3 . THURSDAY) (4 . FRIDAY) (5 .
SATURDAY) (6 . SUNDAY))
```

Uz pomoc **cons** kreiramo **dotted listu**.


Postoji **ogromna** kolicina loop varijanti. (str 200, 201)

#### EVOLVE GAME
Igra simulacija.


``` common-lisp
; Parametri sveta

(defparameter *width* 100)
(defparameter *height* 30)
(defparameter *jungle* '(45 10 10 10))
(defparameter *plant-energy* 80)
```

Dimenzije sveta *width*, *height*. *Jungle* predstavlja junglu u sredini mape (x, y, widht, height).
*Plant-energy* je energija koju zivotinja dobije kad pojede biljku (omogucava joj 80 dana bez hrane)


``` common-lisp
(defparameter *plants* (make-hash-table :test #'equal))
```

- :test - podesavanje test conditiona

Ovde se skladiste sve biljke (x,y kordinate) u hash table.

> By default, a Common Lisp hash table uses eq when testing for the equal-
> ity of keys. For this hash table, however, we’re defining :test to use equal
> instead of eq , which will let us use cons pairs of x- and y-coordinates as keys. If
> you remember our rule of thumb for checking equality, cons pairs should be
> compared using equal . If we didn’t make this change, every check for a key
> would fail, since two different cons cells, even with the same contents, test
> as being different when using eq .


``` common-lisp
(defun random-plant (left top width height)
  (let ((pos (cons (+ left (random width)) (+ top (random height)))))
    (setf (gethash pos *plants*) t)))

(defun add-plants ()
  (apply #'random-plant *jungle*)
  (random-plant 0 0 *width* *height*))
```

> The **random-plant** function creates a new plant within a specified region
> of the world. It uses the **random** function to construct a random location and
> stores it in the local variable **pos** . Then it uses **setf** to indicate the existence
> of the plant within the hash table . The only item actually stored in the
> hash table is **t** . For this *plants* table, the keys of the table (the x,y position of
> each plant) are actually more than the values stored in the table.

> It may seem a bit weird to go through the trouble of creating a hash table
> to do nothing more than store t in every slot. However, Common Lisp does
> not, **by default**, have a data structure designed for holding **mathematical sets.**
> In our game, we want to keep track of the set of all world positions that have
> a plant in them. It turns out that hash tables are a perfectly acceptable way of
> expressing this. You simply use each set item as a key and store t as the value.
> Indeed, doing this is a bit of a hack, but it is a reasonably simple and efficient
> hack. (Other Lisp dialects, such as Clojure, have a set data structure built right
> into them, making this hack unnecessary.)

> Every day our simulation runs, the add-plants function will create **two** new
> plants: one in the jungle and one in the rest of the map . Because the jungle
> is so small, it will have dense vegetation compared to the rest of the world.

#### ANIMALS

``` common-lisp
(defstruct animal x y energy dir genes)
```

* X, Y
> We need to track several properties for each animal. First, we need to know
> its x- and y-coordinates. This indicates where the animal is located on the
> world map.

* ENERGY
> Next, we need to know how much energy an animal has. This is a Darwinian
> game of survival, so if an animal can’t forage enough food, it will starve and
> die. The energy field tracks how many days of energy an animal has remain-
> ing. It is crucial that an animal find more food before its energy supply is
> exhausted.

* DIR

0 1 2
7 x 3
6 5 4

> We also need to track which direction the animal is
> facing. This is important because an animal will walk
> to a neighboring square in the world map each day.
> The dir field will specify the direction of the animal’s
> next x,y position as a number from 0 to 7:
> For example, an orientation of 0 would cause the
> animal to move up and to the left by the next day.


* GENES
         FWD
        7 0 1
LEFT    6 x 2   RIGHT
        5 4 3
         BCK

> Finally, we need to track the animal’s genes . Each
> animal has exactly eight genes, consisting of positive integers. These integers
> represent eight “slots,” which encircle the animal as follows:

> Every day, an animal will decide whether to continue facing the same
> direction as the day before or to turn and face a new direction. It will do this
> by consulting these eight slots and randomly choosing a new direction. The
> chance of a gene being chosen will be proportional to the number stored in
> the gene slot.

SLOT | VALUE
 0   |   1
 1   |   1
 2   |  10
 3   |   1
 4   |   1
 5   |   1
 6   |   1
 7   |   1
 8   |   1

> In this example, an animal has a large number (10) stored
> in slot 2. Looking at our picture of the eight slots around the
> animal, you can see that slot 2 points to the right. Therefore,
> this animal will make a lot of right-hand turns and run in a circle. Of course,
> since the other slots still contain values larger than zero, the animal will occa-
> sionally move in another direction.

``` common-lisp
(defparameter *animals*
  (list (make-animal :x  (ash *width* -1)
                     :y  (ash *height* -1)
                     :energy 1000
                     :dir 0
                     :genes (loop repeat 8
                                  collecting (1+ (random 10))))))
```

> We make the animal’s starting point the center of the world by setting
> the x and y positions to half of the map’s width and height, respectively. We
> set its initial energy to 1000 , since it hasn’t evolved much yet and we want it to
> have a fighting chance at survival. It starts off facing the upper left, with its
> dir field set to 0 . For its genes, we just use random numbers.

* Performance of hash-table vs list
> Note that unlike the *plants* structure, which was a hash table, the
> *animals* structure is just a plain list (currently containing only a single mem-
> ber). This is because, for the core of our simulation, we never need to search
> our list of animals. Instead, we’ll just be traversing *animals* once every simu-
> lated day, to let our critters do their daily activities. Lists already support effi-
> cient linear traversals, so using another, more complex data structure (such as
> a table) would have no significant effect on the performance of our simulation.


#### ANIMAL MOVEMENT

``` common-lisp
(defun move (animal)
  (let ((dir (animal-dir animal))
        (x (animal-x animal))
        (y (animal-y animal)))
    (setf (animal-x animal) (mod (+ x
                                    (cond ((and (>= dir 2) (< dir 5)) 1)
                                          ((or (= dir 1) (= dir 5)) 0)
                                          (t -1))
                                    *width*)
                                 *width*))
    (setf (animal-y animal) (mod (+ y
                                    (cond ((and (>= dir 0) (< dir 3)) -1)
                                          ((and (>= dir 4) (< dir 7)) 1)
                                          (t 0))
                                    *height*)
                                 *heigth*))
    (decf (animal-energy animal))))
```

> The **move** function modifies the *x and y fields*, using the *animal-x and
> animal-y accessors*. As we’ve discussed, these are automatically generated
> through the *defstruct macro*, based on the field names. At the top of this
> function, we use the accessors to retrieve the x- and y-coordinates for the
> animal. Then we use the same accessors to set the same values, with the aid of setf.

> To calculate the new **x-coordinate**, we use a **cond** command to first check
> if the direction is *2, 3, or 4* . These are the directions the animal may face
> that point east in the world, so we want to add one to the *x-coordinate*. If the
> direction instead is *1 or 5*, it means the animal is facing *directly north or
> south* . In those cases, the x-coordinate shouldn’t be changed. In all other
> cases, the animal is facing west and we need to subtract one . The y-coordi-
> nate is adjusted in an analogous way .

> Since the world needs to *wrap around at the edges*, we do some extra math
> using the **mod (remainder) function** to calculate the modulus of the coordinates
> and enable wrapping across the map. If an animal would have ended up
> with an *x-coordinate of width , the mod function puts it back to zero, and it
> does the same for the y-coordinate and height* . So, for example, if our func-
> tion makes the animal move east until *x equals 100*, this will mean that ( mod
> 100 *width* ) *equals zero*, and the animal will have wrapped around back to
> the far west side of the game world.


#### HANDLING ANIMAL TURNING

``` common-lisp
(defun turn (animal)
  (let ((x (random (apply #'+ (animal-genes animal)))))
    (labels ((angle (genes x)
               (let ((xnu (- x (car genes))))
                 (if (< xnu 0)
                     0
                     (1+ (angle (cdr genes) xnu)))))
             (setf (animal-dir animal)
               (mod (+ (animal-dir animal) (angle (animal-genes animal) x))
                    8))))))
```

* label - isto sto i flet (lokalna definicija f()) ali dozvoljava rekurziju
* angle - naziv rekurzivne funkcije definisane u labels
* xnu - var gde se cuva 

> This function needs to make sure that the amount the animal turns is
> proportional to the gene number in the given slot. It does this by first sum-
> ming the amount of all genes, and then picking a random number within
> that sum . After that, it uses a recursive function named **angle** , which
> traverses the genes and finds the gene that corresponds to the chosen number,
> based on the respective contributions of each gene to the sum. *It subtracts
> the running count in the argument x from the number stored at the current
> gene* . If the running count has hit or exceeded zero, the function has reached
> the chosen number and stops recursing . Finally, it adds the amount of
> turning to the current direction and, if needed, wraps the number around
> back to zero, once again by using mod .



#### ANIMAL EAT
``` common-lisp
(defun eat (animal)
  (let ((pos (cons (animal-x animal) (animal-y animal))))
    (when (gethash pos *plants*)
      (incf (animal-energy animal) *plant-energy*)
      (remhash pos *plants*))))
```

* remhash - remove item form hash-table

Proverava da li na poziciji animal postoji biljka ukoliko da uvecava energiju animal za vrednost,
i uklanja biljku iz hash-table *plants*.


#### REPRODUCTION

``` common-lisp
(defun reproduce (animal)
  (let ((e (animal-energy animal)))
    (when (>= e *reproduction-energy*)
      (setf (animal-energy animal) (ash e -1))
      (let ((animal-nu (copy-structure animal))
            (genes     (copy-list (animal-genes animal)))
            (mutation  (random 8)))
        (setf (nth mutation genes) (max 1 (+ (nth mutation genes) (random 3) -1)))
        (setf (animal-genes animal-nu) genes)
        (push animal-nu *animals*)))))
```

* ash e -1 =  arithmetic shift operation ovo prepolovljuje broj
* copy-structure - kopiranje structure elementa
* copy-list - kopiranje liste

> It takes a healthy parent to produce healthy offspring, so our animals will
> reproduce only if they have at least **200** days’ worth of energy . We use the
> global constant *reproduction-energy* to decide what this cutoff number should
> be. If the animal decides to reproduce, it will lose *half its energy to its child* .

> To create the new animal, we simply copy the structure of the parent
> with the **copy-structure** function . We need to be careful though, since
> copy-structure performs only a *shallow copy* of a structure. *This means that if
> there are any fields in the structure that contain values that are more compli-
> cated than just numbers or symbols, the values in those fields will be shared
> with the parent*. An animal’s genes, which are stored in a list, represent the only
> such complex value in our animal structures. If we aren’t careful, mutations
> in the genes of an animal would simultaneously affect all its parents and
> children. In order to avoid this, we need to create an explicit copy of our
> gene list using the copy-list function .

> To mutate an animal in our reproduce function, we randomly pick one of
> its 8 genes and place it in the mutation variable. Then we use setf to twiddle
> that value a bit, again using a random number.


#### SHALLOW COPY
Problemi sa shallow copy.

``` common-lisp
(defparameter *parent* (make-animal :x 0
:y 0
:energy 0
:dir 0
:genes '(1 1 1 1 1 1 1 1)))
*PARENT*

(defparameter *child* (copy-structure *parent*))
*CHILD*

(setf (nth 2 (animal-genes *parent*)) 10)
10

*parent*
#S(ANIMAL :X 0 :Y 0 :ENERGY 0 :DIR 0 :GENES (1 1 10 1 1 1 1 1))

*child*
#S(ANIMAL :X 0 :Y 0 :ENERGY 0 :DIR 0 :GENES (1 1 10 1 1 1 1 1))
```

> Here, we’ve created a parent animal with all its genes set to 1 . Next, we
> use copy-structure to create a child . Then we set the third (second count-
> ing from zero) gene equal to 10 . Our parent now looks correct . Unfor-
> tunately, since we neglected to use copy-list to create a separate list of genes
> for the child, the child genes were also changed when the parent mutated.
> Any time you have data structures that go beyond simple atomic symbols or
> numbers, you need to be very careful when using setf so that these kinds of
> bugs don’t creep into your code. In future chapters (especially Chapter 14),
> you’ll learn how to avoid these issues by not using functions that mutate data
> directly, in the manner that setf does.



#### SIMULATING DAY

``` common-lisp
(defun update-world ()
  (setf *animals* (remove-if (lambda (animal)
                               (<= (animal-energy animal) 0))
                             *animals*))
  (mapc (lambda (animal)
          (turn animal)
          (move animal)
          (eat animal)
          (reproduce animal))
        *animals*)
  (add-plants))
```

* remove-if - koncidional
* mapc - kao mapcar ali ne vraca listu

Prvo se uklanjaju sve mrtve animals iz sveta.
Nakon toga prolazi kroz sve animals i aktivira *turn,move,eat,reproduce* f().
Na kraju ponovo kreira nove biljke u svetu. (2 nove svakog dana, 1 jungle 1 rest)


#### DRAWING WORLD


``` common-lisp
(defun draw-world ()
  (loop for y
          below *height*
        do (progn (fresh-line)
                  (princ "|")
                  (loop for x
                        below *width*
                        do (princ (cond ((some (lambda (animal)
                                                 (and (= (animal-x animal) x)
                                                      (= (animal-y animal) y)))
                                               *animals*)
                                         #\M)
                                        ((gethash (cons x y) *plants*) #\*)
                                        (t #\space))))
                  (princ "|"))))
```

* some - u okviru petlje vezano za predikate da li postuju neki uslov. U ovom slucaju da li makar jedan elemenat iz liste (some test list) - ovde je test lambda, lista *animals*
* #\ - character literal

> First, the function uses a loop to iterate through *each of the world’s rows*
> . Every row starts with a new line (created with fresh-line ) followed by a
> vertical bar, which shows us where the left edge of the world is. Next, we iterate
> across the *columns of the current row* , checking for an animal at every
> location. We perform this check using the **some** function , *which lets us
> determine if at least one item in a list obeys a certain condition. In this case,
> the condition we’re checking is whether there’s an animal at the current x- and
> y-coordinates*. If so, we draw the letter **M**at that spot . (The capital letter M
> looks a little like an animal, if you use your imagination.)
> Otherwise, we check for a plant, which we’ll indicate with an asterisk ( ***** )
> character . And if there isn’t a plant or an animal, we draw a **space** charac-
> ter . Lastly, we draw another vertical bar to cap off the end of each line .


#### UI

``` common-lisp
(defun evolution ()
  (draw-world)
  (fresh-line)
  (let ((str (read-line)))
    (cond ((equal str "quit") ())
          (t (let ((x (parse-integer str :junk-allowed t)))
               (if x
                   (loop for i
                          below x
                          do (update-world)
                          if (zerop (mod i 1000))
                          do (princ #\.))
                   (update-world))
               (evolution)))))
```

* (cond ((equal str "quit") ()) - ovde je izlazak iz programa prazna lista ()
* parse-integer - konvertuje neki podatak u int
  :junk-allowed t - to znaci da prihvata i string

> **First**, this function draws the world in the REPL . **Then** it waits for the
> user to enter a command at the REPL using read-line . If the user enters
> **quit** , the simulation ends . Otherwise, it will attempt to parse the user’s
> command using **parse-integer** . We set **:junk-allowed** to true for parse-integer ,
> which lets the interface accept a string even if it isn’t a valid integer.

> If the user enters a valid integer **n**, the program will run the simulation
> for **n simulated days**, using a loop . *It will also print a dot to the screen for
> every 1000 days, so the user can see that the computer hasn’t frozen while
> running the simulation.*

> If the input isn’t a valid integer, we run *update-world to simulate one
> more day*. Since read-line allows for an empty value, the user can just tap the
> ENTER key and watch the animals move around their world.

> Finally, the evolution function recursively calls itself to redraw the world
> and await more user input . Our simulation is now complete.

 Jos dodatnih informacija o samoj simulaciji na kraju chaptera.
 
 
 ## CHAPTER 11
