# Scheme In Scheme

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Scheme In Scheme](#scheme-in-scheme)
    - [Why would you do this?](#why-would-you-do-this)
    - [Can't you just use eval?](#cant-you-just-use-eval)
    - [Built-ins](#built-ins)
- [Part 1: A Limited Scheme-in-Scheme (60 points)](#part-1-a-limited-scheme-in-scheme-60-points)
    - [A Small note on testing](#a-small-note-on-testing)
    - [1a: Very Basic Expressions (5 points)](#1a-very-basic-expressions-5-points)
    - [1b: my-if (10 points)](#1b-my-if-10-points)
    - [1c: variables and my-let (15 points)](#1c-variables-and-my-let-15-points)
    - [1d: my-lambda (15 points)](#1d-my-lambda-15-points)
    - [1e: builtin functions (10 points)](#1e-builtin-functions-10-points)
    - [1f: recursion (actually, it's already implemented) (5 free points if you got this far)](#1f-recursion-actually-its-already-implemented-5-free-points-if-you-got-this-far)
- [Part 2: More complex parsing (40 points)](#part-2-more-complex-parsing-40-points)
    - [What is parsing?](#what-is-parsing)
    - [What will our code do?](#what-will-our-code-do)
    - [How to do parsing](#how-to-do-parsing)
    - [Advice for going step by step](#advice-for-going-step-by-step)
- [That's it!](#thats-it)

<!-- markdown-toc end -->


We're going to begin this with a very classic scheme project and then
make it slightly more interesting in the second step.  The idea is
this: we want to build a scheme interpreter in scheme.

This scheme interpreter will be a function that takes in a quoted
scheme program and evaluates it - for example:

    (my-eval '(my-let ((x 1)(y 2)) y))
    2
    
You can see that I've prefixed all the classic scheme structures with
my- (e.g. let becomes my-let).  This isn't strictly necessary but I've
done it to make it obvious when you're looking at code to be evaluated
by your interpreter.  I call the whole language we're making
"my-scheme" for obvious reasons.  That said, you can expect the my-
versions to act very similarly to their official scheme versions,
except where noted.

## Why would you do this?

The first thing to say is that scheme is a very nice language to build
a scheme interpreter in.  You could build a scheme interpreter in
(say) Java but as scheme programs are basically hierarchical lists
using a language designed to manipulate lists makes it easy.

The second thing is that implementing a language within a language is
often very useful.  We might want to write programs that might look
superficially similar to their host language but they actually execute
in some different way (e.g. get uploaded to run on video card
hardware, get distributed and processed across a network, etc.).
Sometimes we want a flexible way to write configuration or plugins
that that can only execute in a way that can't affect the real running
application (lua UI tweaks in a game like world of warcraft are an
example of this).  Sometimes we want to add new features to the
language that can't be accomplished with simple macros.

We won't be going through the trouble of actually going far enough to
justify the existence of our particular scheme-in-scheme, but I want
to be clear that this is a classic project partially because it isn't
too far fetched you might actually want to do this.

## Can't you just use eval?

Yes - but don't.  You can write a simple program that translates the
given my-scheme code into pure scheme - and then call eval on that.
This defeats the point of the assignment though.

For the most part, you are required to accomplish what you need to
accomplish without using the underlying scheme feature.  For example,
your lambda implementation should not build Racket lambdas - instead
you will need to create your own way to represent lambdas which fields
for params, code etc.

Now the intention is not to be pedantic - of course to generate these
lambda functions you might write a Racket function called
make-my-cool-lambda-structure or whatever.  Because all racket
functions are implemented with lambda, in some sense you're using
lambda in your lambda implementation.  The key thing here is that the
code to represent and evaluate my-lambda expressions must be of your
own design - not piggybacking on the feature in Racket.  The same is
true of all the features we ask you to implement.

## Built-ins

There is an exception to this that I call "built-ins".  Built-ins are
features of your language that you can more-or-less implement in terms
of their corresponding scheme implementations.  One obvious one is
numbers and + - * / operations.  You can use Racket's representation
of numbers - and your can implement my-+ primarily by calling the
built-in Racket function +.

Now just because you're allowed to call + doesn't necessarily mean the
implementation will be trivial.  You'll still have to figure out a way
to for a my-+ in given code to be parsed, discover the operation being
requested, evaluate all parameters, and then invoke + (hint: you might
find the scheme function apply handy here).

Once you get to the point of adding builtins, it's worth it to maybe
ponder a bit how to represent the builtins in a way that doesn't
require a ton of custom code for each one.

# Part 1: A Limited Scheme-in-Scheme (60 points)

So this part will get into the details of your scheme implementation.
The my-scheme will actually be a pretty small subset of scheme both
removing some features that important but not so critical (e.g. we
implement if but not cond for example) and some that are hard (e.g. we
won't do macros or continuations).  Hopefully this will be enough to
give a feel for how you'd do the whole thing, without taking too much
time.

I've organized this assignment in an order I hope will be easy to do
step-by-step.  If you'd like to do it in a different way though feel
free.

## A Small note on testing

So to make your life nice, I've organized testing in two different
ways this assignment.  If your look at schemeinscheme.rkt you'll see that
there are commented out check-equal statements in the source file.  My
thought is that as you implement features you can uncomment these
lines (and maybe add some of your own) going step by step through the
various features.

But then, there is also a test.rkt which has all the tests that can
run at once.  There's no extra tests in test.rkt, but they are better
organized which lets us grade more easily.

Note that the given unit tests are not necessarily exhaustive - we
don't have another secret bigger library of tests, but if we notice
looking at your implementation that you've got a bug we will take away
points.  Test yourself on a variety of cases and don't make your
implementation rely on idiosyncrasies of the tests.  If you have
questions on correct behavior in some cases post of piazza.

## 1a: Very Basic Expressions (5 points)

You can consider Racket's representation of numbers, strings, and #t
and #f to be builtin.  So most constants should just evaluate to
themselves:

    (my-eval 1) => 1
    (my-eval "hello") => "hello"
    (my-eval #t) => #t
    
Similarly, you can consider Racket's representation of lists and
symbols to be built in.  That should make my-quote very easy to
implement:

    (my-eval '(my-quote (my-+ 1 2))) => '(my-+ 1 2) 

As usual with quote, my-quote returns a symbolic representation even
when the thing represented is something that could be evaluated.

Note that my-scheme does not implement ' - you have to explicitly say
"quote".

## 1b: my-if (10 points)

my-if always takes 3 parameters and works similarly to if.  If is not
exactly a built in but it's understood you'll have to use Racket's if
(or cond) to check the value of the conditional expression.

    (my-eval '(my-if #t 1 2)) => 1

Note that once we get functions with side effects my-if will be required
to only evaluate one of the two branches.  So for example:

    (my-eval (my-if #t (display "hello") (display "goodbye")))
    
Should only display hello.  This means that if can't be a normal
function.  Right now there's no way to test that behavior but I
mention it now to maybe save you a little trouble up the road.

## 1c: variables and my-let (15 points)

So this is where things get interesting.  Let is most definitely not a
built-in - you must develop your own way to represent variables.  Not
only is this important in terms of your learning - but it basically is
a necessity in terms of the embedded language being safe and separated
from it's host language.  For example it should always be the case
that in code like this

    (let ((secret-var 33)) (my-eval 'some-code))
    
it is not possible, regardless of the value of some-code, to read or
write secret-var.

So let's talk about how to represent variables - you are going to need
what is traditionally called an environment.  An environment is
basically a mapping of variables to values.

Your main eval function will take an environment as an additional
parameter.  So the operation of something like let will be:

a.  Evaluate all of the expressions in the variable part of the let to
determine their values, using the current environment.  

b.  Make a new environment, based on the existing environment, that
has the newly declared variables mapped (plus all the other variables
that are already mapped from elsewhere)

c.  Now evaluate the inner code of the let, passing it this new
environment

It is up to you to decide how to represent the environment.  Consider
Racket hashes or lists of symbol mapping pairs.

One thing to note - usually an environment is implemented as a list of
sub-environments.  So if I was using hashes I'd do a list of hashes.
That way adding on to the environment doesn't involving copying or
editing a structure - it just looks like:

    (let ((new-environment (cons new-mappings old-environment)))
         (my-eval-with-environment new-environment let-code))

That's not required but what is required is that variables do not
exist outside of their scope.  Once a let completes the environment
must be returned to its original state: remember a variable may have
been shadowed by a declaration of the same name.

The my-eval we use for tests still won't take any additional
parameters.  But it will just construct an initial environment and
then call the recursive version that takes an environment.

That initial environment might not be completely empty however.  For
example, my initial code maps the symbol null to '().  Although you
could handle it other ways, it's handy insofar as it can let you test
your variable lookup code before you actually implement let:

    (my-eval 'null) => '()

Once that works move on to let itself

    (my-eval '(my-let ((x 1)(y 2)) x)) => 1
    (my-eval '(my-let ((x 1)(y 2)) (my-let ((x 3) (z 4)) x ))) => 3
    
Note that my-let always has one 1 code section (i.e. (my-let ((x 1)) x
x) is not valid.  Note also that recursive definitions are not
allowed - this is let and not letrec.

## 1d: my-lambda (15 points)

Lambda is again not a built in - you must represent lambdas yourself.
But how?

The idea is actually pretty simple: a lambda is a set of parameters
names, the environment at the time of the lambda's creation, and the
code to run.  A lambda expression will simply return that as a data
object (I called mine a procedure, to distinguish it conceptually) -
exactly how it is constructed is up to you (you might consider
Racket's structs though).

Function invocations will expect the first parameter to evaluate to
this "procedure" type.  All other parameters will be any type (though
of course they might be procedures too, or ordinary values).

At invocation time is when we evaluate the lambda code.  We make a new
environment with the value of the parameters and append that to the
stored lambda environment.  Remember that outside of the parameters
the lambda executes in the context it was created not the context it
was invoked in.  That custom environment is all you need - beyond that
it should be a simple call to your regular evaluation function.

    (my-eval '((my-lambda (x y) x) 1 2)) => 1

Note that similar to let will we limit lambda expressions to one code
block (e.g. (my-lambda (x y) x 555) is not legal).

## 1e: builtin functions (10 points)

Now that we know how to represent user-coded functions in our
language, let's add in some functions to do basic manipulation.

Here's the functions we expect:

- my-+
- my--
- my-*
- my-/
- my-cons
- my-car
- my-cdr
- my-null?
- my-eq?
- my-display

All of these may be implemented using their corresponding Racket
function.  I'd encourage you to try and think of an implementation
that makes adding new builtins like this easy, as you need to add
quite a few.  You may use Racket lambdas to help you in that endeavor,
if you wish.

    (my-eval '(my-let ((x 3) (y 4)) (my-+ x y))) => 7

Note that these builtin functions should act in all ways like ordinary
functions from my-scheme's perspective.  So you should be able to do
stuff like this:

    (my-eval '(my-let ((x 3) (y 4) (func my-+)) (func x y))) => 7

## 1f: recursion (actually, it's already implemented) (5 free points if you got this far)

So we've implemented a very small subset of scheme here.  You might
think it's not enough to actually do any useful computation - in
particular you might notice we have no looping constructs and
recursion seems impossible.  But we actually have more than enough to
make recursion work - consider this implementation of factorial:


    (my-let ((simple-fact
              (my-lambda (recurse val)
                         (my-if (my-eq? val 0) 1
                                (my-* val (recurse recurse (my-- val 1)))))))
            (simple-fact simple-fact 3))
            
This is a recursive implementation of factorial that should work
correctly in your my-scheme.  It looks a little different than the
classic version - note that when you call simple-fact, you pass it its
own lambda as a parameter.  Then to recursively call itself, it uses
that parameter and passes it on.

Turns out within the bounds of my-scheme we can make even more normal
recursion (and I may discuss that at some length in a lecture) but for
now this is enough for a rudimentary code system.

# Part 2: More complex parsing (40 points)

I've mentioned before that Scheme is easy to parse.  Now I want you
explore parsing a language that's a little involved.

Imagine we wanted to extend our my-scheme language to allow numerical
computations in the classic style: x + 4 * y.  Our operators will be
infix and we'll support operator precedence: x + 4 * y will be
conceptually x + (4 * y) because multiplications and division have
precedence over addition and subtraction.  When you want to violate
this precedence you'll also be able to use parenthesis (actually \< and
\>) to denote what should be evaluated first.

We won't actually add the parsing to our my-scheme.  Instead, we'll
write a function parse that takes in expression language
and outputs my-scheme structures that can be executed as normal.

## What is parsing?

Programs are hierarchical structures but looking at them the hierarchy
is not always obvious.  Consider this snippet of psuedo-java:

    int var = 99;
    for (int var = 0; var < 10; var++)
    System.out.println(var);
    System.out.println(var);

Knowing java, you probably realize the hierarchy looks something like this:

    (let ((var 99))
        (let ((var 0))
            (for 
                (lambda () (< var 10)) 
                (lambda () (set! var (+ var 1)))
                (lambda () (display var))))
        (display var))

I've expressed it in scheme but don't let that distract you: the key
thing is that second println is in a different place in the hierarchy
than the first println.  This explains why the second println is
actually referring to a different variable var than the first println.

If you wanted to run java code, you have to take the given code and
recover the hierarchical view of the language - called the abstract
syntax tree (AST).  Only from the AST can actual running or compiling
happen.  The process of converting the textual form to the abstract
syntax tree is called parsing.

This is why scheme is considered an easy language to parse: the
parenthesis make explicit in scheme what is implicit in Java.  It also
makes things like macros much easier - macros usually operate on the
AST, so it's best if the ASTs are understandable by macro writers. In
scheme understanding the code is basically the same as understanding
the AST.  In languages like Java the AST is actually really ugly and
complicated meaning programs that transform Java programs are really
annoying to write.
    
## What will our code do?

So our code will take a ordered sequence of tokens, expressed as a
scheme list and output a corresponding my-scheme expression.  For
example:

    (parse '(1 * foo - 2)) => '(my-- (my-* 1 foo) 2)

I say "tokens" because we're going to a list of numbers/symbols as
input rather than characters.  So you won't have to worry about
figuring out that foo is a singular thing.  You won't have to worry if
foo\*1 should be considered 3 things (symbol symbol number) or one
thing (a single symbol called foo\*1).  Converting raw character data
to this list of tokens would normally be considered *lexing* (as
distinguished from parsing) but that's a distraction for our purposes
here.

Note however that this list is just a flat list, with no lists within
it.  So even if we see input with some explicit structure

    (parse '(< < 1 - q  > * < 2 > >)) => '(my-* (my-- 1 q) 2))

it comes into the parser as a flat series of tokens and reconstructing
that hierarchy is up to the parser.  This is why incidentally I opted to
use the pointy parens (\< \>).  If we used anything else scheme would
try and interpret that as lists and do our job for us.

Of course when you are writing a parser you have to be explicit about
the language you are parsing.  Here's the official one for us:

     expression = 
         term
         | term "+" expression
         | term "-" expression
     
     term = 
         factor
         | factor "*" term
         | factor "/" term
     
     factor =
         ident
         | number
         | "<" expression ">"
         
Note that ident here refers to any Racket symbol, with the exception
of + - \* / \< or \>.  Number refers to a scheme number.

We require that the input to parse be exactly 1 expression, which
fully consumes all tokens.

To understand the above, read it like this:

an "expression" is either:

* a term
* OR a term followed by the symbol "+" followed by an expression
* OR a term followed by the symbol "-" followed by an expression

The others work similarly.

As an example, let's interpret "2 * 3 + 1".  It can correctly parse
like this:

* An expression consisting of a term "2 * 3" "+" expression "1" (2nd derivation)
* term "2 * 3" consists of a factor "2" "*" and term "2" (2nd derivation)
* factor 2 is a number (2nd derivation)
* expression 1 is a factor 1 (1st derivation)
* term 1 is a factor 1 (1st derviation)
* factor 1 is a number (2nd derivation)
* etc.

It's important to note that although one could consider the original 2
to be a term by itself, if you do so there is no way to interpret the
"*" and therefore the expression can't parse.

If I've designed the language correctly, any given expression will
have exactly 1 working derivation - i.e. one unique abstract syntax
tree associated with it.

Further in this language, it's always desirable for a given rule to
consume all the tokens it can (e.g. if you have a choice in matching 2
or "2 * 3" always match the longer).

Some expressions can't parse at all (e.g. '(26 q)) and I don't care
what happens in those cases.

## How to do parsing

So in this step I'm going to allow you more freedom in your
implementation.  My only request is that your not use an built in
Racket parse capability or parser-generator and instead code it yourself.

I suggest you use what's called a recursive descent parser.  The basic
idea is pretty simple.  Say I'm in a function that's attempting to
parse an expression.  If the rule I'm parsing has a subrule ("term"),
I call a function for the subrule on the input first which will
consume some tokens.  Then I proceed on those remaining token to match
the remainder of my rule.

Three things to keep in mind:

1.  It's possible that I will attempt to parse a subrule and it fails.
    In that case I might try a different derivation or if there are no
    others fail myself.

2.  Don't forget what I mentioned before about the always consuming
    the max you can.
    
3.  If I'm successful in a subrule, I need to communicate back 2
    things back to the caller: the result of my match and what tokens
    remain unconsumed.  The result could be a structure I'll convert
    later (e.g. '(termDerv2 (factorDerv2 1) (termDerv1 (factorDerv2
    2))) or just my-scheme that this part of tree represents
    (e.g. '(my-* 1 2)).  The tokens that remain unconsumed is normally
    just a list of tokens, but you can represent it other ways if you
    wanted.


That's how my solution works anyway - and it generalizes pretty well
to complex grammars (more complex than the ones here).  

I think there are even easier solutions that could work because the
language we're trying to parse is pretty simple.  So don't be afraid
to go your own way!

My solution uses a lot of functions that generate functions - not that
you have to do that way - but I want you be aware some interesting
approaches exist.

## Advice for going step by step

So my solution had several parse functions, one for each kind of
non-terminal (e.g. factor, expression) .  I started with the innermost
nonterminal (e.g. I started by parsing factors, then built a function
to parse terms, etc.).  The last thing I did was parens.

The function that all the tests use is "parse".  In my version that
basically sets up a clean parse state, passes it parse-expression,
then get the result back and pulls out the final derivation.

# That's it!

This is as far as we'll go in this assignment.  We could go further -
expand our parser language to support traditional function call style
(e.g. myFunc(a,b,c)) or add a whole bunch more powerful features to
my-scheme (continuations anybody - you might find it gratifying to
meditate on how to implement continuations without using
continuations).  But I hope this helped illuminate Scheme and some
interesting language concepts!

Submit your schemeinscheme.rkt as usual
