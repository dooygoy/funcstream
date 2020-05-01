# Functional programming with Haskell

This is an exploration of a beautiful programming language Haskell and functional programming.
I am learning from several books and documenting the learning here. There are
Haskell related links and I will attempt to write short reviews on each, and then
there are observation that are following books I currently read, often using
real life analogies and examples.

I find that observing code not just through
numbers but through seemingly unrelated analogies enables me in turn to get a
better grasp at numbers. When I try to explain some Haskell code to people who
do not know programming I notice they intuitively *do* get programming, it's just
the thing with numbers is that they can induce heavy feelings of guilt or remind
them on how bad were they in math which is only the fault of the teachers and
their reluctance and to bring clarity that is also rich with a variety of
conceptual examples that are not just stiff images but life like analogies that
motivate and *remind* the human about processes they are familiar with that are in
their essence programming, and not just that but at their core, functional
programming. There will be *errors* and before your vents go off please notice that
I am thinking in what  you could call a *free jazz improvisational* method, or
the so called Miles Davis *there are no wrong notes* method in which after you
make a mistake you do not *correct* the mistake but find a new turn from which
the wrong note, or sound will *sound* correct. Such exploration may be tedious
in the beginning, and yet this *spaced error repetition* eventually builds a
different kind of knowledge, one that goes from errors to correctness, from
exploring a variety of *mistakes* to a much richer *correct* understanding of
the issue at hand. It also reduces the memory footprint since by pruning the
errors and developing from them, our understanding becomes capable of a much
wider web of connections. If one orients on just being correct it is possible to
induce higher stress levels since one is always striving for correctness.

> This is so because the subject matter is not exhausted in its *aims*; rather,
> it is exhaustively treated when it is *worked out*. Nor is the *result* which is
> reached the *actual* whole itself; rather, the whole is the result together with
> the way the result comes to be. The easiest thing of all is to pass judgements on
> what is substantial and meaningful. It is much more difficult to get a real grip
> on it, and what is the most difficult of all is both to grasp what unites each of
> them and to give a full exposition of what that is.

### Table of contents

* [Books](#books)
* [Online Courses](#online-courses)
* [Talks](#talks)
* [Papers](#papers)
* [Blogs](#blogs)
* [Code](#code)
* [Inspirational](#inspirational)
* [Observations](#observations)
  * [Let vs Where](#let-vs-where)
  * [More of where](#more-of-where)
  * [Scope](#scope)
  * [To String or not to String](#to-string-or-not-to-string)
  * [Types](#types)
  * [Five Types of Buddhas](#five-types-of-buddhas)
  * [Addition Curry](#addition-curry)
  * [Mappings](#mappings)

## Books

  * [A type of programming](https://atypeofprogramming.com/)
  * [Learn you a Haskell for Greater Good](http://learnyouahaskell.com/)
  * Richard Bird: Thinking Functionally with Haskell
  * Graham Hutton: Programming in Haskell
  * Simon Thompson: Haskell - The Craft of Functional Programming
  * Allen/Moronuki: Haskell From First Principles
  * Will Kurt: Get Programming with Haskell
  * Hudak/Quick: The Haskell School of Music
  * David S. Touretzky: Common Lisp: A Gentle Introduction to Symbolic Computation
  * Alejandro Serrano Mena: Practical Haskell - A Real World Guide to Programming
  * [Phil Freeman: Purescript by example](https://leanpub.com/purescript)
  * [Discrete Mathematics using a computer](http://www.x.edu.uy/inet/Springer.pdf)


## Online courses

  * [Bartosz Milewski Super Awesome Category Theory Lectures!](https://www.youtube.com/user/DrBartosz/playlists)
  * [Functional programming in Haskell: Supercharge your coding](https://www.futurelearn.com/courses/functional-programming-haskell)
  * [Bartosz Milewski - School of Haskell](https://www.schoolofhaskell.com/user/bartosz)
  * [Functional programming in Haskell](https://www.youtube.com/playlist?list=PLJ5C_6qdAvBFJP1RiUrUUJI4GEhnJhgQw)
  * [C9 Lectures: Dr. Erik Meijer - Functional Programming Fundamentals](https://youtu.be/UIUlFQH4Cvo)
  * [Haskell Summer Course](https://www.youtube.com/playlist?list=PLaAHmR4OoQXcrQl7kgkraWQAgQ-8FpEmS)
  * [Brent Yorgey - CSCI 360: Programming Languages (Fall 2016)](http://ozark.hendrix.edu/~yorgey/360/f16/)
    > from: [My new programming languages course](https://byorgey.wordpress.com/2017/01/13/my-new-programming-languages-course/) I decided to use class time in an unconventional way: for each class meeting I created a “module”, consisting of a literate Haskell file with some example code, explanatory text, and lots of holes where students needed to write answers to exercises or fill in code. I split the students into groups, and they spent class time just working through the module. Instead of standing at the front lecturing, I just wandered around watching them work and answering questions. It took a bit of getting used to—for the first few classes I couldn’t shake the feeling that I wasn’t really doing my job—but it quickly became clear that the students were really learning and engaging with the material in a way that they would not have been able to if I had just lectured.

    > A happy byproduct of this approach is that the modules are fairly self-contained and can now be used by anyone to learn the material. Reading through all the modules and working through the exercises should be a great option for anyone wishing to learn some basics of programming language design and implementation. For example, I know I will probably reuse it to get summer research students up to speed. Note that the course assumes no knowledge of Haskell (so those familiar with Haskell can safely skip the first few modules), but introduces just enough to get where I want to go.

## Talks

  * [Why is Haskell so Hard to Learn and How to Deal With It](https://youtu.be/RvRVn8jXoNY)
  * [Stop Treading Water: Learning to Learn](https://youtu.be/j0XmixCsWjs)
  * [Why algebraic data types are important - Bartosz Milewski ](https://youtu.be/LkqTLJK2API)
  * [PureScript: Tomorrow's JavaScript Today](https://youtu.be/5AtyWgQ3vv0)
  * [LambdaConf 2015 - Learn Functional Programming with PureScript John A De Goes](https://youtu.be/LqYfdmb0eUU)
  * [Adventures with Types - SPJ](https://youtu.be/6COvD8oynmI)
  * [Conal Elliott - Denotational Design: From Meanings to Programs](https://youtu.be/bmKYiUOEo2A)

## Papers

 * [Ralf Hinze collection of papers](https://www.cs.ox.ac.uk/people/ralf.hinze/publications/index.html)
 * [Graham Hutton: Universality and expressiveness of fold](http://www.cs.nott.ac.uk/~pszgmh/fold.pdf)
 * [Eugenio Moggi: Notions of computation and monads](https://person.dibris.unige.it/moggi-eugenio/ftp/ic91.pdf)
 * [Raul Rojas: A Tutorial Introduction to the Lambda Calculus](https://arxiv.org/pdf/1503.09060.pdf)
 * [Why calculating is better than scheming](https://www.cs.kent.ac.uk/people/staff/dat/miranda/wadler87.pdf)

## Blogs:

* [Steve Yegge - Execution in The Kingdom of Nouns](http://steve-yegge.blogspot.com/2006/03/execution-in-kingdom-of-nouns.html)
> Hello, world! Today we're going to hear the story of Evil King Java and his quest for worldwide verb stamp-outage.
Caution: This story does not have a happy ending. It is neither a story for the faint of heart nor for the critical of mouth. If you're easily offended, or prone to being a disagreeable knave in blog comments, please stop reading now.
* [Letter to a Young Haskell Enthusiast](http://comonad.com/reader/2014/letter-to-a-young-haskell-enthusiast/)
> The following letter is not about what "old hands" know and newcomers do not. Instead, it is about lessons that we all need to learn more than once, and remind ourselves of. It is about tendencies that are common, and understandable, and come with the flush of excitement of learning any new thing that we understand is important, and about the difficulty, always, in trying to decide how best to convey that excitement and sense of importance to others, in a way that they will listen. It is written more specifically, but only because I have found that if we don't talk specifics as well as generalities, the generalities make no sense. This holds for algebraic structures, and it holds for other, vaguer concepts no less. It is a letter full of things I want to remember, as well as of advice I want to share. I expect I will want to remind myself of it when I encounter somebody who is wrong on the internet, which, I understand, may occur on rare occasion.
* [Monday Morning Haskell](https://mmhaskell.com/)
> At Monday Morning Haskell, we have tutorials for all levels of programmers! If you're new to Haskell, take a look at our Beginners series. If you have some experience with the language already, we've got some more Advanced material so you can get started on real world projects! Either way, come back every Monday morning for some new material on the Blog!
* [Why are partial functions (as in `head`, `tail`) bad?](https://www.reddit.com/r/haskell/comments/5n51u3/why_are_partial_functions_as_in_head_tail_bad/?utm_source=share&utm_medium=web2x)
>The problem with partial functions is that they're liars. Consider head: its type is [a] -> a, which means "give me a list of as and I'll give you an a". So I give it [] - does it give me an a? No, it doesn't, it throws an exception instead.
And when functions start lying about the things they return, you can no longer reason about them.
 * [Applied Haskell Syllabus](https://tech.fpcomplete.com/haskell/syllabus)
 > Applied Haskell is a commercial training program focusing on teaching intermediate Haskell. The goal is to help someone move from knowing Haskell basics to being able to write commercial software, with enough knowledge to pick up any new skills needed on demand.
 * [Haskell Learn - FPComplete](https://tech.fpcomplete.com/haskell/learn)
 * [An introduction to recursion schemes](https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html)
> Because nested structures appear in almost every problem domain and programming environment, from databases to 3D graphics to filesystems, the act of iterating through these structures is common, so common that most programmers barely notice when they’re doing it. As such, generalizing the act of recursive traversals provides immediate real-world benefits: our new generalized traversal can replace a host of type-specific traversal functions. In addition, by decoupling how a function recurses over data from what the function actually does, we reduce cognitive overhead and can focus entirely on the core behavior of our recursive functions.
 * [The Road to Proficient Haskell](https://williamyaoh.com/posts/2020-01-11-road-to-proficient.html)
 * [Getting started with Haskell](https://stackoverflow.com/questions/1012573/getting-started-with-haskell/1016986#1016986)
 * [You Could Have Invented Monads](http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html)
 > In fact, I hope to get you to invent them now if you haven't already. It's then a small step to notice that all of these solutions are in fact the same solution in disguise. And after reading this, you might be in a better position to understand other documents on monads because you'll recognise everything you see as something you've already invented.
 * [How To Do Basic Error Handling Logging](https://williamyaoh.com/posts/2019-10-12-how-to-basic-error-handling-logging.html)
 * [Basic Haskell: An Examination of a Todo List](https://www.benlopatin.com/basic-haskell-todo/)
 * [Standardized ladder of functional programming](https://pbs.twimg.com/media/CydL5EYUsAAI-61.jpg:large)
 * [Sum Types](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/sum-types)
 * [How Laziness works](https://two-wrongs.com/how-laziness-works)
 > What I'm about to describe is completely useless to learn how to write Haskell, but if you're like me and like poking at things under the hood, by all means join in.
 * [Space Leak Zoo](http://blog.ezyang.com/2011/05/space-leak-zoo/)
 > There are a few different types of space leak here, but they are quite different and a visitor would do well not to confuse them (the methods for handling them if encountered in the wild vary, and using the wrong technique could exacerbate the situation).
 * [Money in the type system where it belongs](https://ren.zone/articles/safe-money)
 > Notwithstanding the value civilization gives to a particular currency, the amount of said currency one owns can't spontaneously increase nor decrease in number. As programmers, we do play a crucial role in ensuring amounts of currency are never made up nor lost. In this article we will explore how we can leverage types, functional programming, and in particular the safe-money Haskell library to ensure that our software deals with monetary values and world currencies as carefully as civilization requires. Mostly, we will be exploring the type system and learning how to reason through types.
 * [Embedding Linear Lambda Calculus, Quickly and Easily](https://blog.functorial.com/posts/2017-08-05-Embedding-Linear-Lambda-Calculus.html)
  > Suppose you want to create an embedded DSL based on the linear lambda calculus. Why might you want to do this? Well, you might want to control access to some resource, or perhaps you've heard that linear types can change the world and now you'd like to compile your EDSL to some target language and optimize things using mutable data structures.
 * [Imperative Haskell](https://vaibhavsagar.com/blog/2017/05/29/imperative-haskell/)
  > I was working through Tim Roughgarden’s Algorithms 1 (which has now been replaced by two smaller courses) and attempting to do all the exercises in Haskell when I bumped up against an uncomfortable truth.
 * [An Opiniated Guide to Haskell in 2018](https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/)
 > In the meantime, in the interest of both sharing with others the small amount of wisdom I’ve gained and preserving it for my future self, I’ve decided to write a long, rather dry overview of a few select parts of the Haskell workflow I developed and the ecosystem I settled into.
 * [Functor-Oriented Programming](http://r6.ca/blog/20171010T001746Z.html)
 > My style of Haskell programming has been evolving over the 15 years that I have been working with it. It is turning into something that I would like to call “functor oriented programming”. The traditional use of typed functional programming focuses on data types. One defines data types to model the data structures that your program operates on, and one writes functions to transform between these structures. One of the primary goals in this traditional methodology is to create data structures that exclude erroneous states to the extent that is reasonably possible. As long as one ensures that pattern matching is complete, then the type system catches many errors that would otherwise lead to these erroneous states, which have been crafted to be unrepresentable. Functor oriented programming is a refinement of this traditional focus on data types.

## Code

 * [Lambda launcher](https://github.com/balsoft/lambda-launcher)
 * [Input Output](https://github.com/input-output-hk)
 * [Input Output/Plutus](https://github.com/input-output-hk/plutus)
 * [Awesome Haskell List](https://github.com/krispo/awesome-haskell)
 * [Chris Done - a demo web browser engine](https://github.com/chrisdone/vado)
 > This is a demonstration program that is able to load a web page up and render it like in the early 90's. It supports laying out text, different font sizes for headings, inline and block elements, hyperlinks, bold and italics. It supports mousewheel scrolling, too. I wrote this in a couple evenings, because it seemed straight-forward to do so given the libraries available today. That's a good sign for Haskell. Also, there's an inarticulate gut feeling I have that tells me maybe it's worth celebrating these days in which the web is still viewable in its simplest, earliest form.
 * [Chris Done - subset of Haskell, aimed at aiding teachers teach Haskell](https://github.com/chrisdone/duet)

```
 $ duet run demo.hs
(\x -> x + 5) (2 * 3)
(2 * 3) + 5
6 + 5
11
```
## Inspirational

* [Wallace Stevens - Thea idea of order at Key West](https://www.poetryfoundation.org/poems/43431/the-idea-of-order-at-key-west)
>  The sea was not a mask. No more was she.
The song and water were not medleyed sound
Even if what she sang was what she heard,
Since what she sang was uttered word by word.
It may be that in all her phrases stirred
The grinding water and the gasping wind;
But it was she and not the sea we heard.

## Observations

  * Explaining Haskell datatypes and constructors to a buddhist/yogi
by using the chakra system and seed sounds of each chakra analogy:

```
soundToChakra :: Sound -> Chakra
soundToChakra = \x ->
  case x of
    Lam -> Muladhara
    Vam -> Svadishtana
    Ram -> Manipura
    Yam -> Anahata
    Ham -> Vishuddha
    Aum -> Ajna

stringToSound :: String -> Maybe Sound
stringToSound = \x ->
  case x of
    "lam" -> Just Lam
    "vam" -> Just Vam
    "ram" -> Just Ram
    "yam" -> Just Yam
    "ham" -> Just Ham
    "aum" -> Just Aum
    _     -> Nothing
```
  * What is interesting is that in Haskell you don't say, "well, maybe this or maybe
that" like I hear often people say in real life, and true, it does seem
ambiguous to use maybe for more possibilities than one or nothing at all. You
instead say "Well, maybe just this or nothing at all" and if you want to express
more outputs, or results that involve something more than just nothing you use
"Well, either this or that". But even more, the `Maybe` is actually made of
"Just this or nothing at all" so there is always this layered approach to
composition of functions, of expressions. The `Maybe` itself is defined by
`Just` this or `Nothing` at all. Why? Well, It seems it is because we can call
it `Maybe` precisely because it somehow produces an *action* and a terminating
*non-action*; we can also visualise it as a parent node whose name is Maybe with
two children, named Just and Nothing. This intuitive tricky part is that Nothing
is not like Just, Just will eventually pass something while Nothing will just
say no to what ever is being passed on, simply declaring Nothing if or Maybe
gets something which it doesn't need, which is not defined explicitly. In
Haskell it seems each expression can have a name itself, so that it is possible
to connect it to anything else, while at the same time it is defined *totally*
or at least that is what we strive to do. In real life we sometimes use a name
for an expression and then offer an explanation of what we really mean. We even
might get irritated if we are asked to explain our *Maybe's*.

> Alice: "Are you gonna do it?"
Bob: "Maybe"
Alice: "What do you mean maybe?"
Bob: "Well I mean that I am going to just do it or I am going to `Just` sit back
and do `Nothing`.
Alice: "But Bob please if you can't do it then do the other thing please, you
can't `Just` sit back and do `Nothing`.
Bob: "Ok then, I am going to do `Either` this thing or that thing if you need it
so much, is that ok? *Just* give me some time.

Yet people often use the word maybe for more than one thing. You could maybe do
this, or you could maybe do that, or bla.. which does seem a bit *unprecise*.


```
fromEither :: Either Sound Chakra -> ThisOrThat
fromEither = \x ->
  case x of
    Left s -> This s
    Right c -> That c

toEither :: ThisOrThat -> Either Sound Chakra
toEither = \x ->
  case x of
    This s -> Left s
    That c -> Right c
```

```
Right Chakra == toEither (fromEither (Right Chakra))
That Chakra == fromEither (toEither (That Chakra))
```

So `That` is on our `Right` and `This` is on our `Left`. `That` contains
all the 6 chakras we have in our body and `This` contains all the sounds we
utter that work on each chakra. The intuitive distinction that might seem
hard to grasp is that we seem to have another layer when we visualise
the either containers, or better to say when we visualise the path from
chakra container to sound container. We cannot just visualise sound and chakra
flowing between but instead we must so to say go up the tree root using
two levels of Either to understand the whole structure, this needs to be done
in order for us to prove they all relate, that they are all *isomorphic*
to one another.

```
                   This                             That
                  6 sounds                        6 chakras
                    \             fromEither          /
                     \          /    |       \       /
                      \        /     |        \     /
                       \      /      |         \   /
                       Left          +           Right
                         \                       /
                          \                     /
                           \                   /

                                     *
                                     |
                                     |
                                     |
                                  toEither

                    Right Chakra == toEither (fromEither (Right Chakra))
                    That Chakra == fromEither(toEither (That Chakra))

                    Left Sound == toEither (fromEither (Left Sound))
                    This Sound == fromEither (toEither (This Sound))
```
> `Either sound chakra` is making a statement saying that we have *one* of two possible values of types x *or* y in it. It turns out that `Either`, or *sum types* more generally, have a very reasonable and useful *dual* construction in which we have *both x and y*. In Haskell, we can convey thia idea as well. Let's call it `Pair`.

`data Pair x y = MakePair x y`

That is, a pair of a Chakra and a Sound will have type `Pair Chakra Sound`.
So while with `Maybe` we had to just pick one or nothing at all, and in `Either` we could pick this or that, which were some real alternatives, in a `Pair` we have this notion of carrying both alternatives with us, they are together and we are not making a choice by choosing either this or that.
It does seem like `Maybe` relates somehow to the notion of `Identity` since it is just giving us the thing we ask while the Either resembles to a Boolean choice or either this or that, either though we could say `Maybe` as well resembles a boolean choice since Just could be understood as `true` and Nothing as `false`. It is interesting to find relations with basic mathematical functions. In that case `Either` could be understood as showing two truth values and yet we are only supposed to pick one, now that really resembles to `OR`! But bool values always pick just one so even though this thought process *smells* wrong it does bring benefit when learning Haskell to compare it with previous concepts.

> Quote from atop:
We say `Either` and `Pair` are fundamental because once we can group or tell
apart two things, we can group or tell apart as many things as we want.

Once we begin to extend our space of pairs we realize pairs are just like
containers of two. Can we imagine triples? Or quads? If we jump to a musical
analogy, a pair is a musical interval, a pair of two tones in any scale makes an
interval. Once we hear a tone it is like hearing a point in space, though it can
sound pretty we know very little about it since the whole sound is coming from
one source (aha but what when both sources sing the same tone? Where is the
source then?). To get back a pair of two tones is like a line, and a triplet of
three tones can be understood as a triangle, a musical chord, defining a space between three
tones. Technically speaking a musical chord is a cluster of three or more tones.
Let us add three tones, c, e, and g that will make a C chord

```haskell
add3tones :: (Tone, Tone, Tone) -> Tone
add3tones = \(c, e, g) -> c + e + g
```
--------------------------------------------------------------------

### Let vs Where

It seems the difference between let and where lies in the order of declaration.
When we want to first name the *things* we want to use in our action we use let
in the sense, let there be light, let there be this, let there be that and then
after we have declared all out *lets* we will begin an action like "ok now
create this". We could understand it like this as well:
```haskell
let there be a human called "Bob"
let there be a human called "Alice"
in "Alice" loves "Bob"

-- further

let human = "Bob"
let human = "Alice"
"Bob" loves "Alice"

-- further we ommit using let twice

let firstHuman = "Bob"
    secondHuman = "Alice"
in "Bob" loves "Alice"

{-
This was not valid Haskell code, but it is written like this in order to
increase conceptual understanding of let vs where expressions
-}

-- valid Haskell

let x = 2
    y = 3
in x + y
```

Now, when we use `where` we like to first define our action or an event happening
and then tell something about our actors who define the event itself. Something
like "where some event is happening, its components are this and that, or where
Alice loves Bob, Bob is human and Alice is human too." If we miss describing a
single component of our event the event will not compile, Haskell will not
accept our declaration. Notice we could define a component and not use it in our
action, but it will still go through, and the event will not fail, same as declaring "Alice",
"Bob" and "Hayka" and then telling "Alice" loves "Hayka", the program would run
even though "Bob" was left all by himself.

The above let expression can be written with where like:

```haskell
love = human1 + human3
  where human1 = "Bob"
        human2 = "Alice"
        human3 = "Hayka"
```
Let's use correct Haskell code for our love program. We will use '++' instead
of just '+' since '++' is used to add or *concatenate* words, aka strings
(of characters).

```haskell
love = human2 ++ " loves  " ++  human3
  where human1 = "Bob"
        human2 = "Alice"
        human3 = "Hayka"

-- and our previous number example with let

add = x + y
  where x = 2
        y = 3
```
### More of where

Let us use the where in a slightly developed example, this is a variation on
"Haskell from first principles" chapter 3 exercise, but we will use first and last
name instead of hello world. The difference is in two examples is that we will
abstract our name into first and last while in the book "hello" and "world" is
used for expression and for the function name as well.

```haskell
module MyName where

{- before we define our name, I am just using my own here,
   we say that myName is a type of a string, as in a string of characters,
   then we define myName by concatenating the first and the last name. We are not
   explicitly mentioning first and last name, but instead just providing the value
   of the first and last name, which is "domagoj" and "miskovic"
-}

myName :: String
myName = "domagoj" ++ " miskovic"

-- but now we will declare the firstName is "domagoj"
firstName :: String
firstName = "domagoj"

-- and last name as "miskovic"
lastName :: String
lastName = "miskovic"

{- and now we will print out the myName and then we will link together
   our firstName and lastName into one expression by calling our firstName and
   lastName and Haskell should print out the values we provided before when we
   run the program.
-}

main :: IO ()
main = do
  putStrLn myName
  putStrLn firstAndLast
  where firstAndLast =
    concat [firstName, " ", lastName]
```
We introduced `firstAndLast` function with a `where` which was not maybe
necessary. We could have just linked together before declared firstName
and lastName without the `where` by writing `putStrLn (concat [firstName, " ", lastName])`
but in Haskell, even from simplest examples, one is *practicing* composability,
abstracting from the tiniest to the grossest. Since we learned `where` which
is like a basic tool for abstracting, describing our processes, like separating
the what and the who, we should use it in our exploratory learning and try to apply
it when ever we can. Here is a simple sum of squares example with where:

```haskell
-- sumOfSquares.hs

sumOfSquares x y = square x + square y
  where square n = n * n
```
Let us use lambda for our Sum of Squares

```haskell
module SOS where

sumOfSquares :: Integer -> Integer -> Integer
sumOfSquares = \x y -> square x + square y
  where square = \n -> n * n
```
Here also, we could have just used `sumOfsquares x y = x*x + y*y` instead. But
there is something else too, there are too many squares, square this square that,
though as a learning aid repetition is very good. Little children like to
repeat things they learn, so do we.

---------------------------------------------------------------------------
### Scope

Note:
Haskell from first principles builds intuition by first going through various
syntax examples and then plays with local and top level definitions by using
`let` and `where` as basic tools for abstraction. What we notice is the importance
of *spacing* in Haskell code, from *whitespace* that has a silent like mysterious
*apply* function because a simple `f x` meaning `f` is *applied* to `x`, to the
silent matrix like grid system or invisible columns and rows define the
play between local and *global* or *top-level* definitions. Global might be a
*wrong* word so we use top-level which brings us somewhat closer to the code at hand,
and a bit away from all expansive *global* notions.

a top-level definition is basically like a tree and local definitions are like
branches which have a life of their own within that same tree.
```
         /-----
   + ===[ ------
   |     \-----
   |
   |
   |     /-----
   + ==={------
         \_____
```
--------------------------------------------------------------------------------
Sometimes in real life conversations you will listen to two people talking,
and one of them will ask a question to another but you will not really know what
they mean by it. The second person might already answer it and you will still ponder
what the first person really meant. Then later when the second person leaves you
ask the person who was answering: "Hey what did she mean with that question? I
did not understand it." "Oh, you know sometimes she talks like this, she knows
I know what she really means so she just cuts out the whole question and asks me
implicitly."

What basically happened is that I was *left out* of the *scope* of the
conversation so I could not put together what they meant by it. They were talking
about local definitions without providing explicit values to those definitions
so I was left out wondering what those values were. Unfortunately sometimes
people get angry too when you do not ask them explicitly something but *force*
you to provide explicit local definitions, remebering something they wish to tell
. People use these aggresive tactics sometimes. Somebody might tell you:
"Hey, when are you going to cut your hair!?" What they mean by it it that your
hair is too long but they do not explicitly say it out loud. You might play naive
and ask them, well I do not know, why do you ask? Then the other person expects
you to *know* what they really mean and that is that your hair is too long and that
you should cut it. Unfortunately these situations sometimes happen in various
shapes and sizes.

------------------------------------------------------------------------------

### To String or not to String

Let's go through some of the exercises from chapter 3 in HaskellBook and try
out basic abstractions. We would like to append or concatenate two separate strings into one string.
Our first string will be "Curry is awesome" and our second string will be an exclamation mark '!'.
Our '!' exclamation mark is actually a character, and characters are embraced by single quotes `' '` while actual strings
which are a collection of characters are embraced by double quotes `" "` so a letter a is written as `'a'` while a
string abcd is written as `"abcd"`. Now, can we write a character a as a string by putting double quotes around it, as in
`"a"`. Yes we can. But how is that possible?

```haskell
> "a" == 'a'
..error:
    Couldn't match expected type '[Char]' with actual type 'Char'

> "a" == ['a']
True

> [' ', 'a', ' ']
" a "

> " a " == "a"
False
```
Hmm.. what is actually happening here? Let us try appending two chars, `a` and `b` and
at the same time check with `==` sign if our expression is true

```haskell
> appendTwoChars = "a" ++ "b" == "ab"
> appendTwoChars
True

-- by trial and error we see that

> appendTwoChars = 'a' : 'b' : [] == "ab"
> appendTwoChars
True
```

Looking at this we could say that a string is a *list* of characters, represented by square brackets `[ ]`
while characters have only single quotes meaning double quotes are *like* single quotes embraced by a bracked.
```haskell
> ['a'] == "a"
True
```
Ok, so back again to our "curry is awesome" and "!" example. Let's concatonate them and abstract these examples into a
function that will concatenate any two strings.

```haskell
> concatenateTwoStrings = ("Curry is awesome" ++ "!") == "Curry is awesome!"
> concatenateTwoStrings
True

> concatenateTwoStrings firstString secondString = firstString ++ secondString
> concatenateTwoStrings "first" " second"
"first second"

-- using beloved lambda

> concatenateTwoStrings = \firstString secondString -> firstString ++ secondString
> concatenateTwoStrings "hello" " world"
"hello world"
```
### Types

Though usually first basic dataype that is introduced in Haskell books is the
`Bool` datatype we will start with something different, which might bring our
intuition *closer* to Bool. It is important to note that while HaskellBook begins
the whole *type* story with introducing `data Bool = False | True`, same as
Graham Hutton's *Programming Haskell*, *A Type of Programming* introduces types
as four seasons of the year.

```haskell
data Bool = False | True

data Season = Winter | Spring | Summer | Fall
```
We too will define a `Human` datatype with its two data constructors `Male` or
`Female`

```haskell
data Human = Male | Female

-- let's change some genders, for now just keeping it as a binary choice for
-- making things simple

changeGender :: Human -> Human
changeGender Male = Female
changeGender _    = Male

> changeGender Male
Female
> changeGender Female
Male
```

We are using *or* instead of *and* because the `|` symbol,
the so called Pipe, indicating logical disjunction "or", indicates that
this is a *sum type*. So a human can be a female or a male but not
both at the same time. But is this the only way we can write data declarations?
How about using *and* and various *what if* this and that happens relations. Types
are never so pure in real life, and we often need to ponder before we implement
a solution. Seems to me a Haskeller can spend an eternity just pondering basic
questions. "A type!" a haskeller says, "What the hell is a type?"
It is important not to over think the type relations but somehow mix and match
while progressing in our composition. The good part is that type driven design can eliminate many failures that can
happen along the way, and in turn enable us to deal with future turns with much
greater success. HaskellBook tells this in a very clear way by describing the
basic flow of a data type declaration.

> The whole thing is called a data declaration. Data declaration do not always
follow precisely the same pattern - there are datatypes that use logical
conjuction (*and*) instead of disjunction, and some type constructors and
data constructors may have arguments. The thing they have in common is the keyword
`data` followed by the type constructor (or name of the type that will appear in
type signatures), the equals sign to denote a definition, and then data constructors
(or names of values that inhabit your term-level code).

```haskell
data Mood = Happy | Sad

changeMood :: Mood -> Mood
changeMood Happy = Sad
changeMood _     = Happy

> changeMood Happy
Sad
> changeMood Sad
Happy
```
### Five Types of Buddhas

Playing with *sum types* and five Buddha families

```haskell

data Buddha = Vairocana
        | Amoghasiddhi
        | Amitabha
        | Ratnasambhava
        | Akshobhya

data Color = White | Green | Red | Gold | Blue
data Element = Space | Air | Fire | Earth | Water
data Cardinality = Center | North | West | South | East
data Stress = Ignorance | Jealousy | Selfishness | Pride | Aggresion
data Season = None | Summer | Spring | Autumn | Winter
data Wisdom = Meditation | Perfection | Observation | Equanimity | Reflection
data Symbol = Wheel | Vajra | Lotus | Jewel | Sceptre
data Means = Turn | Protect | Magnetize | Enrich | Pacify

colorOfBuddhas :: Buddha -> Color
colorOfBuddhas = \x ->
  case x of
    Vairocana -> White
    Amoghasiddhi -> Green
    Amitabha -> Red
    Ratnasambhava -> Gold
    Akshobhya -> Blue

seasonOfBuddhas :: Buddha -> Season
seasonOfBuddhas = \x ->
  case x of
    Vairocana -> None
    Amoghasiddhi -> Summer
    Amitabha -> Spring
    Ratnasambhava -> Autumn
    Akshobhya -> Winter

wisdomOfBuddhas :: Buddha -> Wisdom
wisdomOfBuddhas = \x ->
  case x of
    Vairocana -> Meditation
    Amoghasiddhi -> Perfection
    Amitabha -> Observation
    Ratnasambhava -> Equanimity
    Akshobhya -> Reflection
```
This looks clear and yet we feel something missing. We would like to now define
more complex constructors, and we will get back to this when we understand a bit
more about `Either` and `Maybe` which will *enrich* our flow of types. I showed
this chart to a person who knows something about these Buddhas and yoga and
intuitively the person understood the flow of types. There five families are also
a nice way to put various groups of five together and think on various functions
between them.

------------------------------------------------------------------------------

### Addition Curry

```haskell
addOne :: Natural -> Natural
addOne = \x -> x + 1

addTwo :: Natural -> Natural -> Natural
addTwo = \x -> addOne (addOne x)


                         addTwo
             -----------------------------------
           /                /                  /\
          /                /                  /  \
         /    addOne      /       addOne     /
         -----------------  -----------------
        | \x    ------    || \x     ------   |
        |      |      |   ||       |      |  |
 x ----------->|  +   |----------->|  +   |----------->
        | 1--->|      |   ||  1--->|      |  |
        |       ------    ||        ------   |
        |                 ||                 |
         -----------------  -----------------

```
Our `addOne` can be also understood an an *incrementor* function because it
*increases* or increments the number we give it to. So if we give a number 0 to the
incrementor function it gives us back the number 1, then if we give it the number 1
it gives us back the number 2; we can give it any number and it will always give back *one*
number back. You could also say it *sends* the number it recieves to the next one.
It is a very simple function. What does *simple* mean? Maybe better to use the
word *primitive* or *small* in the sense that it is easy to understand conceptually.
Simple is a *complex* word because people have different notions on what
is simple and what is complex.

The opposite of our increment function would be a function that would send the
number we provide to the previous number, so if we gave it a number 3 it would give
us the number 2. It decreases the number we give it to, so we will call it the
decrement function. So within our natural number realm our increment function
behaves the same as our `addOne` function because it *adds* a number 1, and our
decrement function behaves the same as the function `subtractOne` which
*subtracts* a number 1 from the one we give it to.

```haskell
addOne :: Natural -> Natural
addOne = \x -> x + 1

subtractOne :: Natural -> Natural
subtractOne = \x -> x - 1

-- rewritten as increment and decrement would be

incr :: Natural -> Natural
increment = \x -> x + 1

decr :: Natural -> Natural
decrement = \x -> x - 1

-- hmm can we make these functions  to add any number we want?

incAny n = \x -> x + n
decAny n = \x -> x - n

-- Maybe you haven't applied a function to enough arguments?

incAny x n = \x -> x + n
decAny x n = \x -> x - n

-- Maybe you haven't applied a function to enough arguments?

incAny x n = \x n -> x + n
decAny x n = \x n -> x - n

--  Maybe you haven't applied a function to enough arguments?

incAny = \x n -> x + n
decAny = \x n -> x - n

> incAny 3 4
7
> decAny 3 4
-1
```
> Addition is a function that maps two natural numbers to another one.

Let's see different ways we could add two numbers using our incrementor and
decrementor function

```haskell
addition = \x y -> (increment x) + (decrement y)

> addition 3 4
> (increment 3) + (decrement 4)
> ((\x -> x + 1) 3) + ((\x -> x - 1) 4)
> ([x=3] 3 + 1) + ([x=4] 4 - 1)
> (3 + 1) + (4 - 1)
> 4 + 3
> 7

addition = \x y -> 1 + ((decrement x) + y)

addition 3 4
1 + ((decrement 3) + 4
1 + ((\x -> x - 1) 3) + 4
1 + ([x=3] 3 - 1) + 4
1 + (3 - 1) + 4
1 + 2 + 4
1 + 6
7

> :type addition
addition :: Integer -> Integer -> Integer
```
Now let's expand our calculation with more steps.

```haskell
addition = \x y -> (increment x) + (decrement y)
addition 3 4
  = (\x y -> (increment x) + (decrement y)) 3 4
  = (increment 4) + (decrement 3)
  = (increment 5) + (decrement 2)
  = (increment 6) + (decrement 1)
  = 7 + 0
  = 7

addition = \x y -> 1 + ((decrement x) + y)
addition 3 4
  = (\x y -> 1 + ((decrement x) + y)) 3 4
  = 1 + ((decrement 3) + 4)
  = 1 + (1 + (decrement 2) + 4)
  = 1 + (1 + (1 + (decrement 1) + 4))
  = 1 + (1 + (1 + 0 + 4))
  = 1 + (1 + (1 + 4))
  = 1 + (1 + 5)
  = 1 + 6
  = 7
```
I still can't explain how but I find the second version so pretty,
it remembers its steps, as in takes much more care when calculating, it
just seems more intelligent way to add things, even though it requires more
steps. I always would find it strange when people talk about something taking less time as being better. I could see my mind stopping and pondering why would that be true, why would optimizing have this guideline which seems so narrow. Yes, there are valid explanations and reasons for this but still, the notion of embedding more information within each level
of your computation somehow seems much more intelligent approach in the long run. Who knows maybe artificial intelligence is precisely one long extensive computation where each step is carrying a billion other steps all in sync so why not explore that direction instead. Well, for more
input on these increment and decrement function check the amazing SICP lectures by Abelson and Sussman. These are just pure awesomeness, I have yet to write about them and how they helped in motivating this functional pursuit into haskell. Who knows, maybe Lisp lurks somewhere in Haskell heart too.

Addition is associative `((x + y) + z = x + (y + z)` meaning if three people
x y and z want to go out of the house, no matter who comes out first by the time
all three come out of the house there will be three of them out of the house.
Also notice if you haven't actually witnessed these people coming out of the
house how can you tell who came out first and what actual difference does it
make? If we the three of us are going to a party and all three of us come out of
the house to take the bus, there is no difference who comes out first or who
goes in the bus first since all three of us are going to the party anyway. Well
maybe it does matter for some mysterious spooky action at a distance reason, but
our coming out of the house is an associative operation. Also when we have to
pay for the ticket 10 dollars, it makes no difference if we give the driver
first a 5 dollar note and then a 2 dollar notes and then a 1 dollar note or if
we give the driver the money in some other order. We still have to pay 10
dollars. But these are *simple* examples. I'm thinking on what would be an
associative operation in music, as in if I have a chord of three notes does the
order matter when I play them? The similar idea to our three little friends
would be if I had to play the three notes at the same time, then there would be
no difference, but still, then I cannot see the whole process of two separate
groupings of three notes. Still, you could visualise *giving* more awareness to
some two notes within a three note chord and they will then sound a bit
different even though you will play them at the same time. *This* is one of the
key features of great musicians that even when they play three notes at the same
time, they never really play them in the same way, they often do subtle
movements that somehow *increment* your perception of music. These movements are
so subtle they could be barely described as movements and still, if you are
attentive you can hear them.

------------------------------------------------------------------------------

> In Mathematics, the **associative property** is a property of some binary
operations. Within an expression containing two or more occurrences in a row of
the same associative operator, the order in which the operations are performed
does not matter as long as the sequence of the operands is not changed.

> Associativity is not the same as commutativity, which addresses whether or not
the order of two operands changes the result.

Seems to me my whole previous rambling was describing *commutativity* and not
*associativity*! But they do seem awfully similar:

```
x + (y + z) = (x + y) + z               x + (y + z) = (x + y) + z
1 + (2 + 3) = (1 + 2) + 3               1 + (1 + 1) = (1 + 1) + 1
1 + 5 = 3 + 3                           1 + 2 = 2 + 1
6 = 6
```
Still what seems clear from this is we can *shift* things around, we can commute
them around and we get the same result while with associative property we can
group things differently but the operations we apply to them will not change the
result. Why does this require so much pondering? There is this
smell that associative properties and commutative properties can change and
often seem intermixed depending on the operations we use with them.

> All functions in Haskell take one argument and return one result. This is
because functions in Haskell are nested like Matryoshka (Babushka) dolls in
order to accept "multiple" arguments. The way the (->) type constructor for
functions works means `a -> a -> a` represents successive function applications,
each taking one argument and returning one result. The difference is that the
function at the outermost layer is returning *another* function that accepts the
next argument. This is called *currying*.

Our `addTwo` was defined for just two input numbers. What if we provide three
numebrs to add?

```haskell
> 1 + 2
3
> 1 + 2 + 3
6
> (+) 1 2
3
> (+) 1 2 3
error:Non type-variable argument in the constraint..
-- hmm..
```
```scheme
;; this is written in lisp, a functional language but here plus
;; is applied to every argument when written in prefix notation.

> (+ 1 2)
3

(+ 1 2 3)
6

(+ 1 2 3 4 5)
15
```
Let's get back to Haskell. Though it is possible to put plus before the
arguments similar to lisp it does not really work the same way, it still somehow
takes only two arguments. Is this limited? Not really, what I understand now is
that the plus function itself is defined minimally in Haskell, like the very
essence of plus is defined as something that adds *at least* two things, but
to go even further in Haskell, only two things! If you need to add three things
then use two pluses, use two functions to add three things. If we
wanted to apply plus to more than two arguments without defining how many, it
seems we would need a function that would *map* somehow this plus to any number of
numbers we want. This seems like we actually need two functions to get a similar
behaviour like in lisp example. This does seem super cool, meaning functions in
Haskell are interlinked somehow each having one input and one output. Even our
imaginary *mapping* function would not really add a million numbers but would tell
plus to add a million numbers, orchestrating the event coordinating many little
pluses to do the addition, collapsing the numbers like dominoes.
So how is lisp then just adding many numbers by having a plus function at
the beginning of the expression? Ponder the pond..

> The way the type constructor for functions, (->), is defined makes currying the default in Haskell. This is because it is an infix operator and right associative. Because it associates to the right, types are implicitly parenthesized like so:

```haskell
f :: a -> a -> a

-- associates to

f :: a -> (a -> a)

-- and

map :: (a -> b) -> [a] -> [b]

-- associates into

map :: (a -> b) -> ([a] -> [b])
```
> The association here, or grouping into parentheses, is not to control precedence or order of evaluation; it only serves to group the parameters into argmuents and results, since there can only be one argmuent and one result per arrow...

But this very technical desription of this action does not fully explain this behaviour except by mapping out every movement into words. What I do realize from previous pondering is that as with our plus sign visually it is supposed to be right associative merely because the result is the last thing we are going to get if the function completes so the result is *embraced* with the last argument meaning the last function is going to take the numbers given to it by all the previous functions and give us the result. At least this idea helped me to deepen the understanding of this movement. Notice also the explanation says that

> This is because it is an infix operator and right associative..

Now the expression is saying that something happens because something is like this or that. Such definitions only provoke further questions but why is is like that then? Next it continues with again just repeating the previous expression with
> Because it associates to the right..

And again I know and see it associates to the right but why is the word *because* used to explain something by just repeating what was already said? And that is why it is parenthesized like that. But I already see that myself, what idea is actually expressed here? I am sure many might stop and ponder what this really means. By thinking on functions that only pass on what was given and at the same time do just a small calculation that in whole creates the final result we can see that it might be obvious that it will be parenthesized to the right. Again it is difficult to put this thinking flow into words since words are so vague and well, code is really explicit. Maybe that is why I am pondering so much on this plus sign, associativity and commutativity.

```haskell
> map (+) [1,2,3,4]
error

> map (1+) [1,2,3,4,5]
[2,3,4,5,6]

-- hmm
```

### Mappings

> Consider the expression `7 - 4 + 2`. The result could be either `(7 - 4) + 2 = 5` or `7 - (4 + 2) = 1`. The former result corresponds to the case when `+` and `-` are left-associative, the latter to when `+` and `-` are right-associative.

When I type this expression into ghci or an android calculator I get 5 as the result.

> A mapping function applies a given function to each element of a list or other collemtion.
```elisp
(mapcar 'car '((a 1) (b 2) (c 3)))
    => (a b c)

(mapcar 'cdr '((a 1) (b 2) (c 3)))
    => (1 2 3)

(mapcar 'string "abc")
    => ("a" "b" "c")
```

How is this mappping `mapcar` defined in emacs lisp?

```elisp
(defun mapcar (function &rest args)
  "Apply FUNCTION to successive cars(heads) of all ARGS.
   Return the list of results."

  ;; If no list is exhausted,
  (if (not (memq nil args))

       ;; apply function to cars.
       (cons (apply function (mapcar 'car args))
             (apply 'mapcar function

                     ;; recurse for rest of elements
                     (mapcar 'cdr args)))))

(mapcar 'cons '(a b c) '(1 2 3 4))
     => ((a . 1) (b . 2) (c . 3))
```
Compare the elisp mapcar with the Haskell definition of `map`

```haskell
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs
```
We notice that from line `f x : map f x` the `f x` looks like `(mapcar 'car args))` and `f xs` looks like (mapcar 'cdr args). `car` in lisp lingo is actually same as haskell's `first` and `cdr` is same as `rest`, meaning `x` and `xs` are written in the same style, like saying after the `first beginning` come many more `beginningS`. Actually after the first comes always the rest, no matter how many or in which way they appear. There wouldn't be any firsts if there were no rests, right?

In Haskell they like to use variable names like `n` and `ns` for `number` and `numbers` and some more polymorphic like variables for `x` and `xs`

Notice also that `cons` from elisp definition is the cons operator which is represented in haskell as *infix* operator `:` stuck between `f x` and `map f xs`. Also elisp mapcar definition first says `(if (not (memq nix args))` which relates to haskell's `map _ [] = []`. Haskell seems to be just saying if anything, meaning `_` as an empty placeholder which is used in pattern matching, is provided to an empty `map _ []` then return an empty list `[]` Why? Because we have nothing to do with an empty list. We could define an amazing function but if we do not provide any arguments to it nothing will happen, because an empty list has no members, it is just empty. And out of an empty list input how would we create a *filled* list of an output? This imaginary function would have to be some sort of an artist that would create something out of nothing, worth pondering about..

```haskell
map f (x:xs) = f x : map f xs

map (+3) [1, 2, 3, 4, 5]

[(1 + 3), map (+3) [2, 3, 4, 5]]
[(1 + 3), (2 + 3), map (+3) [3, 4, 5]]
[(1 + 3), (2 + 3), (3 + 3), map (+3) [4, 5]]
[(1 + 3), (2 + 3), (3 + 3), (4 + 3), map (+3) 5]
[(1 + 3), (2 + 3), (3 + 3), (4 + 3), (5 + 3)]
[(1 + 3), (2 + 3), (3 + 3), (4 + 3), 8]
[(1 + 3), (2 + 3), (3 + 3), 7, 8]
[(1 + 3), (2 + 3), 6, 7, 8]
[(1 + 3), 5, 6, 7, 8]
[4, 5, 6, 7, 8]
```
Let's see how map is defined in another functional lisp dialect, Clojure

> Returns a lazy sequence constisting of the result of applying f to the set of first items of each coll, followed by applying f to the set of second items of each coll, until any one of the colls is exhausted. Any remaining items in other colls are ignored. Function f should accept number-of-colls arguments. Returns a transducer when no collection is provided.

``` clojure
(map inc [1 2 3 4 5])
;;=> (2 3 4 5 6)

(map + [1 2 3] [4 5 6])
;;=> (5 6 7)
```

From [Brave Clojure](https://www.braveclojure.com/core-functions-in-depth/)
> I think of abstractions as named collections of operations. If you can perform all of an abstraction's operation on an object, then that object is an instance of the abstraction. I think this way even outside of programming.

This seems like a nice thinking process for building *something like* abstractions but I feel this notion of an abstraction being a named collection of operations should also have a unique property, the result which transformed the collection itself. This abstraction principle does not have to go both ways just by unfolding the operation which was applied to the collection. In some sense, you could abstract the mapping of pluses to a completely different operation that would in turn do the addition as a side effect while just mapping pluses to an arbitrary number of operands does seem like a trivial form of abstraction principle.

> The prejudice and respect for abstract thinking are so great that sensitive nostrils will begin to smell some satire or irony at this point

Seems to me Hegel would be a wonderful Haskeller, this short description somehow relates in a nice way to our mapping definitions, just think of the difference between a total and a partial function:

> For Hegel, only the whole is true. every stage or phase or moment is partial, and therefore partially untrue. Hegel's grand idea is "**totality**" which preserves within it each of the ideas or stages that it has overcome or subsumed. Overcoming or subsuming is a developmental **process** made up of "moments" (stages or phases). The **totality** is the **product** of that process which preserves all of its "moments" as elements in a structure, rather than as stages or phases.
