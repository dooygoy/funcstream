
## Observations

* Explaining Haskell datatypes and constructors to a buddhist/yogi
by using the chakra system and seed sounds of each chakra analogy:

```haskell
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


```haskell
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

```haskell
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

```haskell
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

-------------------------------------------------------;

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

```haskell
         /-----
   + ===[ ------
   |     \-----
   |
   |
   |     /-----
   + ==={------
         \_____
```

-----------------------------------------------------;

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

---------------------------------------------------;

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

--------------------------------------------------;

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

----------------------------------------------------;

> In Mathematics, the **associative property** is a property of some binary
operations. Within an expression containing two or more occurrences in a row of
the same associative operator, the order in which the operations are performed
does not matter as long as the sequence of the operands is not changed.

> Associativity is not the same as commutativity, which addresses whether or not the order of two operands changes the result.

Seems to me my whole previous rambling was describing *commutativity* and not
*associativity*! But they do seem awfully similar:

```haskell
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

> All functions in Haskell take one argument and return one result. This is because functions in Haskell are nested like Matryoshka (Babushka) dolls in order to accept "multiple" arguments. The way the (->) type constructor for functions works means `a -> a -> a` represents successive function applications, each taking one argument and returning one result. The difference is that the function at the outermost layer is returning *another* function that accepts the next argument. This is called *currying*.

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
>
-------------------------------------------;

### Viewing type signatures

In chapter five exercises page 221 of HaskellBook
it is asked to figure out how the type would change and why, and make a note of what we think of the new inferred type would be.

```haskell
-- Type signature of general function

(++) :: [a] -> [a] -> [a]

-- How might that change when we apply
-- it to the following value?

myConcat x = x ++ " yo"
```
We will get `myCOncat :: [Char] -> [Char]` but what is interesting that somehow the type did not seem to change but only the frame of reference from which the signature is being observed, as in first we saw the type signature by unfolding the `(++)` which said that it concatenates two *things* to get another *thing*, and then we see the `myConcat` function which is made out of concatenating a thing with some other thing called " yo". The first frame is seeing just the `(++)` and how it *interlinks* `[a]` with `[a]` into an `[a]` while the second frame is seeing the same `(++)` but this time we are witnessing a type of a function which is using the `(++)` to interlink two `[Char]`s. That's why we have only two `[Char]` and not three `[a]`s like in the pure `(++)` signature. Did the type really change as the exercise says it does? I find this actually confusing because the viewpoint seems to change while the signature stays the same. Let's try with the next one.

```haskell
(*) :: Num a => a -> a -> a

myMult x = (x / 3) * 5

:t myMult
myMult :: Fractional a => a -> a
```
And here we witness that the `myMult` has a signature which seems to be an *instance* of the more general `Num a` typeclass. Again we see the conversion of the view, in the first type signature the `(*)` is being seen as an operator that interlinks from one to the second which gives the third while our `myMult` function is some fractional instance or a result that is being interlinked from two variables `a` and `a`. We can intuitively understand that a fractional number is an instance of a larger family of numbers. After all we do say a fractional *number* and Haskell's type system works in the same way, *type classes* are like large families of instances of minor type families which are often automatically inferred by Haskell, inferred meaning Haskell can sign our functions with appropriate types. But it cannot do that for all. If we use our Buddha example from before Haskell would not know anything about the Buddha typeclass and all the instances of deities, buddhas, sages, etc that belong to the Buddha typeclass.

In the next example we can see this *changing* view even better.

```haskell
(>) :: Ord a => a - > a -> Bool

myCom x = x > (length [1..10])

:t myCom
myCom :: Int -> Bool
```

Here it seems that the `myCom` signature is telling us just the beginning and the end of our function which is defined as some operation that takes an `Int` and produces a `Bool` while the `(>)` signature has nothing at all to do with this function because it is defined just by itself, how it an instance of some Order typeclass apparently having something to do with ordering things and that it will take something and compare it with something else, finally producing a `Bool` which will tell us `True` or `False`, meaning if the first thing we provide it is trully greater than the second thing. `myCom` on the other hand is just saying: "Hey I'm some function that is going to tell you if your number is true or not". Seems to me we cannot really see what is happening from this type signature `myCom :: Int -> Bool`, what is this type actually doing?

### Credit Card Digits

* CIS 194: Homework 1

When solving the homework, strive to create not just code that works, but code that is stylish and concise. Try to write small functions which perform just a single task, and then combine those smaller pieces to create more complex functions. Don't repeat yourself: write one function for each logical task, and reuse functions as necessary.

## Validating Credit Card Numbers

How do websites validate your credit card number when you shop online? They don't check a massive database of numbers, and they don't use magic. In fact, most credit providers rely on a checksum formula for distinguishing valid numbers from random collections of digits.

> What is a checksum?

Wiki: [Checksum](https://en.wikipedia.org/wiki/Checksum)

A **checksum** is a small-sized datum derived from a block of digital data for the purpose of detecting errors that may have been introduced during its transmission or storage. By themselves, checksums are often used to verify data integrity but are not relied upon to verify data authenticity. The procedure which generates this checksum is called a **checksum function** or checksum algorithm. Depending on its design goals, a good checksum algorithm will usually output a significantly different value, even for small changes made to the input. 

In this section, you will implement the validation algorithm for credit cards. It follows these steps:

* Double the value of every second digit beginning from the right.
That is, the last digit is unchanged; the second-to-last digit is doubled; the third-to-last digit is unchanged; and so on. For example,
`[1,3,8,6]` becomes [2,3,16,6] 

* Add the digits of the doubled values and the undoubled digits from the original number. For example, `[2,3,16,6]` becomes `2+3+1+6+6 = 18`

:confused:

> Note: I do not understand how is this calculation done.

How am I supposed to add `[2,3,16,6]` and `[1,3,8,6]`

> Note: So 16 is not a digit?

[Numerical digit](https://en.wikipedia.org/wiki/Numerical_digit)

A **numerical digit** is a single symbol (such as "2" or "5") used alone, or in combinations (such as "25"), to represent numbers (such as the number 25) according to some positional numeral systems. The single digits (as one-digit-numerals) and their combinations (such as "25") are the numerals of the numeral system they belong to. For example,the decimal system (base 10) requires ten digits (0 through to 9), whereas the binary system (base2) has two digits (e.g.: 0 and 1)

> Note: add the digits of the doubled values and the undoubled digits from the original number.

so `[2,3,16,6]` = `2 + 3 + 1 + 6 + 6 = 18`
and `[1,3,8,6]` = `1 + 3 + 8 + 6 = 18`

oh wow now I see, this is cool, never seen this one before.

* calculate the remainder when the sum is divided by 10. `rem 18 10 => 8`

### Exercise 1

We need to first find the digits of a number. Define the functions.

```haskell
toDigits    :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]

-- toDigits should convert positive Integers to a list of digits.
-- for 0 or negative inputs, toDigits should return the empty list.
-- toDigitsRev should do the same, but with the digits reversed.
```

> Note: So it is about the positional system, we somehow need to decompose a decimal number into its digits, aka it's tenths, hundreds?, thousands,.. example: a number 12345 into 5, 45, 345, 2345.. at least that is the first idea. Later we have to somehow add single digits, for now be begin to extract the last digit.

```haskell
lastDigit :: Integer -> Integer
lastDigit = (`mod` 10)
-- this will give us the last digit, or the 10th column
-- for example to get 100ths or 1000ths we would done
lastDigit 1234 
4

lastTwoDigits = (`mod` 100)
lastTwoDigits 1234
34

-- or 
lastThreeDigits = (`mod` 1000)
lastThreeDigits 1234
234 

-- also
divMod 1234 10
(123,4)

-- now check this magic

dropLastDigit :: Integer -> Integer
dropLastDigit = (`div` 10)

dropLastDigit 1234
123
```

So our dropLastDigit and lastDigit are like basic operators on lists!
Usual haskell tutorials mention these but too bad there are no examples like the credit card because here we have extraction of digits from a single number which at first seems like a difficult problem. What are our list operators? `init` and..`last`

-- have to put tea :)
-- gosh, still have to learn vim..

```haskell
head [1,2,3,4]
5

tail [1,2,3,4]
[2,3,4]

last [1,2,3,4]
4

init [1,2,3,4]
[1,2,3]
```

Next we have to write a function that splits a number into its digits in reverse order called, `toDigitsRev`

```haskell
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = lastDigit n : toDigitsRev (dropLastDigit n)
```

> But how to just get digits of some number in Haskell?

[stack overflow question](https://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell)

```haskell
135 `div` 10 = 13
135 `mod` 10 = 5
```

generalize into a function:

```haskell
digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

-- and reverse

toRevDigits :: Integral n => n -> [n]
toRevDigits 0 = []
toRevDigits n = n `mod` 10 : toRevDigits (n `div` 10)
```

* [ ] - Check more interesting answers on stack on this question.

> Note: Notice lexi-lambdas solution is more elegant since the functions are already abstracted. Before beginning to just solve the function provided here we see two smaller functions composed into one. 

Next lexi-lambda observes:

> Sure enough, that fixed things. Still, something about me doesn't like the asymmetry between the two expressions on either side of the cons (is it still called that in Haskell?). I remember something about `where` clauses; let's try that.

```haskell
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = d : toRevDigits ds
     where d  = lastDigit n
           ds = dropLastDigit n
```

> Well, that seemed to work, but is it nicer...? I don't know. Now it just seems more verbose, honestly. I can't really decide..


(break) -- gotta do lunch..

:gem:
We see that easily we can decompose our thinking process into smaller and smaller functions, or maybe better to say our flow of thinking includes creating small functions chained together, each chain creating in turn a higher function, and so it continues. Sometimes, as in the first lexi-lambda example where we see only a partial application of `div` and `mod` abstracted into a unique function while in stack overflow answers these calculations are not abstracted but included in the final `toDigitsRev` function. What seems better? Am I to abstract partial applications as well? As a learning method this seems good. Chaining smallest possible entities into larger ones. How flexible is that actually? Is there some cognitive upper bound since there are many abstracted functions. This also brings to the point the qestion on naming functions. Our names should reflect the intention of the function. Also seems to me that abstracted functions are *easier* to read since there are visually *fewer* calculations. Hm.. 

End of **Exercise 1 and 2**

(break) -- gotta do lunch..

### Exercise 3

The output of `doubleEveryOther` has a mix of one-digit and two-digit numbers. Define the function: `sumDigits :: [Integer] -> Integer`
to calculate the sum of all digits. 
*Example*: `sumDigits [16,7,12,5] = 1+6+7+1+2+5=22`

So here as well we need to reuse the function `toRevDigits` to split the number into a digit seems to me.

```haskell
sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toRevDigits
```

[stack answer on concatMap](https://stackoverflow.com/questions/5225396/what-does-concatmap-do)

Yes, the `concatMap` function is just `concat` and `map` put together. Hence the name. Putting functions together simply means composing them:
`(.) :: (b -> c) -> (a -> b) -> a -> c`
However `concat` and `map` cannot be put together by simply using function composition because of the type signature of `map`:

```haskell
map :: (a -> b) -> [a] -> [b]
        ^^^^^^     ^^^    ^^^ 
           a        b      c
```

* [ ] - why is this understood as *a, b, c*, I do not really understand since this now seems to merely put visual blocks between arrows into another layer of variable letters?

Let's see concat, map, and concatMap in action!

```haskell
map (+1) [1,2,3,4]
> [2,3,4,5]
```

Will this work on strings too? Compare how `map` still keeps the list while `concatMap` somehow has a list without the commas in between, without the elements separated. What is the length of `"some string"`?

```haskell
map (++"!") ["one", "two", "three"]
> ["one!", "two!", "three!"]

concatMap (++"! ") ["one", "two", "three"]
> "one! two! three!"

length ["one!", "two!", "three!"]
> 3

length "one! two! three!"
> 16
```

As you can see function composition expects a function of type `a -> b`, but `map` is of type `a -> b -> c`. To compose `concat` with `map` you need to use the `.:` operator instead:
`(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d`

The `concat` function has a type signature of:

```haskell
concat :: [[a]] -> [a]
          ^^^^^    ^^^
            c       d
```

Hence `concat .: map` is of type:

```haskell
concat .: map :: (a -> [b]) -> [a] -> [b]
                 ^^^^^^^^^^    ^^^    ^^^
                      a         b       d
```

Which is the same as that of `concatMap`:
`concatMap :: (a -> [b]) -> [a] -> [b]`

* [ ] - the stack answer is very good, do check it out again because it includes also an interesting part where  if you `flip` the arguments of `concatMap` you get the `>>=` (bind) function of the list monad!

* [ ] - on next coding session do an exploration of this part and redo the previous calculations again. How is monad connected to this?

### Exercise 4

define the function `validate :: Integer -> Bool` that indicates whether an `Integer` could be a valid credit card number. This will use all functions defined in the previous exercises. 
*Example*: `validate 4012888888881881 = True`
*Example*: `validate 4012888888881882 = False`

> Note: now what bugs me is that I do not understand why is the first example True and the second example false? Immediatelly I thought about putting any number into the validate function that will extract the digits, reverse them and then concatenate and then.. but how does it actually know is some number is true if there is no action of comparing two credit card numbers? Confused :confused:

Off to google lands.. and stack overflow fields..

[codereview.stack on the same exercise](https://codereview.stackexchange.com/questions/176569/validating-credit-card-numbers-haskell)

Write functions in terms of other functions

You have written both `toDigits` as well as `toDigitsRev`. However, you only need one of them. THe other one is the `reverse`d variant:

```haskell
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev
```

> Note: Why did we define `teDigitsRev` in the first place?

The exercise does say "double the value of every second digit beginning from the right. That is, the last digit is unchanged; the second-to-last digit is doubled; the third-to-last digit is unchanged; and so on." But why are we starting from the right in the first place? What difference does it make to begin reading the card number from the last digit since we actually read from left to right? Shouldn't these kind of statements be explained in some terms or is this going too much into details? So calleds details, since these kind of *obscurations* may just interfere with the cognitive process. I do not understand why are we beginning from left to right so now I must investigate further why is the exercise stated in these terms. Thus, I must *go away* from the code yet again, and explore. While stumbling through these exercises and code explanations online we see that many just go over these details, which may seem superficial of super obvious to some people, and yet there are some *idiots*? like me who do wonder why are we suddenly reading the numbers from left to right. Also, often just basic operations, or syntactic operations are explained as in "look, this is some forrest, you can do this and you can do that, now go into the forrest and find me this or that" while another maybe much subtler explanation would be to take the student into the forrest and just tag along offering advice on each step, since there are many rabbit holes in which one can fall into or explore. Sometimes experienced Haskellers just go over these seemingly obvious steps while the rest of us stumble and even though we might just follow through what happen after a while is that not enough *spaced repetition* in learning was done so these details will melt away. Maybe then, when we reach higher planes of abstraction we will not be equiped with enough functional *lingo*, enough explored rabbit holes, to fully undertand all the varieties of solving specific problems. Ok, only in this exercise I realized basic operation on lists, taking out single elements, like taking the `head` or the `tail`, or taking out the `init` or the `whats the other one..` can be compared with this extraction of the digits from a number. In this example there is abstraction and composing of small functions and at the same time, we can understand with a much richer context the operations on lists themselves which might seem *boring* if we are just told, "hey this is how you get the first and the rest of the list". This might seem like a newbie ranting, but what I am aiming at is at defining a much richer context in which simple abstractions contain a higher level view, or like the exercise itself contains two higher level contexts which might enable one to better understand what is happening and how it all connects. We can see in the stack answer on `concatMap` which is a wonderful example how a seemingly simple composing of `concat` and `map` can take us all the way to the `Monad` itself. 

While learning Haskell I feel I have to explore each of these rabbit holes and then write about it too, just merely linking each Haskellers exploration but at the same time deepening my own exploration. Obviously this could be automated in the future where an AI could just in a wiki-like manner explore all *angles* of an exercise and tailor the deepening trajectory for each individual student depending on the progress. This could be modified in real time, so a student might progress on its own from a simple `concat` function to a monad. And no *book* would be the same since it would define itself by the student that interacts with it. Kinda like a *smartbook*.

Notice the finishing touch on lexi-lambda's [post](https://lexi-lambda.github.io/learning-haskell/day-1.html) on this exercise:

> Done. Well, that wasn't terribly interesting, though it was helpful to get used to working in the language again. I think I'll go through the next assignment, and if it doesn't get better, I might have to switch to something else. For now, though, I feel somewhat accomplished, despite having written an extremely trivial set of functions.

I am grateful for lexi's post, it helped and motivated this pursuit and I haven't felt progressing in Haskell like this since I started, at least not in practical terms. Everything up to this point has been exploration and now this exploration has somehow interlinked into this one exercise. But the bitter taste remains because I wish other Haskell books would look like simple from super abstract exploration within a single space, a single assignment that can transform itself into a whole field of language.. well I can't really put this into words for now, but I feel nice about the progression :)

To get back at our exercise, lexi wrote mysterious  `luhn` instead of `validate`. What is `luhn`? We are grateful for lexi's boon and we follow the `luhn` into the Haskell forrest of doom! :)

`search google.com -> luhn gives us Luhn algorith`

[Luhn algorithm](https://en.wikipedia.org/wiki/Luhn_algorithm)

The **Luhn algorithm** or **Luhn formula**, also known as the "modulus 10" or "mod 10" algorithm, named after its creator, IBM scientist Hans Peter Luhn, is a simple checksum formula used to validate a variety of identification numbers, such as credit card numbers, IMEI numbers, National Provider Identifier numbers in the United States, ... and some other.. notice: It is not inteded to be a cyptographically secure hash function; it was designed to protect against accidental errors, not malicious attacks.

> Note: Here we come to the "Why do be begin reading our card numbers from the right" question from before.

The formula verifies a number against its included check digit, which
is usually appended to a partial account number to generate the full account number. This number must pass the following test:

> Note: And here is the 'right to left' bit: This is similar to the exercise description. Would be cool if the exercise had this article linked too..

1. From the rightmost digit(excluding the check digit) and moving left, double the value of every second digit. The check digit is neither doubled nor included in this calculation; the first digit doubled is the digit located immediately left of the check digit. 

> Note: So we see that the digits not included in the calculation are called 'the check digit'. These are the digits we will not double.

If the result of this doubling operation is greater than 9 (e.g., 8 x 2 = 16), then add the digits of the result (e.g., 16: 1 + 6 = 7, 18: 1 + 8 = 9) or, alternatively, the same final result can be found by subtracting 9 from that result (e.g., 16: 16 - 9 = 7, 18: 18 - 9 = 9).

> Note: Ok so we see another way how to get digits from doubledigit numbers, this was not mentioned before.

Take the sum of all the digits. If the total modulo 10 is equal to 0 (if the total ends in zero) then the nmuber is valid according to the Luhn formula; otherwise it is not valid.

> Note: There are more complex algos such as 

Verhoeff and Damm algorithma and Luhn mod N algorithm is an extension that supports non-numerical strings. 

The algorithm appeared in a United States Patent for a hand-held, mechanical device for computing the checksum. Therefore, it was required to be rather simple.

> Note: I still need to wrap my head around this algo, it seems simple but the underlying logic is not super clear.. but we see that this exercise comes in Programming in Haskell from Graham Hutton. That's great!

* Check [this stack overflow on implementing the luhn in Haskell](https://stackoverflow.com/questions/40832422/luhn-function-in-haskell)

```haskell
luhn :: Int -> Int -> Int -> Int -> Bool
luhn w x y z = (luhnDouble w + x + luhnDouble y + z) `mod` 10 == 0
```

* Check [this stack overflow on luhn](https://stackoverflow.com/questions/42013519/luhn-algorithm-in-haskell)

```haskell
doubleAndSum :: [Int] -> Int
doubleAndSum = fst . foldr (\i (acc, even) 
               -> (acc + nextStep even i, not even)) (0,False)
  where 
    nextStep even i
      | even = (uncurry (+) . (`divMod` 10) . (*2)) i
      | otherwise = i

myLuhn :: Int -> Bool
myLuhn = (0 ==) . (`mod` 10) . doubleAndSum . (map (read . (: ""))) .show

testCC :: [Bool]
testCC = map myLuhn [.... , .... , .... , ...]
-- => [True, False, False, True]
```

> Note: Well this version is too verbose for me, currently beyond 'alien speak'. I am aware of the names of these functions but do not really understand how they relate to each other. But writing Haskell code out like reading a difficult piece of music seems like a good practice too.

There is also **Luhn** on Hackage, Haskell's repository of programs and libraries, the [source](https://hackage.haskell.org/package/luhn-0.2/docs/src/Luhn.html) is commented and seems nice to follow. Check it out, it is defined as a module and has test's as well. This one seems like the most comprehensible solution so far. 
