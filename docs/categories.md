
# Category Theory

## 1.2 What is a category

- Abstraction
- Composition
- Identity

*Homotopy type* theory. What is equal? What it identity? Composition and Identity is what defines category theory.

We are now ready for first definition!

A category is a *bunch* of objects. What can be bigger than a set? Sets are defined by membership. Elements of sets can also be sets. They have a set hammer and everything is a set nail.. What is a good example of a set that is a member of itself? A bunch of dogs is not a dog. *Barbers paradox* Who shaves the barber?

A Category consists of objects, and arrows. These arrows are also called morphisms. So a morphism is an arrow that goes from a to b `a ---(f)---> b`.
What is an object? It has no properties. What is an arrow, morphism? It is just a name for an arrow. We percieve the universe through these notions described by hunter gatherers, with spatial relationships. Categorists put things in space, higher or lower level abstractions.. Hunter gatherers understand movement too. Realize what language you are using and how it constrains you.

So what kind of things can happen? You can have zero or more arrows for each pair of objects.Some objects are not connected, some are, some are connected with infinite number of arrows. So if you have an idea that category is a graph except you have to be open minded what is a graph.

You can have arrow going from `a` to `b`, from `b` to `a` and arrows going from `a` to `a` and from `b` to `b`.. It's OK, just give them different names.

Composition is a very simple property that if you have an arrow going from `a` to `b` and an arrow from `b` to `c` you have an arrow going from `a` to `c`.

```text
A --- f ---> B --- g ---> C
           g . f
    ------------------->
```

So this is called composition. The category is defined by saying what the objects are and arrow and then defining the composition, like multiplication table for arrows. Different composition tables will give you different categories.

Identity for every object, there is this arrow that we call identity, one per object. Why am I calling it identity? Because of composition. If I compose `f` with identity `b` I get `f` again. So:

```text
(id a ---> a) a --- f ---> b (id b ---> b)
idb after f = f
```

So if you think of it as a graph it has to have some properties, it has to have an identity arrow on each node. So that is left and right identity. The third law is the law of associativity.

```text
f ----> g ----> h
```

`g` after `f` and then I compose this with `h`. So I have:

```haskell
h . (g . f) == (h . g) . f
```

That is extremely important to make is manageable for us humans. Is it possible to have no associativity? There are mathematicians that work on making associativity weak..

If objects form a set it's called a *small* category, if not it's called a *large* category.

In programming, objects are types and arrows are functions. A function is an arrow or a morphism between two types. In Haskell is a little more complicated because of laziness, so the trick is that every type contains the bottom value, because categories don't really take into account *time*. Time is hard to describe in mathematics but in programming is important. When does the calculation terminates? If the function never ends what is its type? So in Haskell it return an Int type, the bottom type, which means it never terminates.

**You might ask what are types?**

Sets of values? There is a simplistic model, they are just sets of values so we can model programming as in a category of sets, sets of values and functions are just functions between sets. And that's a good model too. So a mathematical function is between sets.

> NOTE: Interesting when drawing arrows between sets the arrows as functions are not morphism. They seem to just map objects but do not morph them. It is like a different view. Seeing morphisms blindly like seeing on which objects they work but without seeing the work, the type of work itself.

*When I put my categories glasses I see no structures like in sets.*

We are studying the sets and we find out there are many functions going from set to itself, we find the identity function and this is the identity morphism in my big multiplication table.. I'm abstracting, forgeting what's inside the objects, what the functions do and I end up with a category sets. I have these arrow I forget where they came from.. but I have the multiplication table which fulfills my laws. I can forget where it came from. I don't care about the structure of my functions or objects, I forget what these are.. What can I say about these objects if I just *look* at the morphism? Oh this set is *empty* How do you know it? Well, it turnes out the empty set has this property that can be expressed just in terms of morphisms.. it's not easy but it's possible.. You can identify a lot of properties about sets just by looking at the multiplication table without looking at the sets.. If you just look at the sets you are like doing assembly language.. instead you use a higher language of categories looking at the relations.. You look at the *interface* not the objects. Think *data hidding* and *abstraction*. This is the end of road for abstraction. The most abstract language you can think of. We can stop now. (Clap)

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)
```

## 2.1 Functions, epimorphisms

recap: Category of types and category of functions, identity and composability

Operational vs denotational semantics, sometimes I use semantics of sets and sometimes semantics of types.
A mathematical functions are total functions while in imperative programming we use partial functions which explode for some conditions.

*How can you tall if a functions is pure?*
A function is pure if you can memoize it, if you can turn it into a lookup table. Like functions on characters are easy to tabulate, while functions of strings or integers are not easy to tabulate, though that is a problem of resources.

What is the lowest, or the highest level of abstraction? The simplest building blocks with which we can build complex stuff? We want to get to the bottom so that we can recompose stuff. So now we have this category of types and functions

*How can we use functions as category of morphisms on sets?*

So functions are defined as special kind of relations. So we have two sets, and we look at elements. A relation is a subset of pairs of elements. So.. it's just pairing. This element is in a relation with this element.

*What is a set of pairs?*

A Cartesian Product. The set of all pairs forms a cartesian product. Now we take a subset of these pairs and any subset is a relation, by definition. There are no other requirements. So in this sense, a relation has no directionality while functions have these arrows. Functions have some kind of directionality. What kind of condition do we have to impose on a relation to be a function.

So many elements from set 1 can be in relation with one element with set 2 but one element cannot be mapped with bunch of things. It's still OK for many to be mapped to one value. And all elements have to be mapped into something in the other set. However not all elements of the second set have to be mapped to the first set.

```text
Domain ----- f ----->  Codomain
```

The mapped part(subset) is called the *image* of the function
This directionality is very important, this intuition of functions.

Ask yourself, is the function invertible? Usually it's not, there isn't always a function that goes other way arount, an *inverse* function.

`f :: a -> b`

The function is invertible if there is a function that goes from `b` to `a`

```haskell
g :: b -> a
g . f = id
f . g = id
```

A function that is invertible, symmetric, is called *isomorphism*.

```haskell
                     f
  (id a) <-  a < ========= > b -> (id b)
                     g
```

One reason for a function not to be isomorphic is to collapse. All even numbers map to `True`, all odd numbers map to `False`. That's one reason not to be invertible.

Other reason is that it's whole image does not fill the whole codomain. So if you had to invert this whole image what to do with elements beyond the image in the second set? So instead inverting you could also say the counterimage, fiber.. you could build a set of fibers so on a set of fibers this thing is invertible.. fibration is interesting in categories too!

So there are these two reasons for invertibility. So a function takes place in time. A function that is not invertible is something that increases *entropy*. You cannot unboil an egg. These two phenomena correspond to very interesting thought process, abstraction. Like I am throwing some information and I am left with one piece of information. Like abstracting numbers to even or odd.

Like embedding a shadow on a wall of a cave..

If a function does not collapse things than it's called injection. An injective function does not collapse things. No shrinking, no abstraction, it just injects..

```haskell
x1 -> f x2
y1 -> f y2
```

If the function covers the whole codomain, if the image covers the whole codomain it's called surjective. If its surjective and injective it is called isomorphism.

For all `ys` there exists an `x` that `y = f x`.

*So I have defined something in terms of elements* How can I talk about category theory now if I cannot look at the elements?

I have to express this stuff only in terms in morphisms. It is a very holistic approach. 

/If my microscopes don't work maybe my telescopes work?/

Note: In category theory we don't like latin we like greek.

When something is surjective is called *epic*, when something is injective is called *monic*. (when you consider set theory)

*Epimorphism* and *Monomorphism*

Let us say that we have guys that are in terra incognita, outside of the image in set `b` so I make a set `c` and map a function `g` to `c`.

So `g` after `f` will actually not probe this terra incognita even though `g` maps everything, inside composition it will actually only act on this inside a composition. Ok if I have two of these functions `g1` and `g2` mapping the same point, but if they are outside of `f` function halo then the composition is the same. So the converse of this is if `g1` after `f` is equal to `g2` after `f` then `g1 = g2`, so the function is *surjective*. Now I have expressed this purely in categorical terms. This is an *epimorphism*. If I have `g1 . f = g2 . f` then I can cancel `f` meaning `g1 = g2`.

## 2.2 Monomorphisms, simple types

Recap:

`f :: a -> b`

`f` is an epimorphism from `a` to `b` if for every other object `c` and for every pair of morphisms that goes from `b` to `c`, if the composition `g1` after `f` is the same as `g2` after `f` follows then `g1 = g2`, then this is an epimorphism. In another words if have a composition `g1.f` equals to `g2.f` we can cancel the `f` on the right.

[[https://en.wikipedia.org/wiki/Epimorphism][Epimorphism]]

Note: wikipedia

In category theory, an *epimorphism* (also called *epic morphism* or, an *epi*) is a morphism `f : X -> Y` that is right-cancellative in the sense that, for all objects `Z` and all morphisms `g1, g2: Y -> Z`,

`g1 . f = g2 . f => g1 = g2`

Epimorphisms are categorical analogues of surjective functions (and in the category of sets the concept corresponds exactly to the surjective functions), but it may not exactly coincide in all contexts

A function `f` from a set `X` to a set `Y` is surjective (also known as *onto*, or a *surjection*), if for every element `y` in the codomain `Y` of `f`, there is at least one element `x` in the domain `X` of `f` such that `f(x) = y`. It is not required that `x` be unique; the function `f` may map one or more elements of `X` to the same element of `Y`. A function would not be surjective if the image does not fill the whole codomain.

Identity function `idx` on `X` is surjective.

The function `f : Z -> {0,1}` defined by `f(n) = n mod 2`, (that is, even integers are mapped to 0 and odd integers to 1) is surjective.

So let's start with something that's not a monomorphism. A non-injective function will just map two different elements on one set to the sem element of set `B`, like `x1` and `x2` are mapped to the same `y`.

```text
 --g1--->   x1 \
z               ---f---> y
 --g2--->   x2 /
```

If you compose `g1` with `f` and `g2` with `f` you get the same result. They only differ in the way they map `z` but you get the same result. `f` after `g1` will be equal to `f` after `g2`. This is similar to epimorphism, we use precomposition rather than postcomposition.

For every object `C` and every pair of `g1` and `g2`, this time they go from `c -> a` if `f after g1 = f` after `g2` leads to `g1` equal `g2` always then we say it is a *monomorphism*. Notice `f` is not monomorphism by itself. I have to use the whole universe to define this product, a universal property.
I hope you get some better feel what functions do.

Let's talk about sets a little bit. Sets are models for types. Let's think of simplest possible types/sets. Empty set! Doesn't empty set correspond to a type in programming? You find it in Haskell. An empty set corresponds to a type (forget for a moment for functions that do not terminate) *Void*. There is no way to construct a *Void* type. Can we define functions that take Void as argument?

`f :: Void -> Int ?`

Mathematically speaking yes. I have a function of type Void, I challenge you :)

`id void :: Void -> Void`

This is a good function, you can never call it though, it exists in vacuum when you cannot provide an argument.

In Haskell it has a name: `absurd :: Void -> Int`

In logic Void corresponds to false, because you cannot construct falsity from something, you cannot prove something is false, it has no proof. Proof is a function, in this case since you cannot create void there is no proof of falsity, but on the other hand if you assume false, you can derive anything. So it's a polymorphic function: `absurd :: Void -> a`

Next is a singleton set, in Haskell is called a Unit `()` it has only one element.
`() :: ()` corresponds to `True`.

`Unit :: a -> ()`. What about a function that takes a Unit and returns an `Int ~() -> Int`? This function must be constant, has to return the same integer. There are many functions like this. `one :: () -> Int`, boolean...

What about a type that has two elements? It's just boolean. Boolean has `True` and `False`, whatever you call it. Boolean is not an atomic construction in sets or categories, it can actually be defined as a sum of two units.

We can talk about functions from bool to bool. A function that returns a boolean is called a predicate.

### 3.1 Examples of categories, orders, monoids

Last time we talked about sets, sets as sets of elements. That's not a very categorical view, I was trying to reformulate some of the properties of sets in terms of morphisms or functions. It makes sense to reformulate them between sets, then we can ask how does this generalize to an arbitrary category.. but we don't know many categories? Let's broaden our horizons!

Let's start with simplest possible category! Very few objects. Zero! But a category is defined with objects and arrows, so there are *no objects* then there are *no arrows*. Are the conditions then fulfilled. Well the answer is yes, if there aren't any then it's automatically satisfied, so is there an identity arrow? We can say anything about it then since there are no objects. It sort of sounds like a joke. What's the use of this category? It's useless, however the value is in context. Just like zero by itself is useless. What is the context. The context is a *category of all categories*. In that context, it's an *initial object*.

The next category has one object. There has to be an identity arrow `id`. That will be a *terminal object*

Next is two objects. Two objects with two identity arrows, two arrows from `a` to `b` and `b` to `a` and so on... In general we can always start with a *graph*. But not every graph is a category. It turns out if we start with a graph we can keep adding additional arrows to get a category. The first thing we need are identity arrows, for every node in the graph. Then we come up with composition. For every pair or composable arrows `f` and `g` we have a third arrow `g(f)`, `g` after `f`. We need to satisfy *associativity*. These compositions produce the same arrows, then some can be identified. This kind of construction is called *free construction* since we are not imposing any constraints other than constraints of category theory.

*Order* categories - in orders arrows are not functions, arrows represent relations. An interesting relation is less than equal `=<`.

```text
    =<
a ------> b
```

So this arrow doesn't have any meaning other than `a` is less than of equal to `b`. It's a relation. Or we can say `a` *comes before* `b` in some order. There are different types of orded. There is *preorder*, *partial* order, *total* order.

A *preorder* satisfies just the minimum of conditions, it has to be composable, so if `a` is less than `b` and `b` is less than `c` we want a to be less than `c`.

```text
   =<       =<
a ----> b -----> c
  ------------>
       =<
```

We recognize this as composition from category theory. Is it *associative*? It is! Why? It's because two objects are either in a relation or not. If there are no relation than there is no arrows, it's a binary choice. Now in *total arrow* you can say between any two objects there is an arrow. But in *preorder* that is not true.

2nd condition (identity)! So is it true that for every object this object is less than or equal to itself? That's called *reflexivity*.

Here we have only one arrow going from `a` to `b` and another arrow from `b` to `a`, but we cannot have multiple arrows. A category like this is called a *thin category*.

The set of arrows between any two objects has a name also, it is called a `hom-set :: C (a,b)~ or ~C(a,a)`

A *thin category* is one in which a every hom-set is either an empty set or a singleton set. That's the definition in terms of *hom-sets*.

We can now impose additional conditions and the next thing we get is *partial order*. We don't like preorder, we don't like loops. So partial order has no loops because if there is an arrow from `a` to `b` then you cannot have an arrow from `b` to `a`. If you look at a graph it corresponds to a *DAG*, *directed acyclic graph* and further if you say OK, a *total order* is an order in which there is an arrow between *any two* objects.

And now with this *preorder* category I can show you epi and mono. Something whats both epi and mono does not have to be invertible. In sets it corresponded to injective and surjective and if it was both it was reversible, called *bijection* but that is *not true* in every category. You can have an epi and mono that is not inversible.

```text

      mono                          epi
z === h1,h2 ===> a --- f ---> b === g1,g2 ===> c


```

Every arrow in preorder is a monomorphism and every arrow in preorder is a epimorphism, but it's not invertible, especially in partial order than it's definetly not invertible because there are no arrow going back. So that't a counter example.

You can think of the most general category as being like a preorder that is *thick*. It gives you a different intuition. Here when you have order you think of it as a relation and this means it is true or false. So it's a *black and white* world. Now in a *thick* category you might say, if I have a number of arrows, and each of these arrows sort of represent a *proof* of this relation. Here is one proof called `g`, here is another called `f`.. so you might think of a category like a proof relevant *preorder*. A thin category defines a relation and a thick category defines a proof relevant definition. That's a different way of looking. It's not only enough to show that something is related to something, *Homotopy type theory* studies relations in that sense.

In one object category we can have many arrows. We can have many more loops.

```text 
-- Monoid

  /  <===>
m -- <===> id
  \  <===>

```

So any category with a single object and many arrows is called a Monoid, sort of a pre-group. Monoid is usually defined as a set of elements with some operation defined on them, let us say multiplication. So it's a binary operator. And this binary operator, we can impose certain conditions. We want one of these elements to be the `unit` (*identity*), sort of like multiplication by `1`, you always get the same result. And the other condition that can be imposed is *associativity*.

~(a * b) * c = a * (b * c)~
~e * a = a * e~

String concatenation, that's an interesting monoid, does it have a unit? Yes, an empty string, you append or preappend an empty string don't change anything. It's associative. It is a good example because it's not symmetric. Multiplication and addition is symmetric, you can change the order, with strings you can't, you append two strings, the result will be different if you append them in the opposite order, so this is a very nice example of a monoid. And lists, appending lists forms a monoid. In Haskell, strings are lists of characters and they form a monoid. So thi is *one view* of a monoid from set theory.

Let's call this Monoid M. There is only one hom-set from `M(m,m)` since there is only one object. This hom-set is a set, right? This category defines a set, and guess what, there is a third element that corresponds to the composition to these two arrows. Well, let's say this is our multiplication. So the third element is the product of two elements. If you pick any two arrows in hom-set, the end of one is the beginning of the next, so there is a third one, `f`, `g`, `g` after `f`, so `g` after `f` is also an element. And then `id` is here also. All these arrows are members of this set. Example, arrows would correspond to adding a number in some category. So the binary operator in a monoid has to be defined for all elements of the set, has to be a total function of two arguments.

So a category of types corresponds to a strongly typed system. You cannot compose any two functions. The result of one function has to have a type that is the same as the argument of the next function. That's **strong typing**. Not any two functions are composable. The types have to match. A monoid is a very special category in which every two functions are composable, that corresponds to your languages, that have weak typing, any two functions are composable.

### 3.2 Kleisl category

Let's define a relation on sets, this is a relation of *inclusion*, what it means to be a subset of another set. It is a relation, the question is what kind of relation is this? Is this an order, preorder? What should we check? Identity in terms of order is *reflexivity*.

If a is a subset of b and b is a subset of c we have composition. Is it associative? Yes.
So it is definetly a preorder, is it a partial order? We have no loops. If `a =< b` and `b =< a` then `a = b` so it is a partial order, is it a total order? No. Is it possible to have like a diamond relation? They form a *dag*.

```text
      a
    /   \
   b /=  c
    \   /
      d
```

I want to introduce a category close to us programmers, not based on types and function, we get to it by solving a real programming problem. The problem is this:

*We have a library of functions. One day the manager says there is a new requirements, that every function has to have a audit trail, every function has to create a little log that says the name or something, has to be appended to a log. Now go and rewrite our library so that every function leaves a trail.*

The simplest imperative solution would be: have a global log. A simple solution introducing many dependencies. But logs don't compose, deadlocks...

```haskell
pair <bool, string>
negate (bool x, string log) {
  return makePair (!x, log + "Not!");
}
```

The subtle problem is this use of plus, why does a function called *negate* knows about appending strings? This one function is more local but still it has this element knowing stuff it does not belong. So this is a good solution but not quite.

```haskell
par <bool,string>
negate (bool x) {
  return make_pair (!x, "not!");
}
```

Who does appending of this logs? Somebody has to concatenate these logs. So the answer is.. what do we do with these functions? We compose them. What if we modify how we compose functions? Let us define a new way of composing functions.

*Appending strings* is in essence composing of functions.

[[https://blog.softwaremill.com/kleisli-category-from-theory-to-cats-fbd140bf396e][Kleisli-category-rom-theory-to-cats]]

> Note: I didn't get this at all :( but will keep watching the lectures :). I also do not understand the examples in scala above. What I do understand that the composition between `a` and `b` resultet in an embellished function where `a` defines a `b` and a string, so it is not just `a` to `b` but `a` to `b` which results in pairing the result with another string. So this makes a monad, a way of composing special functions.

### 4.1 Terminal and Initial Objects

Recap of Kleisl categories, important to understand Monads. It seems challenging because you have to hold two categories in your head. So you start with one category in which you have objects and arrows. Now based on this category you are trying to build another category, the Kleisli category, and you are building it this way - you're saying *the objects in this category are exactly the same as in the starting category*, however the arrows in this category are not the same arrows as here so if I have an arrow from `a` to `b` is not the same arrow as the one in category `C` from `a` to `b`.

*Actually I have something that for every object here in C gives me some other object.*

Now, we talked about a particular case in which for every type, if I had a type `a`, I assign to it a new type that's a pair of `a` and `String`. A *pair* of `a` and `String` is a different type than `a`, but it's a mapping of types, so for every type `a` I have this type. Let me call this `f a`, but maybe not, it's not a function. So let me call it `m`.
Now, *m* is a mapping that mapps objects to objects or types to types. Later we will learn that this kind of mapping is called a functor.

For type `b` I will have a type `m b` (the pair of `b` and `String`) so if there is an arrow from `a -> m b` this will be my arrow from `a -> b` in my Kleisli category, so this is equal to this.  

```text 
*C*                    *Kleisli*
a ----> ma              a <====> id
|\     (a, String)      |
| \ ------------------> | 
|  \                    |
b   m b                 b
|\  (b,String)          |
| \                     |
|  \                    |
c   m c                 c
     (c,String)
```

So it's like im implementing a new category(Kleisli) in terms of this(C) category, I'm implementing the arrow in this category as an arrow in this category, this is an arrow (Kleisli), this is how it's implemented (C).

*How do I know it's a category?*

So what's an arrow from `b` to `c` (In Kleisli)? It's not an arrow from `b` to `c` (In C). It's implemented as an arrow as `b` to `(c, String)`, or in general some `mc`, right? So in this (C) categry they do not compose because the end of this one, (mb) is not the same as the beginning of this one(b).

How do I compose these (Kleisli) guys? In principle I don't know.

Now I showed you that in this case when is (b, String) and (c, String), let me call this function first `a -> mb` and I'll get this pair `b, String` and I will split this pair into *b and String* and then I will pass this *b* (`b,String`) here (points to *b* `a -> b`) and I will get this `c, String`, I will get `c` and `String`, right? And then I can combine these things, I can concatenate these two strings, and return a pair *(c, s1 ++ s2)* so I have now a way of composing these arrows.

Now in general for any kind of mapping it's not true, I was just lucky. There was a composition, I could define a clever way of composing these things! If I find the way of cleverly composing the implementation here (C) then I can say this is how I compose these arrows in this Kleisli category, and for this to be a category I have to have an identity. How is identity implemented?

It has to go from *a* to this *m a* or in another words *(a, String)* and it has to be a Unit with respect to my new special kind of composition. I have to pick a string thats an empty string so that the concatenation with an empty string will give back the original string.  

Once I do that than I can say this is a category and if this is a Kleisli category, then this mapping from `a` to a `string` or in general from `a -> ma` is called a Monad! So this is one of many definitions of a monad. This is a very categorical construction.

And now for something completely different. So we talked about sets and there is this category set and there is also set theory, and there are these two views, that very useful. One view is sets are things that have elements and we can define things like function, mapping elements to elements, so a lot of these things can be defined in terms of elements. And then we have this category set and in this category we suddenly got amnesia and we are forbidden to talk about elements, only about arrows. We started from arrows, and we know functions between sets so every time we have a function between two sets there will be an arrow in category of sets. And we know how to compose functions.

What is an empty set? How do I define an empty set if I don't know anything about elements, a singleton? A cartesian product (set of pairs)? So all this stuff have to be completely rediscovered. Just in terms of arrows and compositions. There is this method of defining things. It's called *Universal construction*
We use this in category theory to pick a particular kind of pattern. Since we cannot go inside of the object we define the properties of the object in terms of the relation to other objects. So we have to define everything in terms of arrows coming and going to this object. We have to think about the whole universe, and we talked about it with epi and mono.

So it's like googling. Think of a simple pattern, OK google in this category, show me all hits, so everything that matches this pattern, and usually you have lots of hits. The next thing you do is you have to *rank* these hits. If you have two hits, see which one is better. The best match defines the object that you are looking for.

We will try to define a singleton set. How does this set relate to other set? Think arrows!
There is one property of singleton set that's interesting. It has an arrow coming from every other set. There is an arrow from any other set to singleton set. In programming we call it *Unit type*, an empty tuple *()* so from any type or any set there is a function to Unit and this function is called Unit, it's a polymorphic function, it just ignores it's argument and creates a unit and returns it `a --- Unit ---> ()`, or `Void --- Unit void --> ()`.

Does it really single out singleton object? Is there any other type that have the same property? Well unfortunately yes because set is a category that is extremely rich in arrows!

Only, if you have an non empty set and empty set there is no function there! You can only say they all map to the same element, *and I'm fucked!*

So for instance, OK, Bool, the type bool of two element set, is there an arrow from every other set to it? You bet, right? In fact there are two arrows from any other set. One is called `True` and `False`. They just ignore the argument and return true or false.

The singleton type or unit type there is always a *unique* arrow from any other object, so this way we can define using pure arrows we can define what we mean by singleton set, without talking about elements.

Let's forget about sets! What would we call this object? We will call it *terminal object*, for all arows, all arrows will converge on this object. Not every category has a terminal object. We can try, we'll say a terminal object in a category is an object that has a unique arrow coming from any other object. Understand, these are two separate conditions. For all objects/type a there exist an `f` that goes from `a -> ()`. So this is one condition.

And for every two functions from `a -> ()` they have to be equal. That's how you define uniqueness.

```haskell
(for all) a (there exist) f :: a -> ()
(for all) f :: a -> (), g :a -> () => f = g
```

An empty set can be defined by outgoing arrows (singleton set by incoming arrows) `Void --- absurd ---> a`
So I have just reversed the definition I used for terminal object. By the same token I want this arrow to be unique. This object will be called *initial object*, the opposite from *terminal*, it has a *unique* outgoing arrow to every other object. This corresponds to empty set, or in programming to *Void*.

The property of the *terminal* object, no matter what path you take to the terminal object you can always shrink it to one arrow and it's always the same arrow, this is where uniqueness comes. With boolean for example  there would be two ways of shrinking, some path would become true paths some false, see there are more ways of shrinking these paths. When this object is terminal there is only one true path, leading you to the terminal object.

Ok, the next question we might ask, how many of these objects are there? How many empty sets are there? Just one, seems natural to think that, what about terminal object, how many singleton sets are there? Tougher question.. is it the same, the set that contains one apple is it the same as the set containing one orange? I don't know.. from perspective category theory, what does it mean for two objects to be equal? I don't know, *there is no equality of objects.* There is an equality of arrows, if they have the same ends and beginnings, right? So we can compare arrows for equality but cannot compare objects for equality, instead we can ask if they are *isomorphic*. Isomorphism is this fact that you have two arrows, one being inverse of the other. **Terminal object is unique up to an isomorphism.**

And even stronger condition is that there is an unique isomorphism between them. Like if you have two element sets, (true and false) and (black and white), true is black, false is white, these are two morphisms, both invertible.

Suppose we have two terminal objects `a` and `b`, so there will be an arrow from `b` to `a`, and it is a unique arrow because `a` is terminal object, but `b` is a terminal object so there is unique arrow coming from `a` to `b`. What's the composition? It's a loopy thing.

```text
          <------g----
(id a)  a              b (id b)
          ------f-----> 

-- Unique isomorphism:
g . f = id a
f . g = id b
```

How does the pattern and ranking relate to this? So my pattern is an object, a simple pattern, now show me all example of this pattern in your category, what will you show me? You will show me all your objects, because I didn't specify anything about it, that's a very imprecise query, it gives you huge recall, but we have the ranking. So if I have two matches I will say a is better than b if there is a unique arrow from b to a. OK, maybe there is no unique arrows.. ok fine well then you don't compare these objects. I didn't say its a total order, its a partial order. What is like the best fit? One that is better than all else, so terminal object is better than any other object. The difference between initial and terminal object is just in the ranking.

### 4.2 Products

There was a question about terminal objects. There is nothing I said about outgoing arrows from the terminal object. I talked about incoming arrows to the terminal object, they have to be unique from every object. It doesn't mean there are no outgoing arrows and in fact there are usually outgoing arrows from the terminal object and these are the arrows that helps us define generalized elements in other objects, every arrow from the terminal object to another object is a definition of a generalized element in this other object. This is what happens in set, when you map a singleton set into some other set, thats equivalent of picking one element and say, this element of singleton set is mapped to this particular element of the other set, so its picking another one, there are many morphisms, each of them picks a different element.

*Now let's talk...*

We have now two examples of universal construction, the terminal object and initial object. I talked about reversing the arrows. It turns out this has a much deeper meaning. Every construction in category theory has its opposite construction that is done by reversing arrows. If you define the terminal object you get for free the definition of the initial object. You can allways create a new category which is identital to another category but with arrows reversed.

```text
C (a -- f --> b)    C op (b -- fop --> a)
f . g               (g . f) op

(g . f) op = f op . g op
```

*Cartesian product* (the set of pairs), for instance a plane is a cartesian product of two axis, and cartesian product corresponds to a point.

For every cartesian project there are these functions, called projections. In Haskell we call then `fst` and `snd`. Of `a x b` there are these two arrows called first and second. First maps to `a` second maps to `b`. That's a pattern. I'll call `a x b` a `C`, maybe I'll call these two arrows `p(fst)` and `q(snd`. There could be many such things, it could be anything. Now one of that is my cartesian product, but which one?

Universal construction to the rescue! I have to be able to rank them, that some cartesian product is better than another one. So let us say we have `c` and `c'`. We say `c` is better than `c'` if there is a morphism, let's call it `m`.

```text
                    C'
                  / | \
               p'/  |  \ q'
                /   m   \
               /  p | q  \
            a <---- C ----> b       

p . m = p'
q . m = q'

Later: 
a = Int
b = Bool
p = fst
q = snd
```

Is this enough to pick? No it's still not enough. So to summarize, `c` is better than `c'` if there is a unique morphism `m` from `c'` to `c` such that, this is true `(p.m=p', q.m=q')`. How do we read this?

If this were multiplication then you would say `p'` factorizes into `p` times `m` and `q'` factorizes into `q` times `m`. So they have a common factor *m*. So I can like extract a common factor. So this morphism is special, it factorizes these two projections. It takes the *worst* out of these two projections, condenses them. Why the worst? If you look at different candidates, like the [Goldilocks principle](https://en.wikipedia.org/wiki/Goldilocks_principle), some candidates are too small some too big, they don't fit. Morphism can loose information, it can squeeze, may not cover, so like all this non injectivity is concentrated in this *m*. This is a bad guy, it does all this non injective non surjective stuff, they are concentrated in there. Like `p` is this nice clean projection but if you add this uglines you get this `p'` projection.

So the real product of `a` and `b` is a pair `(a,b)`. That's the type.

```haskell
fst (a,_) = a
snd (_,b) = b

a = Int
b = Bool
(Int, Bool)

Int candidate

p :: Int -> Int
p = identity

q :: Int -> Bool
q = True
```

I have to show there is an mapping `m`.

```haskell
m :: Int -> (Int, Bool)
m x = (x, True)  -- non-injective badness
```

*Correction: I mean non-surjective, It misses pairs of the form (x, False)* Let's try a different candidate. We want a richer candidate. Let's have a triple. `(Int, Int, Bool)`
Now I can define a projection.

```haskell
p' :: (Int, Int, Bool) -> Int
p' (x,_,_) = x
q' :: (Int, Int, Bool) -> Bool
q' (_,_,b) = b
```

But this guy (p') is too big, like 3D cube and my product is just a square, so I'm shrinking stuff. What is the `m` in this case. `m` would have to be a mapping from the bad candidate which is `(Int,Int,Bool) -> (Int,Bool)`. `m (x,y,b) = (x,b)` so it's *non-injective*.

How do we define a product? So a categorical product of two objects `a` and `b` is a third object `c` with two projections `p` and `q`.

```text
C   p : c -> a
    q : c -> b
```

For any other `C'` that has some `p'` from `c'` to `a`, and `q'` from `c'` to `b`, for any other pretender there is a unique morphism `m` that goes from `C'` to `C` which factorizes the two projections. `p' = p . m` and `q'= q . m`. *You remember the picture, the commuting diagram. Two paths through a diagram give you a same result. C is called the product of a and b*

### 5.1 Coproducts, sum types

Today I show you the dual of the product, the same thing but in the opposite category, I will take the product and reverse the arrows and show you whats produced and the thing constructed is called coproduct. Co- is usually called when you reverse something, Monad, Comonad..

So a product is this object with two morphism *p* and *q* into /a/ and /b/. So it's a product of a and b and a product *C*, but there are lots of things that have two projections, so product is the best, the ultimate!

But what about this *C'* prime, it also has projections p' and q' why this is not a product? There is a unique mapping ~m~ that makes these two triangles commute. This *C* is the best candidate if for any other candidate we can do this direct mapping, we can reduce it to this one *C* and these two projections (p,q) which in sets were just (a,b) so now if we /reverse/ the arrows, we will try to draw this upside down, we want the diagram flow from top to bottom.

#+BEGIN_SRC 
                 a             b
                    \       /
                     \     /
                \   i \   / j   /
                 \    \\ //    /
                  i'    C     j'
                   \    |    /
                        |
                        |
                        C'
#+END_SRC

So a *coproduct* will match this pattern, It's an object with two arrows coming to it from a and b, and these two arrows are called injections. We just reverse the arrows. 
So instead of mapping the fake candidate into the real thing using a unique morphism we are mapping the real thing using a unique morphism into the fake candidate. 

#+BEGIN_SRC 
i' = m . i
j' = m . j
#+END_SRC

In programming cartesian product gives us a tuple, a pair of two things. ~(a,b)~ and this is well known thing. However a coproduct? What is it in set theory? So the fact that we have this injection means we are embedding the set a into C and be are embeddin a set b into set C, I'm thinking function, because we want to figure out what this gives us. This is like the best fit for this pattern so we want this to inject the whole set without collapsing, so what happens is that like the best fit would be a set which contains both a and b so it's like s /union/ of these two sets. Injected /faithfully/, means that the whole set a is mapped into this and the whole set b is mapped into this and there is no bloating, no unmapped things, just a and just b and nothing else and this defines absolute best possible fit and this is true of every universal construction, picking the ideal thing. Two objects, two injections, a coproduct. Here we can say that a product is something that every other candidate can be shrunk into this product that we can recognize, map this candidate into the product while here, in *coproduct* we can recognize a and b inside of C and this unique morphism tells us which parts of C' belong where, what we can find in C'. This does not completely define this thing. In set theory you have this union of two sets. What happens when they overlap? What's a union of set with itself? Just the same set, but you can tag these elements, it's actually duplicated, because it has a tag. I came from the left one, I came from the right one, so this is called a /discriminated union/. It turnse out that it is a *discriminated union*. There is a mapping from discriminated union to union. a left and a right. So we have twice as many elements, which can be mapped to a single a, a /non-injective/ mapping and it's a unique mapping of this discriminated union into a regular union. The other way around we couldn't. There is no way of mapping of mapping an a from union to a discriminated union because a function cannot split.

What is it then in terms of types?
In terms of types discriminated union is called a /tagged union/ or a /variant/. It means you have a /data type/ like which if you take a union of Integer and Boolean, this is something that either contains an Int or a Bool, not both of them, a pair is something that contains both, you need both to construct it, in order to construct a /discriminated union/ you either give me a integer and I give you this union, and I tag it, I'm an integer if you look inside me you find an integer, or if you give me a boolen we will have a tag that says it's a bool. 

The simplest example of this is an /enumeration/. It is an union of things, it can be either this or that.

A sum type is not built in and the canonical example is called Either

#+BEGIN_SRC haskell
data Either a b = Left a | Right b
#+END_SRC

You read it either left a or right b. It means you can construct an element of this type, either by calling this constructor and it contains an a or by calling this constructor and it contains b, so these two constructors correspond to this ~i~ and ~j~, one of them injects a and other one injects b whereas in a pair I had two, lets say /distructors/, they destroyed the pair by picking it a part (fst, snd) se here I have two /projections/ here I have two /injections/.

Because this is a dual picture, how do we extract stuff? 
Somebody gave me something ~x :: Either Int Bool~ I cannot just say give me an integer from this x, I can't do that because maybe it is a boolean, I do not know how is constructed. I have to take into account both possibilities, to write code that will work in either case, and that's called /pattern matching/, code that will match left pattern and right pattern. 

#+BEGIN_SRC haskell
f :: Either Int Bool -> Bool
f (Left i) = i > 0
f (Right j) = b
-- they will only match when f is called with left element
-- and will make this i equal to the integer sitting
-- inside
#+END_SRC 

Now we pretty much have the foundation of the type system. In every programming language product types are all over the place. 
#+BEGIN_SRC haskell
data Point = P Float Float
    -- record syntax
           = { x :: Float
             , y :: Float
             }
#+END_SRC

Most of programming is done with products.
Standard union in C++ is not tagged. Why is is called a product and a sum? Well sort of like a union, maybe.. so this is sort of like multiplication, like a /plus/

(take a break) and we come back to /algebraic data types/.

* 5.2 Algebraic Data Types

So we have products and we have sums, just like in algebra. Product, sort of like multiplication, what does it mean, it means we have a monoid, at least, right? 
So a monoid would be something that has multiplication, associative and that has a unit. But now we are talking about types. Is there something like an alebgra of types? Is the product in algebra of types actually behaving like multiplication? Let's check a few things in haskall.

The product of numbers, it's not true of every monoid, but a product of numbers is symmetric, let's see if a product of two types is symetric.

~(a, b)~ is it the same as ~(b, a)~? No it's not. If you have a function that takes a pair of Int and Bool it will not accept a pair of Bool and Int. So these two types are not the same, however they contain exactly the same information, they encode it slightly differently, which means that actually they are /isomorphic/. And this isomorphism is called /swap/. 

#+BEGIN_SRC haskell
swap :: (a, b) -> (b, a)
swap p = (snd p, fst p)
#+END_SRC

It is symmetric up to isomorphism.

The monoid product is associative. What does that mean?
~((a, b) c)~ is it the same as ~(a, (b, c))~. This won't typecheck. But again they contain the same information.

#+BEGIN_SRC haskell
assoc ((a, b) c) = (a (b, c))
#+END_SRC

swap is isomorphic because if you swap two times you get the same thing. 

Does it have a unit of multiplication? What would be the type if you pair it with any other type, you will just get back the same type? Well it has to be a type that has only one element. So the type that has one element is called a unit. ~(a, ()) /= a~

#+BEGIN_SRC haskell
munit (x, ()) = x
munit_inv x   = (x, ())

munit       = fst
munit_inv x = (x, ())
#+END_SRC

This follows from sum being associative up to isomorphism.
#+BEGIN_SRC haskell
Either a b ~ Either b a

data Triple a b c = Left a
                  | Right c
                  | Middle b
#+END_SRC

What's the unit of sum? /Void/.
#+BEGIN_SRC haskell
Either a Void ~ a
       a + 0 = a
#+END_SRC

So we have two monoids but that's not all!

We would like to combine these two monoids into a bigger thing. What would be that?

From algebra we know that we can multiply to 0, we have this ~a * 0 = 0~ so a pair of (a, Void) ~ Void
So I can never construct a pair of a and Void which is the same as Void. So a times zero is zero.

There is /distributive law/. 
~a * (b + c) = a * b + a * c~

#+BEGIN_SRC haskell
(a, Either b c) ~ Either (a, b) (a, c)
#+END_SRC

What is this structure called when you have multiplication and addition in the same thing. It is called /a ring/. Except a ring has an inverse of addition. And here we don't have inverses. We don't know how to /subtract/ something. What's the inverse of integer of a type, it's nothing.. 

A ring that has no inverse is called a /Rig/ or /Semiring/

What is the correspondence of 2 = 1 + 1? So 1 is a unit type, we can call left unit true and right ne false, so 2 is a bool.

What else? 1 + a is our friend ~Maybe~
~data Maybe a = Nothing | Just a~ (So Nothing is equivalent to (left) unit ~()~, and right Just a is ~a~.

But there is more one interesting trick :) Let's solve equations!

So the equation i want to solve is this
#+BEGIN_SRC haskell
l(a) = 1 + a * l(a)
l(a) - a * l(a) = 1
l(a)(1 - a) = 1
l(a) = 1 / 1 - a
#+END_SRC

#+BEGIN_SRC haskell
data List a = Nil | Cons a (List a)
#+END_SRC

Now, unfortunately I cannot do division and subtraction.
Does anybody recognize this? It is a sum of geometric sequence. For n = 0 to infinity a to the power of n.

#+BEGIN_SRC 
  ~
= E  a^n = 1 + a + (a * a) + (a * a * a) + ...
 n=0       ^   ^      ^           ^
           ^   ^      ^           ^
	   ^   ^      ^           ^
          []  [a]   [a,a]      [a,a,a]
#+END_SRC

What is not a trick or is a little trick that can be explained later is that this equation can be solved by substitution. This expantion ends in a /fixed point/. This can be formalized in a fixed point combinator. So this is how you get a list. 

#+BEGIN_SRC 
L(a) = 1 + a * (1 + a * L(a))
     = 1 + a + a * a (1 + a * L(a))
#+END_SRC
This is why these are called /algebraic data types/. We will get to the point when we do exponentials. Then our algebra will be really really interesting!

* 6.1 Functors

So today I want to talk about functors. So all previous lectures were just introduction to functors. Mathematicians will say /natural transformations/ are important and you need functors for it.

Why are they important?

There are things in category theory that are formalized that we thought were not. What is really universtal construction about? It about being able to define what is means to be a perfect embodiment of a idea, to be an ideal. It's like we say how do we define a product. Well we have all these possibilities, how do we pick one? Well we just say let's pick the best one. And we have these two types of universal construction, one for product one for coproduct and they sort of define slightly different ways what is more ideal than other thing. And the property of this perfect thing there is a morphism coming from any other candidate (product) which means anything else you throw at me can be /distilled/, any two objects with projections and I can distill it into this perfect thing called a product, finding a unique morphism.

With a coproduct it means here is an ideal coproduct and any other candidate has an image in it because there is a morphism from this perfect ideal thing down to any other candidate that's not perfect, that kind of embedds it, finds this element of perfection. So *functors* now.

Mathematically speaking it's a simple idea, Functor is just a mapping from one category to another. When we talked about products and coproduct I used a loose language, looking for a pattern. So we have this pattern which constists of single object (terminal, initial) or an object and two morphisms (product, coproduct) and now we are trying to find this pattern, match this pattern in our big category. What does it mean? 
 
So pattern recognition, category theory tells us how to formalize /pattern recognition/. Well if you want to recognize a pattern inside a category, you have to define what you mean by a pattern, a pattern must be some kind of structure and you sort of have to map this pattern into this category but you have to map it in such a way that you recognize this as a pattern, meaning you have to preserve the structure. What is structure? Well category is a definition of structure, its pure structure, dots and arrows. So if you want to say I want to recognize a certain structure it means you want to define your pattern as a category. 

So if you want to map one category to another category you first map the objects (small category example) and since objects form sets in a small category its just mapping of sets, and thats a function. 

But there is something funny about functions that i have not mentioned, that functions are sort of primitive, trivial. What we are really interested in are mappings that preserve structure and it so happens that function are *mappings between sets and sets have no structure* They have just a bunch of objects. It's really hard to implement something that totally disorganized on top of hardware thats organized so people implement trees. But in order to implement a tree you need to be able to compare elements.. what is important is to find mappings that *preserve structure*. We are not really used to thinking about what does it mean to preserve structure. 

/A discrete category corresponds to a single set/ 

Any category that is no discrete by definition has structure. So if we want to preserve structure our mapping has to have arrows.

So first we preserve mapping between objects. 

#+BEGIN_SRC 
*C*                                   *D*

a -------------- F ------------------> Fa
|\                                     |  \
| \                                    |   \ 
f   g.f                                | Ff \    
|    \                                 |     \ F(g.f)
|                                      |      \
b------------------------------------> Fb      \
 \                                      \
  \                                   Fg \Fb
   \                                      \
    c                                      Fc

C(a,b) ----------------------------> D(Fa, Fb)

F (g.f) = Fg . Ff

#+END_SRC

/Hom-set/ *C(a, b)*                 
/Hom-set/ *D(Fa, Fb)*

A functor is /huge/ potentially number of separate functions. One function per every hom-set. And I haven't even talked about preserving structure, which is defined by composition. I have to map composition I want to preserve structure.

A /functor/ is this kind of mapping of objects and morphisms that preserves composition and identity. This is sort of obvious way to define a functor. And this also formalizes this idea what does it mean to preserve structure. So functor is something that preserves structure. What else can be say about this functor. Whenever objects are connected in source category they will be connected in target category. Doesn't mean every morphism will have a corresponding morphism, it doesnt have to be surjective of injective, but we can never destroy connections. If you done calculus it is sort of like /continuous transformation/. Essentially thinking about this continuity. Otherwise you can shrink things, you can collapse things, and in particular you can define functors that don't break things.

If the mapping of /hom-sets/ is /injective/ than we call such mapping /faithful/. So a /faithful functor/ is injective on all hom-sets

So a functor is /full/ when /surjective/ on hom-sets.

So it can collapse objects and still be /injective/ on morphisms. Or it can map the whole category into a tiny category and still be surjective on hom-sets. 

The most beautiful is the /fully faithful functor/, the one that preserves, an isomorphism on hom-sets. Surjectivly, injectivly.

What's a functor whose source is just a one object category (will have to have an ~id~)? So if we map it inside another category, well it has to be mapped into an identity as well, functor must map identity to identity. So that's equivalent to just picking just an object in this category. Just like we had with functions, from terminal object to another object. 

The other way around is a bit interesting. So, a functor that just mapps every object in this category into one single category, so it collapses every single, the whole category into a single object, a black hole, and all morphism collapse into one identity morphism.
This is an important functor. This is called 
/constant functor/. So this one is called ~delta C~ from c to c. Very important. 

OK, now what does all this have to do with programming?
Most common functors in programming will just deal with single category because that is the category of types and functions. But I never said that C and D have to be different categories. In principle it can be the same category. Objects in this category can be mapped into the same objects in that category. That's called an /endo-functor/. (going inside)
In Haskell these endofunctors are just called functors. 

So what is a functor translating this (image above)?

A functor has to be a mapping of types, a total mapping, every object has to have an image, which means it's a /type constructor/ but that's just one part of a functor. The type constructor is a mapping on types, function that works on objects, but it also has to map morphisms which means it has to map functions, so let's just grab some examples and see how we can define, starting from a type constructor.

#+BEGIN_SRC haskell
data Maybe a
#+END_SRC

So a is a parameter, so for every type a we are defining a new type a. So we are mapping types to types.

#+BEGIN_SRC haskell
a ----- Maybe -----> Maybe a

data Maybe a = Nothing | Just a
#+END_SRC

Is this a functor? We need to define functions between hom-sets. If we have a and some b, b will go to some Maybe b, we have to define a function that goes from Maybe a to Maybe b, so if this is f this is the mapping of f using the functor. This mapping of functions is called fmap of f

#+BEGIN_SRC 
        Maybe 
a -----------------> Maybe a
|                  |
|                  |
f                  | fmap f
|                  |
|                  |
b -----------------> Maybe b
#+END_SRC

#+BEGIN_SRC haskell
fmap :: (a -> b) -> (Maybe a -> Maybe b)
fmap f  Nothing = Nothing
fmap f (Just x) = Just (f x)
#+END_SRC

fmap for Maybe must be of this signature. What can we put instead of ~fmap f Nothing = Nothing~? .. mental block.. We could say.. It's nothing unless the type a is Integer, Just zero? Why not?

There is somthing called /ad-hoc/ polymorphism but we don't want to use it here. We are kind of straying from mathematics when programming with Haskell. We are actually imposing stronger condition, parametric polymorphism, we are making it restrictive, leading to /theorems for free/. This is something that says because in Haskell can actually only implement functions of some limited kind, a certain type of polymorphism, that imposes the conditions of what we can do. 
* 6.2 Functor II

So we have defined a functor. Maybe we have maybe we haven't :)

How do we know this preserves composition and identity. Well we cannot express this in Haskell, in type system we cannot encode these conditions. Unlike in other languages there is a way of using haskell on a whiteboard to prove things about the language. We would like to prove this functors preserves identity.

~fmap id = id~

Now this id works on a different object than this id.

~fmap  id a = id Maybe a~

and we want it to preserve composition:

~fmap (g . f) = fmap g . fmap f~

What does it mean functions are equal? They have equal values on equal arguments. Whats so special about Haskell is that every definition in Haskell is an equality. And it means what is says. These two things are equal. It is an equation. Left side is same as the right side. In programming this is sometimes called /inlining/, but if you have pure functions you can do /inlining/ and you can do the other way around, /refactoring/, turning an expression into a function call. 

When ~fmap~ acts on ~id~ it produces a function from ~Maybe a~ to ~Maybe a~, right? So I have two cases to check, this ~Maybe~ could be a ~Nothing~ or it could be a ~Just~

~fmap id Nothing = Nothing = id Nothing~
/see what did here? I did refactoring, replacing Nothing with id Nothing, so this checks/
~fmap id (Just x) = Just (id x) = Just x~
~id (Just x) = Just x~

Let's talk how to define a functor in general in Haskell. 

/Lifting/
#+BEGIN_SRC 
Maybe a--------> Maybe b
  ^     fmap f  ^
  |             |
  |             |
  |             |
  a ------------> b
#+END_SRC

*fmap* is a higher order polymorphic function.

It's not like you write one formula for fmap for all functors. So now you are seeing a different kind of polymorphism, in which depending on what your parameter is, in this case the functor, you get a different implementation of a function, fmap in this case. So this is an example of /ad hoc/ polymorphism. It's just we use a slightly different tool for /ad hoc/ polymorphism which is called a /typeclass/. A *Typeclass* is, you define a whole family, or a class of types that share some common interface. So in Haskell this is called a class.

#+BEGIN_SRC haskell
class Eq a where
  (==) :: a -> a -> Bool
#+END_SRC

So every type that supports this operator, that takes two a's and produces a boolean, but ~(==)~ is one name that will serve us for many different types, and its implementation will be different for every type, you implement equality different for integers, different for strings. So that's /ad-hoc/ polymorphism.

Functors are actually /type constructors/. So Maybe is a functor, because it takes a type and produces a type.

So if we want to define a functor we have to define it as a class.
#+BEGIN_SRC haskell
class Functor f where 
  fmap :: (a -> b) -> (f a -> fb)
#+END_SRC
/So f here is actually a type constructor! a is a type, b is a type, then f must be something that acts on a type to produce another type./

The most intuitive example of a functor is list.

#+BEGIN_SRC haskell
data List a = Nil | Cons a (List a)
instance Functor List where
  fmap _ Nil = Nil
 -- h type of a, t type of list of a
  fmap f (Cons h t) = Cons (f h) (fmap f t) 
#+END_SRC

fmap = map /map is just a particular implementation of fmap for lists/ But lists came earlier before functors, so they already had this map defined, but it's really fmap.

#+BEGIN_SRC haskell
type Reader r a = r -> a

#+END_SRC

The arrow itself
~(->)~ arrow takes two types ~r~ and ~a~ and produces a type of function from ~r~ to ~a~. 

Now, so far we've been talking about these type constructors that just take one type as an argument, and here we something that takes two types. But we can always just fix one type and say, we only care about the second type. We fix the arrow and we say let's just /vary/ ~a~.
#+BEGIN_SRC 
    Reader r
a -----------> (r -> a)
#+END_SRC

First one is fixed to bool for example and second one varies. This is called partial application, currying. 

#+BEGIN_SRC haskell
                                    g     f
           f            g        (r --> a --> b)
           ^            ^
           ^            ^  
fmap :: (a -> b) -> (r -> a) -> (r -> b)
-- functor acting on a functor acting on b
fmap f g = f . g = (.) f g  -- I cross f g on both sides
fmap = f . g = (.)
fmap = (.)
#+END_SRC
What is the general intuition behind this? I still haven't showed you identity or const functor. There is one intuition that works for endofunctors, some say it's bad some say it's good. The intuition is that a functor when it's acting on some type encapsulates, hides, the values of this type, so an element of the type Functor of a has elements of a in it, and something that has something inside is usually called a /container/. List is a container, it contains a's, a list of integers, a /tree/ which is a functor too is a container of objects, a vector is a container of elements and it's a Functor. But then there are these Functors that are problematic, like Maybe, it may contain an a or maybe not. A container can be empty? It kinda works and this idea that something is a container, what does it mean to apply a function to the contents of the container, just open this container, look at this stuff and apply the function. So this is what we did with Maybe, if it contains Nothing do nothing well if it contains an a just do this. But then we have this Reader guy, how is this a container? Look at a function that a Boolean and returns some other type? How many possible values does this function have. Two, true and false, so can I say it is a container of two values? I can memoize this function, replace it with a table lookup, which contains these two values. What about a function of integer, it is just an infinite sequence of ints, maybe I cannot memoize the whole thing, maybe I can partially and so on. So this distinction between a function and a data type, if you think about it is, /weak/. A list is a container, ok in Haskell I have a list from one to infinity ~[1..]~, obviously I cannot store it in memory, how is this implemented? As a function. All data types in Haskell, are /thunks/, they are function that can be evaluated to return a value, data are really functions, functions are really data. And we will talk about what function types are in category theory and you will see that it is actually an exponential, which is a data type. The only thing about Functor that's important is that you can apply a function to what it contains, there is no way at least the Functor does not provide you a way to retrieve this value, that's not a part of definition of a Functor. 

I want to leave you with this idea, that functors, /endofunctors/ are containers.

* 7.1 Functoriality, bifunctors

Remember, a functor is like lots of functions put together, there is one major function that mapps objects and that's an actual function only if the category is /small/, objects form a set and functor is just a function on objects, but it also has to /preserve structure/ and that is the most important part, we learned what it means to preserve structure, so functor maps not the only objects but also mapps connections between objects which are morphisms. So for every connection between objects we have this set of arrows between them, which we call a /hom-set/, and as these two ends of a hom-set are mapped from one category to another we define also a mapping of these morphisms between hom-sets, and since hom-sets are sets in a /locally small category/, that's also a function so for every hom-set there is a function that maps it to the corresponding hom-set in the second category. That means preserving connection between objects. 

Since functors are built from functions, we know that functions compose. 

Category in which functors are morphisms and categories are objects is called /Cat/. 

Let's combine two endofunctors we know about, ~Maybe~ and ~List~. There is this function called ~tail~ and tail takes a list of some a's and returns a list of a's.
~tail :: [a] -> [a]~
It's defined so that it just throws away the head of the list, so it's well defined only for lists that are not empty. What if the list is empty? The program dies. The only reason people use it is because it's more optimal, because otherwise you always have to check is it empty? Otherwise it is the /achilles heal/ of ~Prelude~.

But if we want to be sure, then let's define something called safe tail, that takes into account the possibility that list can be empty.
#+BEGIN_SRC haskell
safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

mis :: Maybe [Int]
sq :: Int -> Int

fmap (fmap sq) mis
(fmap . fmap) sq mis
#+END_SRC

Most type constructors that you woul normally used are sort of automatically functorial, what it means is that you have a type constructor and defining fmap for it kinda falls out automatically, it's like most data structures are regular in this sense, what it means, that algebraic data structures are automatically functorial. How do you define them? You form them using things like products or sums and you apply it to a unit or a type variable. If you create an algebraic data type using these operations then you automatically have a functor, so we have to start by asking ourselves is a product a functor? 

So a product of two types ~(a,b)~, this can be rewriten as a type constructor acting on a,b ~(,) a b~ so we could ask is this a functor in b? If we fix a. It kinda is. It's a type constructor. We can construct a type by pairing it with some fixed type. If a function goes from ~a -> b~ we can lift it ~(e,a)~ and ~(e,b)~ with ~fmap f~ where this fmap takes ~fmap f (e,x) = (e, fx)~.

We can define something that is a product of two categories then a functor from a product of two categories would be equivalent to a functor of two arguments, one from one one from another category. We know in category theory products are these beasts and now I'm saying I want to do something bigger, a product of two categories! It turns out that a product of two categories is easier to define than a product from two objects in a category. 

OK, let me take two categories C and D and we take objects from C and D and we form a pair ~(c,d)~ that we call ~CxD~ (/c cross d/) in which objects are pairs of objects so really the objects are cartesian product of the sets of objects. What else do I need? Morphisms.
And again, this is easy to do this pairing, I can do cartesian product of this hom-set and this hom-set. So I have a new category, called a /product category/.
#+BEGIN_SRC 
(f', g') . (f, g) = (f'. f, g'. g)
(id a, id b) = id (a,b)
#+END_SRC

Now when I have a product category is just a category, now I can define a functor that goes from this category. ~C x D -> E~ It means for every pair of objects I pick and object in E. And on morphisms, a morphism in C cross D is a pair of morphisms paired with a morphism in E. 
This functor is called a /bifunctor/. A bifunctor is a functor from a product category. In haskell we would have to lift a morphism from a product category. This would be a product with a same category with itself. ~C x C -> C~
but notice that we are actually talking about a functor that's not set from set, or ~hask~ to ~hask~, it's a functor from some other category to hask, a product of two hasks, so we are already getting outside of hask and getting into /hask "squared"/

What does it mean in terms of function we know? We have a mapping from two types into a type. That looks sort of like this ~(,) a b~. What about morphism? It is a pair of functions, we are lifting two functions /at the same time/. So if we want to define a bifunctor in haskell and just like functor it will be defined as a class.
It will have to have this way of lifting two functions at the same time. This higher order function corresponding to fmap will be called ~bimap~
#+BEGIN_SRC haskell
class Bifunctor f where
  bimap :: (a -> a') -> (b -> b') -> (f a b -> f a' b')
#+END_SRC

OK, product is /bifunctorial/. What about /sum/? mmm... Do we have to go through this construction and come up with a sum of two categories? What happens is that, we can use the same bifunctor idea for a sum so ~Either a b~ - thats the sum type, the canonical sum type of two types. Either a b is actually a bifunctor! So we can define the action of two actions of either a b. So Either a b takes a pair of types from a product category! It is a function of two arguments, takes two types and produces a third type of ~Either a b~. What we want to have in general is that if we have a product in a category, then this product /is/ actually a bifunctor and its a bifunctor of this type ~C x C -> C~. Here we have two examples in /hask/ one is the product in hask one is in hask they both are bifunctors like this. So a coproduct is also a bifunctor. 

#+BEGIN_SRC 
             a x b               
           /   .   \               ------+
     p   /   / . \   \q                    == ------>
       /    /  .  \    \           ......+
     /     /   .   \     \
    + f.p /    .    \g.q  +        ......+ = f x g
    a    /     +     \    b
    |   /    a'x b'   \   |
    |  /   /      \    \  | 
   f| /  /          \   \ |g
    |  /p'     q'     \   |
    +/                  \ + 
    a'                    b'

(a, b)  -> a x b
(C x C) -> C
#+END_SRC

* 7.2 Monoidal Categories, Functoriality of ADTs

In a monoidal category we would like to define what does it mean to multiply two objects, ha? (waves hands) :)
So a product, categorical product is sort of way like multiplying object, we already have one part of a monoid, we have this binary operation, on object, right?

What was the unit for product in Haskell. It was the unit type :D which is a singleton set (in set theory). How do we define a singleton set? Terminal object. Is this terminal object maybe good candidate for a unit in our newly formed monoid structure? It would mean if you construct a product in which you have some object ~a~ and you have this terminal object ~()~, so ~a x ()~ with ~a~ and ~()~ projections. So I want to prove that a is actualy product of a and terminal object and if you multiply a by terminal object you get back a. 

What are the projections here? ~id a~ and this is ~unit~, it's a good candidate, is it the best candidate? Let's try some other, a ~b~, a /candidate/ is really a triple, its an object plus two projections. It has to have a projection that goes to a, lets call it ~p~, and and one that goes to unit, call it ~unit b~ so in order to prove this is the best guy I have to show there is a unique morphism from ~b -> a~

#+BEGIN_SRC 

                      b
                    / | \
                  /   |   \
              p /     |p    \ unit b 
              /       +       \
            /   ida /   \       \
          /      /         \unit a \
         +    /               \     +
           a +                 + () 
#+END_SRC

A product is defined up to unique isomorphism. So a categorical product, then you have this new structure, monoidal structure on objects, I can do the same thing with a coproduct, the unit would be the initial object, so that would also be a monoidal category with coproduct and initial object, in general maybe there are other things like this, but what we really need is a product which is a /bifunctor/, we need this binary operation on object, and we need this unit for the bifunctor and we get the monoidal category.

What is a good name that could be a coproduct or a bifunctor, a good name is a /tensor product/. (writen as circle with a cross inside). 

I started all this discussion because I said ADTs are functorial. So product and coproduct are functorial. What else we use to construct data types? We can construct a datatype that does not depend on a data type. We have this way of constructing a trivial functor from a constant object, called a /Const Functor/, that takes two data types. Constant functor maps every object in one category into a single object in the second category, like a black hole, called ~delta c~ so in Haskell:
#+BEGIN_SRC haskell
data Const c a = Const c

instance Functor (Const c) where
 -- fmap :: (a -> b) -> Const c a -> Const c b
  fmap f (Const c) = Const c

data Identity a = Identity a
  fmap f (Identity a) = Identity (f a)

data Maybe a = Nothing | Just a
           -- Either () (Identity a)
--                   ^
--                   ^
               Const () a                     
#+END_SRC

There is an extesion in Haskell, 
~{-# LANGUAGE DeriveFunctor #-}~, then you can just say ~data Maybe = .... deriving Functor~ and the compiler will derive you the correct ~fmap~. 

There is one more type constructor that takes two arguments? Right! Function, the arrow. ~(->) a b = a -> b~
So arrow is a type constructor, it takes two types a and b and produces a third type which is a type of functions from a to b. Now strictly speaking I have not yet talked about function types, next lecture.. 
#+BEGIN_SRC haskell
newtype Reader c a = Reader (c -> a)
#+END_SRC
Is this a bifunctor?
Well, let's just check if we fix the second, because here we are fixing c, this argument type, what if we instead fix the return type and vary the argument type, can we create fmap for it?
#+BEGIN_SRC haskell
data Op c a = Op (a -> c)   
  fmap :: (a -> b) -> Op c a -> Op c b
--           ^        a -> c     b -> c
--           ^
--       wrong arrow
#+END_SRC

a -> c, a -> b, makes b -> c not good, we need b -> c to make it work. 

This kind of functor that works on the hask on inverted arrows is called /contravariant/
#+BEGIN_SRC haskell
class Contravariant f where
  contramap :: (b -> a) -> (f a -> f b)
#+END_SRC

A contravariant functor, its not like a container, its sort of like a negative container, not only it does not contain it just actually requires a's for its action, needs "fuel" of type a. If you say instead of a's I will be providing you b's then you need to show how to convert b's to a's so that the functor accepts the a's. /"It contains the empty matter of type a, so you have to have a warp converter" :D/

The arrow is a covariant functor in the second argument in the return type and its a contravariant functor in the first argument. Now if you combine these two things, another interesting thing, arrow itself as a functor thingy ~C^op x C -> C~, you take a pair of morphism but the first one is flipped, so a thing of this kind of type is called a /profunctor/. Why is it called profunctor? I don't know..
#+BEGIN_SRC haskell
data class Profunctor p where
  dimap :: (a' -> a) -> (b -> b') -> p a b -> p a' b'
--             f           g         (a->b)    (a'->b')
--                                     h
--            result: g . h . f
{-

               a' ---f---> a ---h---> b
               |                   /  
        result |                /
               |            / g
               |        /   
               b'   /          
#+END_SRC

An arrow is indeed the simplest profunctor. 
* 8.1 Function objects, exponentials

Functions are separate from types. So far we've worked with this model in which types are objects in our category and morphisms are functions, types are objects soo.. functions are not on the same footing as types, right? If you are working in the category of set, which is approximation of what we do in programming, you can think of, you have object a and object b, functions between form a /hom-set/ so if types are sets, then functions are sets too. So this is sort of like sefl-referential thing about this particulary category of sets, so hom-set which is a set of morphisms between objects is also a set. But that's not generally true. In an arbitrary category we don't have this object, what we have is the /hom-set/ being an /external thing/. A hom-set being actually member of set. What we would like to have is an /internal hom-set/, an object in a category that corresponds to the sets of morphisms between two objects, a and b, somehow represents this set of morphisms. And it's possible to define it. So how would we go about this universal construction for a function object.

First we define a pattern, then we define a ranking for matches and then we find the best match.

So this /pattern/ for a function object must involve two types, argument type and return type, object a and b and the third part would be the third object, the candidate function object, let's call it ~z~. We have to have some kind of connection. 

What would define for us the action of a function on a argument that produces a result? We have to find a morphism between some of these objects that would represent this idea. But it is a relation between three objects. So we would like to put an arrow between these two objects and the third object, we can't do it in a category. How to do it in sets?

Let's pick an element of this set that would be an argument and form a pair, function and an argument, this pair can be mapped to result, which is an element of b. So pairing in sets corresponds to taking a cartesian product of these two guys, ok so we can generalize it to a category called product and say ok, so we have a product

#+BEGIN_SRC 
 

    z'       z' x a
     |--|  |-------|
     |  |  |       |\
     |  |  |       | \
     |--|  |       |  \
      |     -------    \
    h |         |       \
      |   h x id|        \ g'
    z +         +         \ 
     |--|  |-------|       \
     |  |  | z x a |        \
a->b |  |..|       | \       \
     |  |  |       |  \       \ 
     ----   ---.----   \g(eval)\
               .        \       \
           |--------|    \       \
           |        |     +--------|
           ----------     |        |
               a          ----------
                         b
#+END_SRC

#+BEGIN_QUOTE
So thats the pattern we are looking at. But notice, that in order to define this pattern, we have to have a product in our category. And thats a very important thing. In order to define a function object in a category, you have to first define a product. If a category doesn't have a product, then we cannot perform this construction, and you will see later that it actually makes more sense when you think about function object as algebraicly as exponential, because an exponential is like iterated product, right? So if you don't have a product, how can you have an exponential? So thats the idea behind this.  
#+END_QUOTE

In the end we call this /eval/, that morphism is called evaluating a function. So thats the first thing to do. The next thing is ranking. So suppose that we have another candidate, ~z'~

z with g is better than z' with g' only if there is a unique morphism h from z' to z such that this diagram commutes
~g' = g (eval) . (h x id)~

And finally the third part of universal construction is picking the winner, the function object. ~a~ changes name to ~a -> b~ and ~g~ to ~eval~ meaning that for any other candidate ~z~ that has this function ~g~ from ~z x a~ to ~b~, there is a unique morphism ~h~ that maps z to ~a -> b~ such that this triangle commutes.

/We can think of g as a function of two arguments, f(x,y,z)/

But now we are seeing something, that a function of two arguments, is equivalent to a single function that takes an argument and returns a /function type/, (hand points to z and then a -> b).

There is one to one correspondence between g and h, so I have equivalence of /two ways of thinking/, one way of thinking I have a function of two arguments as a function that takes a product, and the other one its a function of one argument but it produces a function. And that's called currying. 

~h :: z -> (a -> b)~ because in Haskell this function object is really represented by an arrow. And
~g :: (z, a) -> b~
#+BEGIN_SRC haskell
curry :: ((a, b) -> c) -> (a -> (b -> c))
curry f = \a -> (\b -> f(a, b))

uncurry :: (a -> (b -> c) -> ((a, b) -> c)
uncurry f = \(a,b) -> (f a) b
#+END_SRC

In category theory people often don't call this a function object, they call it an exponential. So a function from a to b would be called b to the power of a, b^a, argument goes to the top, result goes to the bottom. 

If we have a function that goes from ~Bool -> Int~, so its really a pair of integers, one for false, one for true. So all possible functions from Bool to Int well there are just all possible pairs of Ints, so this is really a cartesian product of ints. So you can write it as ~Int x Int~ or ~Int^2~ (/Int squared/) so also Int to the power of Bool like

1 - ()
2 - Bool
...

The number of functions from a to b is really b to the power of a, if you look at all possible combinations, so by counting the number of possible functions you get the counting argument, and also this shows you the connection between product and exponential. An iterated product gives you an exponential. What we do want for programming in which we have exponentials, or function types. We definently want products. There are special kinds of categories called *CCC* /cartesian closed categories/ that are useful in programming. Cartesian category is one that has products for every pair of objects, /closed/ means it has exponentials as well, for every pair of objects a and b it has an exponential a to the power of b, and it also has a terminal object. Terminal object is like a /zeroth/ power of an object. Its like the first power is the object itself, second power of the object is ~a x a~, then we can have these exponentials, but the zero power is terminal object.

We actually want a little bit more in programming, we want /coproducts/ and /initial object/ so something that has not only cartesian products but also coproducts is called *BCCC* /bicartesian closed category/ and in a BCCC we can do our beautiful algebra of types. So far we've seen the algebra using products, coproducts, initial and terminal objects, first we saw that products form a monoid, coproducts form another monoid but we can combine them and they give you this /semiring/ and now we are adding exponentials. With exponentials we can do more algebra!

For instance, whats a to the power of zero ~a ^ 0~? One. ~1~. But is it true for types? What is zero? Thats our initial object, it is ~void~

~a ^ 0 = 1~
~Void -> a ~ ()~
~absurd~

So its a function from void to a, the right hand side is /One/, one is a terminal object, that the unit type. Are these two types equivalent. A unit type is a type that has one element. So if this is a singleton as well then we are done. So first of all, is there a function from void to a and how many are there? Well we seen this function it is called absurd. It takes a void and produces an a, so there is a function likee this, absurd.

What about ~1 ^ a = 1~ This is a function that takes an argument of type a and produces a unit
~a -> () ~ ()~ so there is only one function like this, it maps all elements of a into this unit element, this is collapsing, a const turning everything into a single values.

~a ^ 1 = a~ First power of an object is the object itself, but it also has this meaning ~() -> a ~ a~ unit to a is isomorphic to type a and remember what this is, its a function that takes one element from a. There is one to one correspondence, I call it generalized element (mathematicians call it a global element)

* 8.2 Type alegbra, Curry-Howard-Lambek isomorphism.

~a^b + c = a^b x a^c~
~Either b c -> a~
~(b -> a, c -> a)~

~(a^b)^c = a^b x c~
~c -> (b -> a) ~ (b, c) -> a~ Currying!

~(a x b)^c = a^c x b^c~
~c -> (a, b) ~ (c -> a, c -> b)~  

Sort of like you have to learn just one thing, and then everything else kinda falls out. So probably like the best thing is to start, instead of going to highschool, just start with category theory and then everything else will just follow from this. 

The other thing that, exactly the same structures for types and categories appear in logic. And thats the basis of famous Curry Howard isomorphism, or sometimes called propositions as types. 

So this isomorphism between type theory and programming in general and logic on the other side starts with identifying what it means to, what is a proposition. In logic is a statement that can be true or false so these propositions correspond to types in programming. Just as a proposition can be true or false, type can be inhabited or not, so the truth of a proposition means that type that corresponds to it has elements, members, its inhabited and most of the types we deal with are inhabited, so they are kinda like true propositions but there are types that are not inhibited, and they correspond to false propositions, and we know one such type, thats void.

So if you want to prove a proposition you just have to prove that a type has an element. In logic there are these two basic values, true and false. So the corresponding things in type theory would be void type which corresponds to false, and unit type which corresponds to true. Unit type is always inhabited with one element.


|---------------------------------+----------+---------+-----------------+---------------------+--------|
| Curry Howard Lambek isomorphism |          |         |                 |                     |        |
|---------------------------------+----------+---------+-----------------+---------------------+--------|
| /Logic/                         | true     | false   | and             | a or b              | a => b |
|---------------------------------+----------+---------+-----------------+---------------------+--------|
| /Types/                         | ()       | Void    | (a,b)           | Either a b          | a -> b |
|---------------------------------+----------+---------+-----------------+---------------------+--------|
| /Category/                      | terminal | initial | a x b (product) | a sum b (coproduct) | b^a    |
|---------------------------------+----------+---------+-----------------+---------------------+--------|



(a => b), a) -> b
a => b *and* a -> b   /modus ponens/

* 9.1 Natural Transformations

Triad of things that are foundations of category theory:
1. Category 
2. Functors
3. Natural Transformations.

Category is about structure, what it means. Functors are these mappings between categories that preserve structure, intuition is that they take a category and embedd it inside another category, sometimes called modelling. What if we have two different functors? How are two images related? So we would like to be able to compare images given by functors. So natural transformations are defined as mappings between functors. And these mappings have to preserve structure. 

Let's start with two categories ~C~ and ~D~, lets concentrate on a single object in category C, object ~a~. One functor maps this object into ~F a~ in category D, second functor ~G~ maps the same object to some object ~G a~. 

A /natural transformation/ would be picking a morphism between these two objects (In D, one morphism from this /hom-set/, ~Fa -> Ga~. In this way I am creating a whole family of morphism, these are /components/ of natural transformation. So the related morphism in D would be called ~alpha a~, between ~Fa -> Ga~
Now we map object b the same way ~Fb -> Gb~ and we have ~alpha b~

Having a natural transformation between two functors means they are somehow related. *See naturality square*

#+BEGIN_QUOTE
personal notu: This seem like an abstract jump to a higher level. Feeling kinda lost now but am continuing to watch the lecture. Need time to process and contemplate. Same actions are used but all together somehow making a much more complex /image/. As Bartosz says natural transformation gives us a higher level language of commuting diagrams. Notice natural transformation is a higher level language in category theory while just describing commuting diagrams is like /assembly language/. And then instead of just talking about commuting diagrams you begin to notice relations between functors. In this sense products and coproduts are just a case of limits and colimits and later there will be adjunctions etc..
#+END_QUOTE

In programming, natural transformation would be a family of functions between endofunctors, parametirized by a type, so a natural transformation would be a polymorphic function. 

#+BEGIN_SRC haskell
alpha :: forall a . Fa -> Ga
#+END_SRC

The subtle difference being, in this haskell example, in this form, we are assuming parametric polymorphism meaning if we want to define this function we must use one single formula for all a's. We cannot say do this thing for integers and do this thing for booleans. One single formula for all. And this is /much stronger/ than a categorical definition.
#+BEGIN_SRC haskell
alpha . fmap f = fmap f . alpha
#+END_SRC

Let's pick a list functor and a maybe functor. We talked about safe tail, lets talk about safe head.
#+BEGIN_SRC haskell
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x
#+END_SRC
This is a function that works for every ~a~, a total function, parametric polymorphic so it is automatically /natural/. But lets prove it, lets do equational reasoning on it. Show it on both empty list and non-empty list.
#+BEGIN_SRC haskell
safeHead . fmap f []             safeHead [] 
   
   Nothing                .        Nothing

                                   Nothing

safeHead . fmap f (x:xs)
           f x : fmap f xs

Just (f x)
       \
        \    safeHead (x:xs)
         \       Just x
          - - -Just (f x)
#+END_SRC

If you look at it it is actually an /optimization/. Applying an fmap on a list is expensive, so being able to safeHead first and then fmap is cheaper, of course not in Haskell, because Haskell is lazy.

Basically we use a lot of natural transformations in programming so ~a -> [a]~, thats actually a naturally transformation because ~a~ is just an identity functor acting on ~a~. There are also these function that take polymorphic object and return a number, like ~length~ of a list. Takes arbitrary type of list and returns a length. Thats also a functor, the ~const~ functor which ignores its argument. Natural transformation from a list to a const functor. If you have a function from one ADT to another ADT its a natural transformation because algebraic data types are functors. Not all are because we have these /contravariant/ functors, so if we have a polymorphic function which turns weird stuff it would not be a natural transformation. At some point you might learn about generalized natural transformations which operates on mixed convariants.
#+BEGIN_SRC haskell
return :: a -> m a
#+END_SRC
is actually a function from identity functor to ~m~, a natural transformation and when we talk about monads this thing will be defined as a natural transformation, so a Kleisli category really has to take into account that these transformations are natural. 

* 9.2 Bicategories

OK, lets talk about category theory but just like in a really really wide area, I want to give you the view of category theory as far as I was able to look into it. Because we have these basic part of category theory, categories, functors and natural transformations. 

Natural transformations are mapping of functors, now every time we have mappings of things we ask ourselves do these mappings compose? That should be the first thing on our mind. What does it mean to compose natural transformations?

\alpha F -> G
\beta  G -> H

\beta . \alpha
Is this a natural transformation? We have to check, we have to have another object b

a -> b -> (-> F G H) -> (Fa -> Ga -> Ha) -> (Ff Gf Hf) ->
Fb -> Gb -> Hb

Notation used is [C, D] or D^C, that suggests something :)

Note: Feeling lost now, but following along. Its all abstracted while using the same words like before, product, category, functor, it seems simple yet still not reachable, like being there but still blind not able to remember the last movement, thus feeling lost in a known environment. 

...

* 10.1 Monads

A monad is really such a simple concept. Why do people have problems with monads. It's the wrong approach of explaining what a monad is. Suppose you explain what function is, it like you go to a tropical island, there are some friendly natives and they want to learn functional programming from you and they ask you what is a function? You try to explain and give them an example, like you can have a function that takes a list of fisherman and orders them by the number of fish they caught, or another function takes  person and gives you their age, or another function that takes grain and gives you alcohol, right? And they say wow this is some amazing stuff, it does all these things, and more? Function is some powerful.. then you realize maybe thats not the best approach of explaining what a function is. 

Maybe we try this, function is like an animal, it has a mouth and it eats input and produces output on the other side, thats a better analogy but you get in big trouble when you try to explain function composition (laugh :D)

Why do we use function? So that we can structure our program, so that we can decompose our program into smaller pieces and recompose it, and the power of function is really in the dot. Thats what the power is. Dot is the composition operator in Haskell, combines the output of one into input of the other. Functions are about composition, and so is the Monad.

People start by giving examples of monads, there is state monad, there is exception monad, you know, thats, these are completely different things, what do exceptions have to do with state, with input output? Well its just like with functions, functions can be used to implement so many things, but really functions are about composition and so is the monad. Monad is all about composing stuff. It replaces this dot with the /Kleisli/ arrow. ~>=>~

The so called fish operator. Ok dot ~.~ is used for this simple functions, where output matches the input, the most trivial way of composing stuff. The fish operator is used to compose these functions whose output type is /embellished/, it really the output type of a function type would ~b~, but now we are embellishing it with some stuff, with logging, by adding a string to it, the logging Kleisli arrow, but then in order to compose these things, we have like unpack the return type before we can send it to the next so actually inside the /dot/ not much is happening, just one function is called and the result is passed to another function. Inside /fish/ much more is happening because there is unpacking and there is the passing of the argument to the next function, and also, maybe some decision is taken, like in the case of exceptions we will see that. Once we have this additional step of combining functions we can make decisions, maybe we don't want to call the next function at all?
A lot of stuff can happen inside the fish.

And just like we have the identity function here, identity with respect to the dot, here we have this kleisli arrow that represents identity that return the embellished result of the same type and we call it ~return~ in haskell. And its called return because at some point you want to be able to program like imperative programmer. It's not like imperative programming is bad. It could be good as long as its control, and the monad lets you do in this kinda imperative style, sometimes is easier to understand your code even though it is immediately translated into this style of composing function, so this is just for our convenience.

Using the kleisli arrow is equivalent of using the dot. We don't see many programs even in Haskell that are written using dots, this is called point free style where you just compose function after function and never mention arguments to these function, they are hidden, they are not given any names, they just go straight from output from one function to the input of another function. Point free style is popular with some people but it is considered hard to read. So this definition of a monad with fish operator is not the main definition that is used in other languages. 

** Fish anatomy

#+BEGIN_SRC haskell
~(>=>)~ :: (a -> m b) -> (b -> m c) -> (a -> m c)

f >=> g = \a -> let mb = f a 
                in mb >>= g

|-->   (>>=) :: m b -> (b -> m c) -> m c
|
|
|                  class Monad m where
|                  (>>=) :: m a -> (a -> mb) -> m b
|                  return :: a -> m a
|                ------------------------
|                class Functor m => Monad m where
|                   join :: m(ma) -> ma
|                   return :: a -> ma
|
|   -- but mathematicians go deeper into the fish, how to implement
|    -- bind?
|
|-->   ma >>= f =   join ( fmap f ma)
                   /      (a -> ma) m a
                  /        m (mb)
                 /
                /
               /
            join :: m(ma) -> m a
#+END_SRC


Remember this is all polymorphic in a, b and c. A, b and c are completely arbitrary types, we have to be able to define the fish operator for /any/ type a,b,c. Once you say this is any type it means you cannot do anything type specific, which means you cannot do anything :D
except! you have a function here that takes an ~a~, thats like you only chance to do something to a, well apply ~f~ to ~a~. So the sytax is ~let mb = f a~ like defining a local variable in other languages, in Haskell it just means giving a name, binding a name to some value.  



Why are we using these monads? Monads are used to provide composition for kleisli arrows, but why kleisli arrows? The magic is not in what a monad is, its just composition, the magis is why are these kleisli arrows so useful, what kind of problems they solve and why? The idea was that functional programming, everything is a pure function, and as programmers we know pure functions cannot express everything, there are so many computation like basic things, input output is not pure, a function that getChar if it returns a different character, it returns a different character every time you call it, so its not a pure functions. Then there are functions that throw exceptions. It turns out and this is a miracle that everything that can be computed using non-pure function, they can all be converted as long as you replace pure functions with functions that return an embellished result. This result is encapsulated in some weird way, this is the interesting part. Where does the monad come in, when we say I have this gigantic function that starts with argument a and produces this embellished result, and do I have to write it all inline? No, I want to chop it into pieces, into hundred side effects and then compose it. This is where monad comes in play, when you want to compose these functions you use the monad. The monad lets you split you computations into smaller computations and glue them together. One example are functions that are not defined for everything, so called /partial functions/. Square root is defined for positive integers for example, what you do? The program can blow up or it can throw an exception, but a function is not pure if it throws an exception. So we can use ~Maybe~.
#+BEGIN_SRC haskell
a -> b
a -> Maybe b

join :: Maybe (Maybe a) -> Maybe a
join (Just (Just a)) = Just a
join _ = Nothing

-- this doesn't explain the cleverness of maybe so let me
-- define bind

Nothing >>= f = Nothing
Just a  >>= f = f a
return a = Just a

-- Notice we are shortcircuiting, like exceptions, this
-- function f serves like continuation. If the first part
-- of the computation fails, abandon the computation
-- If we want to pass information about the arrow
-- then instead of Maybe we use the Either type, either string
-- or value
#+END_SRC

Second example is /State/

#+BEGIN_SRC haskell
-- you have a computation from a to b that accesses some
-- external state S, reading of modifying it but it can be turned
-- into pure function as long as we pass the state
-- explicitly

a -> b

(a, s) -> (b, s)

-- using currying. This is a kleisli arrow!

a -> (s -> (b, s))

newtype State s a = State (s -> (a, s))
#+END_SRC

#+BEGIN_SRC haskell
-- categorical terms

m -> T
join -> \mu
return -> \eta

\eta :: Id -> T
\mu  :: 

m (m a) -> m a
T . T --.--> T

                  Id
           ---------------->
          +        |         +
         C    \eta |        C
                   |         
           ---------------->
                   T



           T           T 
        C ----->  C  ----> C 
        \         T       /
         \ ------------> /
          \      |      /
           \     |\mu  /
             \   |   /
               \   /
                 T
#+END_SRC

For better diagrams google /horizontal composition of two natural transformations/. These are now two dimensional diagrams!

* 10.2 Monoid in the category of endofunctors
** Monad Graham Hutton tutorial 

Note: At this point Graham Hutton's [[https://youtu.be/t1e8gqXLbsU][Monad tutorial]] seems intuitive so I will follow along and write the code leading up to the monad itself.

Monads are a concept invented in mathematics in 1960s, rediscovered in computer science in the 90s and what it gives you  is a new way of thinking about programming with effects. This is one of the most important new ideas in programming languages in last 25 years.

The example we are going to look at is the idea of writing a function that evaluates simple experessions. What we are going to start with is by defining a simple data type for the kind of expressions that we are going to be evaluating.

#+BEGIN_SRC haskell
-- data type Expr has two new constructors
-- Val builds expressions from Integers
-- Div builds expressions from two sub-expressions
-- 1                 Val 1
-- 6 / 2             Div (Val 6) (Val 2)
-- 6 / (3 / 1)       Div (Val 6) (Div (Val 3) (Val 1))

data Expr = Val Int | Div Expr Expr

-- first version of evaluator
eval :: Expr -> Int

eval (Val n)   = n
eval (Div x y) = eval x / eval y

safediv :: Int -> Int -> Maybe Int
safediv n m = if m == 0 then
                 Nothing
              else 
                 Just (n / m)
-- new evaluator

eval :: Expr -> Maybe Int
eval (Val n)   = Just n
eval (Div x y) = case eval x of
                    Nothing -> Nothing
                    Just n  -> case eval y of
                         Nothing -> Nothing
                         Just m  -> safediv n m

-- too much management of failure, too much noise
-- the idea here is we are going to observe a common pattern
-- there are two case analysis.
-- when you see the same thing multiple times you 
-- abstract them out and have them as a definition

            m
case __(maybe value)_____ of

    Nothing -> Nothing       f    
    Just x -> __(function to process result)_____ x

m >>= f = case m of
            Nothing -> Nothing
            Just x -> f x

---------------------------------

eval :: Expr -> Maybe Int
eval (Val n)   = return n
eval (Div x y) = eval x >>= (\n -> 
                 eval y >>= (\m ->
                 safediv n m))
----------------------------------

eval :: Expr -> Maybe Int
eval (Val n)   = return n
eval (Div x y) = do n <- eval x
                    m <- eval y
                    safediv n m

-- the maybe monad
return :: a -> Maybe a
>>=    :: Maybe a -> (a -> Maybe b) -> Maybe b
#+END_SRC

1. Same idea works for other effects as well
2. Supports pure programming with effects
3. Use of effects is explicit in types
4. Functions that work for any polymorphism

