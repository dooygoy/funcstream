## Blogs

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

* [The "What are Monads" Fallacy](https://two-wrongs.com/the-what-are-monads-fallacy.html)

> The Haskell community has a monad problem. No, that's not quite right. Let me rephrase that.Haskell beginners have a monad problem, and the Haskell community is partly to blame. 

* [Opaleye's sugar on top](https://ren.zone/articles/opaleye-sot)

> People often talk of how solutions fall naturally into place when we program in Haskell and embrace its type system. This article walks us through that process, serving as a gentle introduction to some practical uses of advanced features of the Haskell type system within the context of Opaleye and SQL. I invite you to continue reading even if you are not particularly interested in Opaleye nor SQL, as the approach explained here can be used in other contexts too.

* [Finite-state Machines, Part 1: Modeling with Haskell Dat Types](https://wickstrom.tech/finite-state-machines/2017/11/10/finite-state-machines-part-1-modeling-with-haskell.html)

> Stateful programs often become complex beasts as they grow. Program state incohesively spread across a bunch of variables, spuriously guarded by even more variables, is what I refer to as implicit state. When working with such code, we have to reconstruct a model mentally, identifying possible states and transitions between them, to modify the program with confidence. Even if a test suite can help, the process is tedious and error-prone, and I insist we should have our tools do the heavy lifting instead.

* [Basic Type Level Programming in Haskell](https://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html)

> Dependently typed programming is becoming all the rage these days. Advocates are talking about all the neat stuff you can do by putting more and more information into the type system. It’s true! Type level programming gives you interesting new tools for designing software. You can guarantee safety properties, and in some cases, even gain performance optimizations through the use of these types.

* [www.mathmeth.com resources](http://www.mathmeth.com/read.shtml)

> Here is a collection of materials which we find particularly instructive and useful. Some materials are available online for free, while others have to be purchased.

* [Builtins](https://github.com/input-output-hk/plutus/blob/master/language-plutus-core/docs/Builtins.md)

> I've recently written a document elaborating on the design of FFI for a functional language implemented in Haskell. Type safety of FFI is ensured via type-level programming (GADTs, type families, etc).
