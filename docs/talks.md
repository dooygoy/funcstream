# Talks

## [Adventures with Types - SPJ](https://youtu.be/6COvD8oynmI)


## Conal Elliott - Denotational Design: From Meanings to Programs 

- youtube link: [Conal Elliott - Denotational Design: From Meanings to Programs](https://youtu.be/bmKYiUOEo2A)

Main job of software designer, which is to build abstractions. What do we build? We build programs. But I don't think we build programs mainly, the program in the sense something you can run. For every program that I make, what's really more powerful in my output, what's more reusable, what's more value, is the libraries I create, so the components out of which I build programs. So every time I do programming I'm really looking for what abstractions are a great fit. If I can already find these abstractions supported of the shelf of some library I'll use them but most often I'm fairly dissatisfied with the building blocks, and so I wanna build my own abstractions, and then build a particular application on top.

This quote I like very much, from the man I respect very much, Edsger Dijkstra:

> The purpose of abstraction is not to be vague, but to create a new semantic level in which one can be absolutely precise.

So we can be precise and low level, and be precise without being abstract but then it's quite complicated and our cognitive abbilities will soon reach our limit before we realise our aspirations. There's a wonderful paper by Dijkstra called [The Humble Programmer](https://www.cs.utexas.edu/~EWD/transcriptions/EWD03xx/EWD340.html). Highly recommended talking about how we can use our abbilities, by walking into the project humbly realising that I'm gonna reach my cognitive limitations quite soon, then it informs the choices I want to make, I want to use abstractions that allow me to continue making progress even though I have a brain that has to fit in a skull. 

So there are three goals, I think of these as my three main goals when I'm doing a software project.

* Abstractions: precise, elegant, reusable.
* Implementations: correct, efficient, maintainable.
* Documentation: clear, simple, accurate.

I'm gonna talk about precision a lot, and it's not that I think that precision is an end to itself, it's not. It's really these other things, like elegance and reusability. The reason I harp on precision is because it is so easy to fool ourselves and precision is what keeps us away from doing that.

My implementation I'd like it to be fast, right?, but I don't want just fast, I want it to be correct, I care about what I implement. By writing no code I've written a very fast implementation but it's not gonna satisfy any interesting requirements. And of course, I want my program, my implementation to be maintainable, to sort of capture and hold my insights so that somebody coming back at me later can understand what I understood in putting them together.

And then the documentation should also be simple, clear and accurate. So one way to make things simple is to make them inaccurate, I can tell you a simple story that isn't true and at some point I'm gonna harm you by doing that. So I want to tell the truth and to do so simply.

* Conventional programming is precise only about how, not what.

So, I'm gonna say something, maybe a little jarring which is the commonly practiced state of the art of software is something that is, it's precise about how not about what. And I think maybe it's a historical accident, it's kind of the nature of our tool, our computer tool that we have to speak to it unambiguously, but it's only a tool for how, only a tool for sort of half of what we are talking about, or some fraction that we are talking about. And it imposes requirement on precision on us, on ambiguity, and the *what*, which to me is much more important than the *how*, it's a more fundamental part of the game, the tool doesn't impose it on us. And precision is hard and hard becomes unpleasant, so I think just for this kind of historical accident we've developed willingness and abbilities to be precise about hows. 

> It is not only not right, it is not even wrong.

Wolfgang Pauli came up with this description which I think applies to most software development. So it's not even wrong because there's no clear question, it's like an answer without a question. So that's what I would say: software without a specification is an answer without a question. Is it right is it wrong? We don't even know what it means. It's not even wrong I would say. And why does this matter?

I love this quote from Bertrand Russell

> Everything is vague to a degree you do not realize till you have tried to make it precise.

So Bertrand Russell in case you don't know, devoted years and years of his life, this was his life mission, to make things precise that people didn't really understand vague. So he found himself in the european mathematical community and was uncomfortable with the level of precision that was accepted in the day and he started poking at you know, professors and other practicing mathematicians, and they weren't really very receptive. They didn't like having it pointed out that the conclusions they were drawing were perhaps not sound, as they were based perhaps on unsound foundations. So what does it have to do with software?

You might not want to write down a clear specification because you're comfortable, you think I basically know what I'm doing, I've got this kind of assignment, I've had a chat with my coworkers or my boss, or just internally in my head, I basically know what it's for, now let's get to the real work and do an implementation. All I suggest is what Bertrand Russell points out here which is that if you think you know what you're doing but you haven't made precise, you're probably wrong, or you're very likely wrong because the imprecisions don't come to the surface until you really try to make them precise. So don't be reasurred by a kind of informal like, ahaaa.., I pretty much know what I'm doing, it's time to go on. This applies particulary to computer programming, and there's this old,old quote by Demosthenes:

> What we wish, that we readily believe.

And as a software delevoper, when I'm in the mode of trying to get something done, which is a lot of the time, sometimes I'm like trying to understand, or I'm trying to experince art or something, when I'm in the mode of trying to acomplish a goal, then, then what? The idea that my program might not be correct, or I might even understand what the question means, *is it odds with my desire to move on to the next thing*? I wanna take a break, I wanna raise, I might just wanna do you other things that are more interesting, so I wish that my program were correct. That means I readily believe that it's correct. So I'm suggesting we have a built in bias to ignore the possibility that we don't even understand what the question is. So not even is it a possibility as Bertrand Russel pointed out that there's an important vaguery hidding but also I think there's, we have an internal psychological bias that makes it quite likely. So that's my pitch for precision, not because precision is important in itself, because it's a way to help us not fool ourselves and we have a tendency to fool ourselves. 

I prefer interactive talks to monologues even if I'm a *monologist*. So absolutely, questions whatever, please pipe up. 

### Denotative programming

Peter Landin recommended "denotative" to replace ill-defined "functional" and "declarative"

Preperties:

- Nested expression structure.
- Each expression *denotes* something,
- depending only on denotations of subexpressions.

> "... gives us a test for whether the notation is genuinely functional or merely masqerading. (The Next 700 Programming Languages, 1966)

So, we are this community that we call functional programming and that term has been around for quite a while. In 1966 Peter Landin has published a seminal paper, [The Next 700 Programming Languages](https://www.cs.cmu.edu/~crary/819-f09/Landin66.pdf), and in this paper he points out that this term that was being used even at the time, functional programming, is really ill defined, it's this fuzzy term along with *non-procedural* I think is one, and *declarative* is another. And if you've ever used the internet :smiley:, particulary programming language communities,programmers blogs, that kind of thing, you might have seen people arguing over whose language is more functional, is Scala really a functional language, is Lisp or Scheme functional, is Haskell with IO functional? I've poked around that one. And what I see is that it's easy to start with these conversations and it's very hard to get anywhere helpful. And I think that Peter Landin explained in his 1966 paper why that was, and it's because these terms are not well defined. And he recommended the alternative, and this is what I recommend: I would say, deprecate the term functional and let's talk about something that has clear meaning and then revisit all these conversations if we still want to revisit about whose language is functional and whose not and how to be more in that direction. And Peter Landin suggested the term *denotative*, defined by having three properties. One is that, we have a nested expression structure, so it's an expression within a expression. And the second is that every expression *denotes* something. So now we are really getting to the heart of things, it's not just syntax, so structured expressions is syntactic property, but that expressions mean something, *denotes* something, and that something depends only on the denotations of the subexpressions. So these are his three properties of what he calls denotative language, or denotative programming style. And these three properties, and in particular the third one he says, it gives us the test for whether the notation is genuinely functional, or merely masquerading. So that's why I sometimes I kind of poke around and say maybe that's functional but it's not genuinely functional.

**Question from the audience**: *When Landin is saying denotation and like today when I'm saying denotation are we talking about the same thing?*

Answer: Yes, we are. So this paper was published in 1966, there was early work by Chris Strachey, at that time, what he called [denotational semantics](https://en.wikipedia.org/wiki/Denotational_semantics), and Peter Landin, if you haven't heard Peter Landin, Peter Landin is one the few, really kind of founding fathers, founding parents of our discipline, and not just of functional programming but of understanding programming languages. So Peter Landin is extremely important figure in our history, and made contributions. For instance, Peter Landin is the one who realized: "Hey, wait a minute, lambda calculus and programming have something to do with each other." Peter Landin figured out something called [SECD machine](https://en.wikipedia.org/wiki/SECD_machine), that's something like, how can we actually evaluate lambda expressions with a machine, OK? So this is soo, we take this so deeply for granted. Peter Landin was on the ALGOL comittee, and that was where he noticed and he wrote a famous paper called [A Correspondence Between ALGOL 60 and Church's Lambda-Notation](https://fi.ort.edu.uy/innovaportal/file/20124/1/22-landin_correspondence-between-algol-60-and-churchs-lambda-notation.pdf), that's where he was realizing. This is *the* seminal paper on the idea of domain specific embedded languages. This paper gave rise, I believe, to Scheme, ML and Haskell among other languages. So very infuential. So that's the idea of denotative programming and it's a clearer question. If you ask is Scala functional, it's a non-question, right?, because functional doesn't mean anything, but is Scala *denotative*? Well, that's a much more interesting question. The question is can we give a semantics to is, in this compositional style, the meaning of an expression depends on the meaning of its components and if so, what does it look like and are we happy with the results? And if we're happy with them celebrate, if we're not happy let's steer. 

### Denotational design

Design methodology for "genuinely functional" programming:

- Precise, simple, and compelling specification.
- Informs *use* and *implementation* without entangling them.
- Standard algebraic abstractions.
- Free of abstraction leaks.
- Laws for free.
- Principled construction of correct implementation.

So *denotational design* is a term I came up with a few years ago, and it's a subject of this workshop and here's a little pitch, a little commercial, so, denotational design, I think it's like a wonderful, natural fit for functional programming, particulary this programming in which we understand specifications in a clear way, and it has these properties, I'm claiming.

It gives us precise, simple and compelling specifications. It addresses both the use of a library, the language so to speak, and it's implementation, but it doesn't entagle them. So, it's entirely clear about what the language means, what that distracting us with the implementation and concerns, and it entirely defines what it means for an implementation to be correct or not. It's correct if and onlf if it's outcome agrees exactly with the denotation. Doesn't mean they have to work at all similarly, in fact, denotations don't work, they just are. Implementations *do*, denotations *are*.

It relates nicely to a set of abstractions that have come out of mathematics for last few hundred years, which we have also been using in some modern typed programming language like Haskell and Scala, so standard algebraic abstractions, things like functor, monoid and applicative, and things like that. Foldable, traversable, monad... And if you follow this principle, where ever you follow this principle, what I am suggesting to you, and if I don't convince during this talk, I want you to challange me on it. So, what I'm claiming is that, where you apply this principle you do not have abstraction leaks. And another way to say this, I mentioned earlier today, is that, if you try to apply this principle and you reach a point where it just doesn't work, you have an abstracion leak, and that's a very useful information. It tells me where my abstraction leaks are, I redesign, I eliminate the abstraction leak, and to do it by finding a way to use this principle. 

And these abstractions are not, so, a standard abstraction like functor or monoid, it's a set of operations, but those operations have properties so there are laws. If you use this methodology the laws will hold. You cannot use this methodology and define an instance in which the laws do not hold. And sometimes to give you examples today, sometimes this process will also lead you to a correct implementation, in addition to giving you a specification that you can use, you can derive a correct implementation. It may not be the most efficient implementation but definitely it will be correct, and if you have a more efficient implementation in mind then it offers a path, or it suggests there is a path to look for, how do I get to my efficient implementation, from this specification, by sort of, correctness (?) steps, how do I kind of calculate, even though if it takes some ingenuity and creativity.

### Broad outline:

- Example, informally
- *Pretty pictures*
- Principles
- More examples
- Reflection

So this is what I wanna talk about today. I am going to give you one example that I intend to give rather slowly and thoroughly, and it is a kind of, something I want to invite you to participate in. So this part is not so much a talk, as a seminar or something like that, for us to investigate together how we might do a library design, and the library design in particular is one for inventory, for image synthesis. Something based on some, example came from something I did around the year 2000. So we will go through this process and I hope you get a feel for what it is like for me to do, to apply the denotational design, and hopefully fun example. And then I'll show you some pretty pictures, which, you know, maybe I'll show you the pretty pictures first and then we will reflect and say what are the principles that we used, and then I have a few more smaller examples, hopefully to give some sense this isn't just a one-off thing. And absolutely, discussion, participation, questions, objections, moral indignation, what ever you have, I invite it. *And I'm explicitly not asking you to believe anything I say today*. OK? I don't want you to believe anything I say because I say it, I want you to try it on and see if it, how it feels to you, which is what I did.

Example I want to talk about is image synthesis and manipulation. OK? That's the assignment. It's either your hobby or you just joined the group and that's the goal. We want to have some kind of groovy, a flexible fun to use library for doing image manipulation.

- How to start?
- What is success?

We want it to be efficient to, but I don't think I'm gonna get into that, and in fact my library that I did this is extremely efficient, it runs insanely fast. So couple of questions is, where do we even start?

Like, we're this team, a product development team, and this is our edict, image synthesis library, high-level, fun to use. So how do we start, how we define success?
Any thought on how we start on this project? Assuming some of you are programmers :smiley:

audience: "What are the things we are manipulating with?"

OK, yeah, so what are the things we are going to be manipulating, so, imagery, well, let's get more specific than that, OK? So what kind of things, and then what is success. Well? That's probably very tightly related to this question, so what kind of functionality, so when we implement this functionality and provide it, conveniently, correctly, maybe that's success. We might understand better these question when we come back to them. 

So, what is we are trying to build? I'm gonna suggest, we can do this brainstorming, what kind of operations, (...) think this phase is terrific and illuminatig so I just kind of threw out a bunch here. So this is a kind of function that I would imagine would come out of either you know, kind of a clear requirements statement or a brainstorm. 

### Functionality

* Import & export
* Spatial transformation:
  * Affine: translate, scale, rotate
  * Non-affine: swirls, lenses, inversions, ...
* Cropping
* Monochrome
* Overlay
* Blend
* Blur & sharpen
* Geometry, gradients, ...

So some way of getting stuff into the system manipulation out the system, so it's purely synthetic imagery it's gonna be nothing interesting at all coming in from the outside. If I'm gonna do some kind of manipulation of given imagery well I'm gonna want to import something from maybe some standard file formats, and I'm gonna do all my manipulations, and then at the end I want to give something out, so maybe I have some kind of import/export kind of thing, and those are just the endpoint, now all the interesting stuff is in the middle. 

So one thing that comes up alot is spatial transformation so that can be things like translate, scale, rotate, OK? These are all linear, affine transformations they are called, but there are also interesting things like doing a twist in the middle or a bulge, or something like that. And I think I said I will show you some pictures so let's pretend that this is part of our information we get about what we are trying to do, let's see, [conal.net/Pan/Gallery](http://conal.net/Pan/Gallery/), so here are a bunch of pictures I made a long time ago with this language, but not really, these aren't the pictures that I describe, these are croppings and samplings of the pictures I described, so these are things I know how to put on the web, these are the kind of things that can be exported, and each of these is a link to a much higher res version. OK? So this is like a playful exploration of image synthesis. All of these are kind of collections of lots of examples. These maybe give you some sort of feel, as you can see, it's not toward kind of practical problem solving, advertisements or something like that, you know, more playful kind of stuff.

All right, In some of these examples, there's kind of bulges that happen, like a magnifying glass effect, and some there are spiraly things happening which you might think of as, I have something linear and then I wrap it into a spiral, infinitely out, infinitely in. So there are all these interesting non-affine, and then there is turning things inside out, makes beutiful pictures. And there is cropping. I've got some image, and I just wanna like maybe crop a circular region out of it or some funny shape, there is this monochrome image, it may sound kind of silly, but it's actually quite useful, I may just want the images red, just red. Now that's likely to be my final output, maybe I'm a modern artist or something, but still likely even in that case, but combined with cropping and overlaying and things like that it's useful. So another operation is *overlay*, if I have two images and put one of them in front of the other one, so that shows up quite alot, reality in our image systems are doing that all the time, by looking at here there's all kinds of overlays happening. And then there is *blending*. I have two images and I want some sort of blend between the two, maybe some kind of linear interpolation, or something like that, some midway point between the two, and then there are area operations which are like blurring and sharpenning so I might have an image that I want all of it or some part of it to be blurred, or I might want to exaggarate trasitions sharpenning it, and there is all kinds of other toys, various geometric things, funny shapes, and then smooth gradients and things like that so, these are the kind of things that we'd like to be able to express.

### API first pass

So we're gonna define an API that's gonna be used in some language. We get to choose a functional language, maybe we are told to, maybe we get to. So, I wanna talk about designing functional APIs, so what kind of things show up in a functional API? I don't mean what kind of functionality, I mean... :smiley:

Questions: So the observations that transform are a monoid.

Answer: Yes, transforms are a monoid in an interesting sort of way, and I want to suggest we come back to that when we think about how can we elegantly structure.

So in a functional language, in an imperative API the operations are gonna be about how to make up state, initialize it, and side effect it. OK? I've looked at alot of imperative imaging API's and they are like that. Build up some graphic state, set some brushes, you know, this kind of stuff, modify some output that's being accumulated, maybe it's on the screen or an offscreen buffer. So it's all about creating, initializing, modifying state, releasing state, that kind of thing. Functional API's are not like that. Functional API's, we want to describe things that *are*, and then building blocks for combining them. So we're gonna have instead of state and operations on it, we're gonna have types and values of those types and values of those types, just because that's the kind of programming that we do. So a good question to ask is what types are gonna be involved? OK, we don't yet have to know what those types mean, we can be a little fuzzy about it, because this is a process of clarifying our ideas.

```haskell
type Image

over       :: Image -> Image -> Image
transform  :: Transform -> Image -> Image
crop       :: Region -> Image -> Image
monochrome :: Color -> Image
-- shapes, gradients, etc.

fromBitmap :: Bitmap -> Image
toBitmap   :: Image -> Bitmap 
```

This is an example of taking the previous slide and starting to give it some concrete shape, some concrete specific shape and again, so far it's just the API, it's not what anything means, and certainly not how to implement it.

So we might have a type `Image`, so imperative API's don't have a type `Image` or if they do, they mean something off at of a file, or something like that, they don't mean it in compositional sense. But we instead of composing side effects, we're composing images. It's a functional language, so the idea of rendering or displaying or updating screen is not even part of the API, we're just talking about what the images are. So in each of those operations I mentioned in the previous slide, it's gonna show up somewhere as an operation with a type. So overlay for instance, is something that takes two images and gives an image, and in imperative API you dont' see overlay. You see a couple of pieces of code that do side effects, one of them being executed after the other, that's how overlay shows up in an imperative API, and our API overlay is an actual operation in it and it is about, or the expression of overlay of two images, it doesn't mean it goes anywhere, it doesn't mean it modifies anything yet. For instance, we might never use the result, or we might use it more than once, or we might transform it somewhere before it shows up. So that's overlay.

And then there's transformation. If I have an image I might want a bigger version of that same image, or a version somewhere else, or a version that's been interestingly warped, so that's what transformation does, that's what transformations are for. Makes sense so far?

Cropping also, we want to crop an image, what we need to describe some shape, I'll call that a region, so, what do I mean by image, what do I mean by region, what do I mean by transform, all this is gonna become clear, that's the creative process.

And then, a monochrome image, I just give it a color, and I get an image of that color. And you might expect me to say, a color and how big it is or something like that, but we'll come back to that question if we have enough argument here.

- question from the audience:

You're wondering why monochrome doesn't say give me a color and an image and I'll give you an image? Aha.. What would that operation intuitively, what would it mean informaly? Oh! I see, I have an image, and I want you to make that image red or something like that. Yeah, that's interesting. So here's a question which we kind of put up in the air, can we describe that operation, can we build it out of simpler parts? One of my goals, I'll make that explicit, is I want to define the simplest possible building blocks. If I have something that is somewhat complex operation I'd really like to make it not be permanent but definable out of simpler pieces. Why? So that I have a simpler set of primitives that I have to figure out what they mean and how to implement them. Like Robbies talk this morning, his keynote about this macro system, it's very nice to have a small set of language primitives, this is part of the ethos of Scheme, very small set of language primitives and then the abbility to define richer things in terms of simpler things and it means that my core is quite simple and implementation of those more complex can be quite simple by translation or if you want that specification you can more efficient implementation(?).

- question from the audience:

So already some critique of this API, *overlay*, that sounds a bit ad-hoc, aren't there other way to combine images besides this one, and maybe this one can be built out of something simpler or more general. Yes, and I completely agree. So, what I am doing here, and absolutely welcome what you are doing, you're getting a little ahead which is fine, this is participatory, this is your talk, this is our session together, so we could, throw these all ideas up, and then critique them or we could critique them while we throw them up, it doesn't matter. There may be some personality issues, like I'll get really offended if you challange my idea or something but I actually won't so we don't need to do that. Yeah.. So already *overlaying* sounds suspiciously specialized. And in this last two we're just getting the information in and out of the system.

But it's important, I'm not assuming that `Bitmap` and `Image` is the same thing, so what I am assuming is that there's this thing called bitmap which is like a jpeg, or who knows, it's just this kind of array of pixels which you know, have some kind of depth and that kind of thing, assuming there's going to be some stuff outside that we're going to want to bring some information in, do cool stuff, and then also after we're done with cool stuff, we're going to put out into. But I don't want to assume that that's what we mean by `Image`, and we will discuss there are very good reasons to not make that choice.

* How to implement?

Now let's talk about how to implement. :smiley:

- *wrong first question*

Let's *not* talk about how to implement, OK? So, I enjoy implementation, I am guessing you enjoy implementation or you wouldn't be here, but it's not a good first question, in part because it's not a well defined question. *How* is an answer, *what* is a question. How do we implement what, how do we implement this, No, we don't even know what this means, what's an image, these are just kind of intuitive and formal ideas, so let's be more clear about what these mean and then ask the question of how to implement. Does that makes sense, or let me put it differently.. Is that? Is there anybody who will like to go more into, more about the separation between why we would wanna make something more clear other than how to implement it at this point? I don't hear it talked about a lot anymore this distinction between specification and implementation.

- Question from audience:

Identify symmetries? ... Ah, are you asking am I sort of content with this API? Is that what you are asking? I'm just making a guess, I don't think I'm right, I'm just trying... How they would be used? ... OK, by symmetries, do you mean, hm, maybe there's some structure that I realize I could refactor? Or... do you mean something like visual symmetries, I'm guessing you don't but maybe you do?... Oh, yeah, yeah, understanding maybe the set of images that are expressible in this form? :smiley:, I think I'm not converging, but I'm roughly, infinitely patient, (Smiles) about understanding what you are wanting to say. OK, ok, come back to it, and say oh! now I know how to get this through your thick skull Conal.

So, *how to implement*? NO (:smiley), different question, OK? What to implement? OK, and we did not just say what to implement in a precise way, so let's get more precise, so.

* What do these operations mean?

What do I mean by mean? There's a more fundamental question which is what do the types mean.

* More centrally: What do the *types* mean?

Once I know what the types mean, then I can at least understand the question of what do the operations mean, because the operations say, *over* takes two images and gives an image, we can't possibly talk about what over means without understanding what an image means, right? Because it's a function on images and gives an image. So this is the central question. Doing the API design we're going to define some types, types of values, because we are doing functional programming, and type functional programming in particular and we want to say what do the operations mean.

- Question from the audience:

OK, let me see if I got you, and I want to encourage people to sit closer, for two reason, one is to be more included, and the other is for me to hear you more easily. If you don't want to do that I'll just try harder to hear. So, I think what you are asking is, when you say *mean*, do you mean how to implement them, and if you don't mean that, what do you mean? OK, thanks. Yeah, I absolutely *do not* intend to say how do we represent the types or how do we implement the operations, so that would be confusion. What I really mean is to clearly specify, I want to give some formal specification, somehow, that tells me as an implementor exactly what it means ... It's not that it tells me whether my implementation is correct or not, it tells me what it would mean for an implementation to be correct or not, that defines correctness. *Then* I know what to aim for. OK? It also tells me the breadth, so I want to be constrained enough to know what correctness is, I don't want to be over-constrained because I don't want to be told how to implement something, because I may have a lot of ideas, and here I'll maybe touch on some of the ways I've implemented this vastly different from each other, OK? So I don't want to be constrained in an implementation, or representation. I do want to get very clear, about, something about the specification, and there's a particular style of specification, and that's the one I'm talking about today, which is *denotational*. And I'll show you supersoon what that looks like. I am so sorry that's freezing in here, right? I think it's even worse for you than for me there. It's blowing here, yeah I don't know... So I appreciate you hanging in, if you have to leave, if your face turns blue, I won't be offended.

So, central question for me is *what types are there and what do they mean?*. What is a mathematical model that I can use to think about those types in a precise way.

So here, so far, the central type of interest is `Image`. There were some other types, transforms, colors, regions. So let's start with one, let's start with images. So what is an image?

Specification goals:

* Adequate
* Simple
* Precise

Before entering that question what do we want out of an answer and I am suggesting these three properties. I want my mathematical model to be adequate, if it's not adequate then I can't express the things I want to express. Either I want to or my users. It has to be simple. Why does it have to be simple? It's because I'm basically not very smart, I mean, I mean I'm smarter than a mouse, right? But I'm hopefully not as smart as my children will be and so on, and all of us collectively so I have very limited ability. So I need the specification to be simple enough that I can reason practicably and reliably. And then precision. And why? So that I *really* have simplicity and adequacy, not just think I do. So precision is all about helping me not fool myself, because I'm psychologically inclined to fool myself, so precision is a coping mechanism. OK?

Now, let's go back to this question what an image is.:smiley:. This is a collaboration. What is an image? OK? And I want a lot of bad suggestions, I want lots of stupid ideas, what I really mean is, you know, please don't be shy about making suggestion, because every suggestion is illuminating either for it's strengths or it's weaknesses or both, and I apology for flashing my answer which I didn't mean to bias you with.

When I've done this exercise, people have quite a variety of intuitions. It's what? A matrix of pixels? Sure, totally. So by matrix of pixels I assume you mean some kind of rectangual, regulary spaced collection of pixels and a pixel is... yeah, colors or maybe something that represents like 24-bit values to be interpreted in certain way. Yeah, so that's a very common answer, so it could be an array or matrix of pixels. Other? Oh, OK, so already we are getting criticism from the very start, programmers.. yeah, so maybe there's a little bit of a discrete bias here that we might wanna question. (Listening to a question)... OK, so it's sort of functioney view, yeah, and last year I talked about this before I talked about FRP, and so nobody made that suggestion but that's certainly one possibility as absurd as it sounds. :smiley: Anybody else? You'll see I'm joking, hopefully at least... A way of collecting and saving light? Sure! Yeah, absolutely. And so we might think that it's a way, hmm, that might being an algorithm, a function, a recipe, something like that, yeah, so, not to hurl insults, but an object oriented sort of perspective might be about it's an interface that has some kind of operations.. yeah, other thought? Yeah, sure, so an image might be represented by a collection of shapes, so for instance, in something like a pdf or postscript, yes, so pdf and postscript are all about image synthesis and manipulation, and they are all about geometry, so it could be some shapes, in other words some descriptions and then... totally, yeah (seeing?) graph, or it's called linear that's all that was. No, I worked in computer graphics, (shiver of disgust?)... so yeah absolutely, It could be some kind of data structury thing which contains in it some kind of descriptions of objects, 2 or 3d dimensional, some description of lights and cameras, kind of some information out of which one can synthesise an image, and a lot of particulary 3D API's are that way... OH! That sounds like a poets answer to me. So, Dicks contribution is, yeah an image is a short line or a short body of text that conjures a what in someones mind? Oh, conjures a picture in the readers mind, I see. I might recursively ask you what a picture is and wonder if you're gonna tell me it conjures an image in readers mind but I won't do that. Thank you. Yeah, absolutely, so again it's something that's like evocative in a sort of more poetic end of the spectrum than like a 3D scene graph API kind of a thing... Cool, so here is an interesting perspective is that maybe we were a little too focused on color, so an image is being sort of color over the discrete, continuous and finite, infinite whatever space, maybe there are other things as well that we maybe want to array over space. Maybe it's the overspaceness that matters more. Yeah, OK. 

So we can take all of these suggestion, if we are doing kind of classic brainstorming write them up on the board. We have our goals which I suggested here and we can evaluate them all and see where they lead. If we leave out precision we are not gonna get a very accurate estimate of where they lead, because for instance, simplicity, many ideas are simple only because they are not precise and when made precise they become quite complex, or their complexity becomes apparent. It's cold, sorry, god..

So my personal answer which especially if you've been listening me talk about FRP is perhaps somewhat perdictable, so my answer is it's an assignment of colors to 2D locations and I'll be more clear what I mean by that.

- My answer: assignment of colors to 2D locations.

This is sort of the same answer in a less specific form than an array or a matrix of pixels. So that's, you know, an array it's an assignment for every slot in my array I assign one color to it, but it leaves kinda little more open ended what the shape of the container is. 

- How to make precise?

```haskell
type Image
```

If we start with this intuition, and then we want to make it precise so that we can understand the implications of it, we can do this to every idea that we've talked about, and some of them we might learn very early what some of it's limitations are, and that's an extremely fruitful part of this exercise. So let's make this precise, and here's the form in which I do it and this is the denotational form. 

```haskell
mu :: Image -> (Loc -> Color)
```

So we have a function, I'm calling it `mu` (greek letter), mu is supposed to suggest *meaning*, *m* right? So mu is a meaning function and the idea is that I want to say what an image means, or give you a model for relating to images, by, it's a mapping so mu is a mapping from images to something, and that something I'm saying is assignment of colors to locations and there's a simple precise to say that which is a function from location to colors. OK? Now over here on the right, so on the left of this main arrow, is the type we are interested in (`Image`), on the right is another type (`Loc -> Color`), for the most part you can think of that type as being, what we mean by functions in Haskell, what I really mean is mathematical functions, which *is* what we mean by functions in Haskell, but it's not what we mean by functions in most other languages, like an ML function, a LISP function, a Scala function, is it a function because it has side effects? I mean really a mathematical function. So you can either think of it as a math function, or you can think of it as a kind of a function in a purely functional language. But it's kind of important to make that distinction because it doesn't have to be implementable, OK?, because we're never gonna implement *mu*, we're just gonna use it to understand our library design and evaluate it as it emerges. 

- What about regions?

OK, so but `Image` was just one type, and I'm not saying that you should somehow know why this is a good choice, we're gonna look at it, I think. And then we can examine alternatives. So what about regions? Well, anybody have an idea what a region might mean? Locations to what? Ah... to opacity? Interesting, OK. So we might also talk about opacity over space, any other thoughts? Oh, a list of locations, yeah, that's actually quite common, used, in awful image, I don't mean this is an awful suggestion, what I mean is it so happens that a lot of the image, strike the word afwul, that choice shows up in a lot of them, often in a kind of compressed form, it maybe like a set of rectangles, that's quite common?... A function from location to boolean, quite similar suggestion as well, yeah, OK, same suggestion. And then... yeah sure, (?) polygon, so I might have some primitives for describing my regions, like polygons, and then I might mean a bunch of them, a union of them, perhaps countable, interesting choice, countable... 

```haskell
mu :: Region -> (Loc -> Bool)
```

So this is my choice, this is one of them, which is it's a function from locations to booleans, if you known this. Function to boolean is isomorphic to another mathematical notion that we use a lot, I hope you know this, which is sets. So function from location to boolean is the same as set of locations, something called characteristic function if you took math a while ago like I did. So another model is to say a region, the meaning of a region is a set of locations, that's very similar to the suggestion of a list of locations. So we could also say it's a set of polygons but a set of polygons is a more complex, it's maybe more efficient, but uh wait a minute, we're not talking about efficiency, that's a non issue yet, we may want to represent out set by a collection of polygons for instance... OK, let try to get what you're saying




