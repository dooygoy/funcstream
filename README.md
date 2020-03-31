# Functional thinking with Haskell

This is an exploration of a beautiful programming language Haskell and functional thinking.
I am learning from several books and documenting the learning here.
Sources used are

### Books
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


### Online courses
  * [Bartosz Milewski Super Awesome Category Theory Lectures!](https://www.youtube.com/user/DrBartosz/playlists)
  * [Functional programming in Haskell: Supercharge your coding](https://www.futurelearn.com/courses/functional-programming-haskell)
  * [Bartosz Milewski - School of Haskell](https://www.schoolofhaskell.com/user/bartosz)
  * [Functional programming in Haskell](https://www.youtube.com/playlist?list=PLJ5C_6qdAvBFJP1RiUrUUJI4GEhnJhgQw)
  * [C9 Lectures: Dr. Erik Meijer - Functional Programming Fundamentals](https://youtu.be/UIUlFQH4Cvo)

### Talks

  * [Why is Haskell so Hard to Learn and How to Deal With It](https://youtu.be/RvRVn8jXoNY)
  * [Stop Treading Water: Learning to Learn](https://youtu.be/j0XmixCsWjs)
  * [Why algebraic data types are important - Bartosz Milewski ](https://youtu.be/LkqTLJK2API)
  * [PureScript: Tomorrow's JavaScript Today](https://youtu.be/5AtyWgQ3vv0)
  * [LambdaConf 2015 - Learn Functional Programming with PureScript John A De Goes](https://youtu.be/LqYfdmb0eUU)
  
### Papers
 
 * [Eugenio Moggi: Notions of computation and monads](https://person.dibris.unige.it/moggi-eugenio/ftp/ic91.pdf)
 * [Raul Rojas: A Tutorial Introduction to the Lambda Calculus](https://arxiv.org/pdf/1503.09060.pdf)
 * [Why calculating is better than scheming](https://www.cs.kent.ac.uk/people/staff/dat/miranda/wadler87.pdf)

### Other:

* [Steve Yegge - Execution in The Kingdom of Nouns](http://steve-yegge.blogspot.com/2006/03/execution-in-kingdom-of-nouns.html)
* [Monday Morning Haskell](https://mmhaskell.com/)
* [Why are partial functions (as in `head`, `tail`) bad?](https://www.reddit.com/r/haskell/comments/5n51u3/why_are_partial_functions_as_in_head_tail_bad/?utm_source=share&utm_medium=web2x)
>The problem with partial functions is that they're liars. Consider head: its type is [a] -> a, which means "give me a list of as and I'll give you an a". So I give it [] - does it give me an a? No, it doesn't, it throws an exception instead.
And when functions start lying about the things they return, you can no longer reason about them.
