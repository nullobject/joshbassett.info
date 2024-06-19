---
title: 'Hello Haskell, Goodbye Scala'
date: 2013-01-22T00:00:00.000Z
---
**I spent** a fair chunk of my free time last year teaching myself functional programming in [Scala](http://scala-lang.org/). I read [Scala For The Impatient](http://horstmann.com/scala/) and countless other blog posts. I laughed, cried, and managed to learn a great deal.

Earlier this year I decided to ditch Scala for [Haskell](http://haskell.org/) and get back to some purely functional roots. I thought I'd reflect on my decision a little.

## Functional By Convention

Scala is a hybrid language which supports both [functional programming](http://en.wikipedia.org/wiki/Functional_programming) and [object-oriented programming](http://en.wikipedia.org/wiki/Object-oriented_programming_language). It runs on the JVM which means it's fully interoperable with the amazing wealth of existing Java libraries (for example [Apache Commons](http://commons.apache.org/)). It also has an advanced type system, a concise syntax, a robust standard library, and industrial-strength [concurrency framework](http://akka.io). What's not to love?

For one Scala is not a [purely functional](http://en.wikipedia.org/wiki/Purely_functional) language, which in my view makes it a poor choice for learning functional programming. I'm not saying that it's a bad language to learn, in fact I think there's quite a lot to like about it. What I'm saying is that because functional programming in Scala is only a convention (i.e. it's not enforced by the language) you don't learn functional programming as quickly as you would with a purely functional language.

Because you aren't forced to do things the 'functional way' it means that if you get stuck solving a problem then you're likely to fall back on old habits to get the job done. Scala's flexibility allows you to come at a problem from multiple angles (i.e. imperative and functional), this seems like a good thing™. The problem is that you cheated and didn't learn anything.

## Scalaz

I also spent some time learning about the [Scalaz](https://github.com/scalaz/scalaz) library. It expands your functional tool belt with a bunch of purely functional data structures.

Scalaz contains a lot of highly academic stuff which I was fairly sure I could live without. There were a few things however, which I was pretty sure I needed to know in order to level-up. For example [Functors](http://en.wikipedia.org/wiki/Functor), [Monads](<http://en.wikipedia.org/wiki/Monad_(functional_programming)>), [Monoids](http://en.wikipedia.org/wiki/Monoid), and [Arrows](<http://en.wikipedia.org/wiki/Arrow_(computer_science)>). But what exactly are these things, why do I need them, and how do I use them? Confusion set in.

If you're interested in learning about Scalaz there's a great series of [blog posts](http://eed3si9n.com/category/tags/scala/scalaz) by Eugene Yokota. There's also an upcoming book from Paul Chiusano and Rúnar Bjarnason called [Functional Programming In Scala](http://www.manning.com/bjarnason/) which explains some of the theory behind Scalaz.

Searching for answers always lead me back to an explanation in Haskell. It was around this time that I asked myself: why am I bothering to learn Scala/Scalaz when I should just learn Haskell?

## Learn Me A Haskell

For Christmas I bought myself a copy of [Learn You A Haskell For Great Good](http://learnyouahaskell.com/) by Miran Lipovača. Apart from being a great Haskell learning resource, it's one of the best programming books I have ever read. I'm part-way through my second reading and I feel like I've learned more about functional programming in the past six weeks with Haskell than I had in the past six months with Scala.

I don't regret taking the time to learn Scala, I guess it lowered the bar to grasp some of the functional programming constructs in Haskell. But if could do things differently I would _start_ with Haskell, and then move on to other functional languages.

Bring on the next six months!

