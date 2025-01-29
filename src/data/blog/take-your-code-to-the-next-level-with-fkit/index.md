---
title: Take Your Code to the Next Level with FKit
date: 2014-10-13T00:00:00.000Z
---
Recently I've been busy working on [FKit](https://github.com/nullobject/fkit):
a functional programming toolkit for JavaScript. It provides many functions for
solving common problems with functions, objects, arrays, and strings.

![FKit logo](fkit.png)

I wrote FKit because I believe that JavaScript is lacking the kind of standard
library that many other languages benefit from. A good example is the `prelude`
library in Haskell: it provides developers with an amazing toolbox to write
their Haskell applications with. Why should JavaScript developers have to
bother solving these common problems over and over again?

Other libraries have attempted to address this issue (Underscore, Lo-dash,
etc), but in my opinion they're rudimentary. The implication is that you often
have to 'reinvent the wheel' while writing JavaScript day-to-day.

FKit is my attempt at a _standard library_ for JavaScript. It aims to provide
reusable building blocks while maintaining a laser focus on everyday utility.
This article explains some of the core features of FKit and gives some
background to many of the functional programming concepts it uses.

## Curried Functions

![Curry](curry.png)

Most functions in FKit are already _curried by default_, so it's worth spending
some time explaining curried functions.

A _curried function_ is a function that instead of taking multiple arguments
takes exactly one argument. It returns another function that takes exactly one
argument, and so on. When all the arguments are specified, the result is
returned.

To illustrate this concept let's define a curried function `add` that simply
adds two numbers together. It takes a value `a` and returns another function
that takes a value `b`. When `add` is applied to the values `a` and `b` then
the result `a + b` is returned:

```js
function add(a) {
  return function (b) {
    return a + b;
  };
}
add(1)(2); // 3
```

With FKit you can easily curry a function of any
[arity](http://en.wikipedia.org/wiki/Arity) using the `curry` function:

```js
import { curry } from "fkit";
const add3 = curry((a, b, c) => a + b + c);
add3(1)(2)(3); // 6
```

## Partially Applied Functions

![Fx](fx.png)

What happens when we don't specify all the arguments to a curried function?
Let's find out.

First, let's define a curried function `mul` that multiplies two numbers
together:

```js
function mul(a) {
  return function (b) {
    return a * b;
  };
}
mul(1)(2); // 2
```

When we apply `mul` to one value only, we get what is known as a _partially
applied function_ instead the result `a * b`:

```js
mul(2); // function
```

Here the `mul` function has been _partially applied_ to the value `2`. This
results in another function which simply multiplies a given number by `2` in
other words, it _doubles_ a given number.

This is actually quite useful. For example, we can assign the partially applied
`mul` function to the variable `double` and apply it to some other values:

```js
const double = mul(2);
double(1); // 2
double(2); // 4
double(3); // 6
```

Many of the functions in FKit can be partially applied to expressively and
elegantly solve problems. Let's take a look at a few examples using FKit.

Here we multiply each number in a list by two:

```js
import { mul } from "fkit";
[1, 2, 3].map(mul(2)); // [2, 4, 6]
```

By using the `reduce` function with `add` (a binary function), we can sum all
the numbers in a list:

```js
import { add } from "fkit";
[1, 2, 3].reduce(add); // 6
```

To filter the all the numbers in a list which are greater than one, we can use
the `gt` predicate:

```js
import { gt } from "fkit";
[1, 2, 3].filter(gt(1)); // [2, 3]
```

## Function Composition

![Notes](notes.png)

Function composition is a powerful way to specify a series of functions, where
each function takes the result of another function as an argument. In other
words, `compose(f, g)` is equivalent to saying `f(g(a))`.

Now let's define a function `compose` that composes two functions together:

```js
function compose(f, g) {
  return function (a) {
    return f(g(a));
  };
}
```

Using our `add` and `mul` functions from earlier, we can compose a function
`doubleAndAddOne` that doubles and adds one to a given number:

```js
const doubleAndAddOne = compose(add(1), mul(2));
doubleAndAddOne(1); // 3
doubleAndAddOne(2); // 5
doubleAndAddOne(3); // 7
```

FKit allows you to easily compose any number of functions together using the
`compose` function:

```js
import { compose } from "fkit";
const myFunction = compose(f, g, h);
```

## Lists

Have you ever wondered why you can call `reverse` on an array, but not on a
string?

```js
[1, 2, 3].reverse(); // [3, 2, 1]
"foo".reverse(); // TypeError: Object foo has no method 'reverse'.
```

This is because arrays and strings are fundamentally different data types in
JavaScript. Other languages don't make this distinction after all, a string is
really just a _list of characters_. Imagine how useful it would be to use the
same functions on different kinds of lists, whether it be an array of numbers
or a string?

![List Monster](listmonster.png)

FKit provides many list functions and they all work in exactly the same way for
arrays as they do for strings. This seemingly simple abstraction is very
powerful.

To highlight this point, let's look at some basic list functions on both arrays
and strings:

```js
import { head, init, last, tail } from "fkit";

head([1, 2, 3]); // 1
head("foo"); // 'f'

tail([1, 2, 3]); // [2, 3]
tail("foo"); // 'oo'

last([1, 2, 3]); // 3
last("foo"); // 'o'

init([1, 2, 3]); // [1, 2]
init("foo"); // 'fo'
```

We can also create new lists by adding elements to lists:

```js
import { append, intersperse, prepend, surround } from "fkit";

append(3, [1, 2]); // [1, 2, 3]
append("o", "fo"); // 'foo'

prepend(1, [2, 3]); // [1, 2, 3]
prepend("f", "oo"); // 'foo'

surround(0, 4, [1, 2, 3]); // [0, 1, 2, 3, 4]
surround("¡", "!", "hola"); // '¡hola!'

intersperse(4, [1, 2, 3]); // [1, 4, 2, 4, 3]
intersperse("-", "foo"); // 'f-o-o'
```

Joining lists together? No problemmo:

```js
import { concat } from "fkit";
concat([1], [2, 3], [4, 5, 6]); // [1, 2, 3, 4, 5, 6]
concat("f", "oo", "bar"); // 'foobar'
```

Building new lists? It's a snack:

```js
import { replicate } from "fkit";
replicate(1, 3); // [1, 1, 1]
replicate("a", 3); // 'aaa'
```

## Combinators

![Map](map.png)

JavaScript provides several _combinators_ for working with arrays, for example
`map`, `filter` and `reduce`. Combinators are higher-order functions (functions
that takes other functions as arguments) that iterate over arrays – or more
generally, _any_ data structure – in some way.

FKit also provides all of your favourite combinators, plus a few more you may
not have encountered before. They also all work on both strings and arrays!

`map` takes a function and applies it to every element in a list, returning a
new list:

```js
import { inc, map, toUpper } from "fkit";
map(inc, [1, 2, 3]); // [2, 3, 4]
map(toUpper, "foo"); // ['F', 'O', 'O']
```

`filter` filters the elements in a list using a predicate function:

```js
import { eq, filter, gt } from "fkit";
filter(gt(1), [1, 2, 3]); // [2, 3]
filter(eq("o"), "foo"); // 'oo'
```

`fold` folds a list into a single value using a binary function and a
starting value:

```js
import { add, flip, fold, prepend } from "fkit";
fold(add, 0, [1, 2, 3]); // 6
fold(flip(prepend), "", "foo"); // 'oof'
```

`zip` zips the corresponding elements from two lists into a list of pairs:

```js
import { zip } from "fkit";
zip([1, 2, 3], [4, 5, 6]); // [[1, 4], [2, 5], [3, 6]]
zip("foo", "bar"); // [['f', 'b'], ['o', 'a'], ['o', 'r']]
```

`concatMap` maps a function that returns a list over every element in a list,
and concatenates the results:

```js
import { concatMap } from "fkit";

function p(a) {
  return [a, 0];
}
concatMap(p, [1, 2, 3]); // [1, 0, 2, 0, 3, 0]

function q(a) {
  return a + "-";
}
concatMap(q, "foo"); // 'f-o-o-'
```

## Immutable Objects

Even though it leads to much simpler application development, JavaScript
doesn't enforce any constraints on the [immutability of
objects](http://en.wikipedia.org/wiki/Immutable_object). Developers tend to
solve problems by mutating the state of their objects, leading to hard-to-find
bugs.

FKit provides several functions to make it easy to work with immutable objects.
To begin the next example, let's define a list of shapes:

```js
const shapes = [
  { type: "circle", colour: "red", size: 1 },
  { type: "square", colour: "green", size: 2 },
  { type: "triangle", colour: "blue", size: 3 },
];
```

If we want to get the colour of each shape we can use the `get` function:

```js
import { get } from "fkit";
shapes.map(get("colour")); // ['red', 'green', 'blue']
```

What if we want to change a property of the shapes? We can use the `set`
function to do that:

```js
import { set } from "fkit";
shapes.map(set("size", 100)); // [{..., size: 100}, {..., size: 100}, {..., size: 100}]
```

Importantly, in the above example the original shapes remain unchanged. FKit
makes a copy of each of the shapes and sets the `size` property to `100`. FKit
will also respect the prototype of the original object and ensure that the copy
has the _same_ prototype as the original.

## Learn Some More

Hopefully by now you've learned something about FKit and some of the functional
programming concepts behind it. What you've seen in this article is only the
tip of the iceberg.

I encourage you to take a look at the [FKit project on
GitHub](https://github.com/nullobject/fkit) and read through the [API
docs](https://fkit.joshbassett.info) if you want to learn more.

## Credits

The images in this article are borrowed from the wonderful book [Learn You a
Haskell for Great Good](http://learnyouahaskell.com) by Miran Lipovača.

This work is licensed under a [Creative Commons
Attribution-NonCommercial-ShareAlike 4.0 International
License](http://creativecommons.org/licenses/by-nc-sa/4.0/).

[Discuss this article on Hacker News.](https://news.ycombinator.com/item?id=8448194)

