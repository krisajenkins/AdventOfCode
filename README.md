# Advent of Code

My entries for AdventOfCode.com, written in PureScript.

_Disclaimer: I'm not claiming that this code is beautiful, or even
100% correct, just that it earns me those precious, precious
stars. :-D_

## Building

``` sh
npm install -g yarn
yarn
yarn run bower install
yarn run pulp test
```

## Thoughts

### 2017, Day 22
Performance problems at first, but basically it's just Langton's Ant,
which I've done before.

### 2017, Day 21

Ugh! Did it, but this is my personal shame list. :-(

### 2017, Day 20

Part 1 was trivial. Part 2 took ages, but because I insisted on going
with maths rather than simulation, and I got one of the intermediate
formulea wrong, which took ages to find. I'm please I can still solve
a quadratic equation though!

### 2017, Day 19

Much easier than it looked at first!

### 2017, Day 18

Probably the most interesting challenge yet. I've solved it, but
hopefully I'll find time to circle back and actually do something
elegant. :-}

### 2017, Day 17

Phew! Editor problems and life getting in the way. Nearly didn't
finish this one. Good puzzle though.

### 2017, Day 16

Another fun one. I made a wrong assumption about how I could optimise
step 2, and that slowed me down, but ended up with a nice optimisation
that made the whole thing performant enough. :-)

### 2017, Day 15

Had to deal with integer overflow (!), but easy enough if you'll
forgive the fact that eachs solution takes 60s to run.

### 2017, Day 14

Much easier than it initially looked, thanks to Day 12.

### 2017, Day 13

Bit disappointed I could get this performant with lazy sequences, but
the sieving trick works nicely.

### 2017, Day 12

Enjoyable. I reckon I could write a simple parser blindfolded
now. This is great practice.

### 2017, Day 11

Fun. I've never looked into hexagonal coordinate systems before. :-)

### 2017, Day 10

This one was a bit of a slog. I'm going to start fearing the
Sundays. The spec could have been clearer. Lord alone knows how people
managed to solve this in less than 10 minutes. :-o

### 2017, Day 9

Fun. My parser practice is paying off. I would not like to do this
challenge without a combinator-style parser library!

Also, that's probably the easiest conversion-to-round-2 so far.

### 2017, Day 8

Probably my most elegant solution yet, just through using
Control.Fold. Also - wow - a real, decent-sized parser. :-)

### 2017, Day 7

Nice. Makes me want to sit down with a bowl of satsumas and
catamorphism tutorial.

### 2017, Day 6

This seemed like an easy one that just had a lot of legwork.

I have clearly gotten over my phobia of Data.Tuple.Nested, as I'm
using `/\` syntax everywhere now. :-)

### 2017, Day 5

Not hard, but hard to get performant. Part 2 really falls apart if
your implementation isn't quick enough.

### 2017, Day 4

Easy. I'm willfully over-using `Fold` here for the fun of it.

### 2017, Day 3

The hardest so far. I got the algorithm really quickly, but getting it
right took me ages. I'm very gad this challenge happened on a weekend.

### 2017, Day 2

Used a real parser today! Nice excuse to use Control.Fold too. I wish I'd
figured out how to solve this one in O(n) time. :-/

### 2017, Day 1

Fun. An easy start. I wish the spec had been clearer though.

### Warm Up - 2016, Day 1

I need to get familiar with PureScript parsing libraries. Arguably
using a real parser is overkill for the task, but why am I doing this
if not to learn? :-D
