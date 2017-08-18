# Recursion Scheme Examples

One day, on stack overflow, I read an answer from someone who believed that explicit recursion should be avoided, instead opting for recursion schemes like folds and unfolds. Being curious, I did some more research and stumbled upon a [this library] by Kmett and was instantly horrified by the crazy types. Luckily I also found [ a few blog postings](http://blog.sumtypeofway.com/an-introduction-to-recursion-schemes/) that acted as an introduction to these complex looking functions.

It's been a while since then, and I've only ever found myself using the basic catamorphism which is more or less a glorified fold. The reason for this, I guess, was the fact that I never had any idea of what use cases the other recursion schemes had. There is always [this chart](http://comonad.com/reader/2009/recursion-schemes/), but the short descriptions weren't very helpful.

So I decided to just try and implement some examples of each by just using type holes to figure out what functions I needed and think of use cases from there. Hopefully these examples will help anyone else looking to know more about the schemes.
