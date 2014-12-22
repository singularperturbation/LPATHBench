**ARM vs x86: Pathfinding benchmark of C++, D, Go, Nim, Ocaml, Common Lisp, Racket, C#, Java, Haskell, F#, Rust and Dart**

In this benchmark I thought it would be interesting to explore a less common pathfinding algorithm. Imagine you've just been "invited" to visit your in-laws (if you don't have inlaws, imagine you're driving a married friend), and want to find the longest possible route to the in-laws' house, in order to minimise the time spent with them. Now, you don't want your spouse (or your friend's spouse) to know you're stalling, so you can't visit the same place twice, can't just spend infinity hours driving in a circle. How would you find this longest path?

One way is to create a graph with the nodes representing different intersections and the connections between these nodes representing the distances of the roads between these intersections. One can then solve it with the relatively simple approach of iterating through all possible routes to find the longest. You may be thinking this sounds incredibly slow, and it is, something like O(n!). Unfortunately however there are no known "fast" algorithms that can find this path; the problem has been proven to be NP hard, meaning if you can solve it in O(n^b) time, where b is a constant, then you get a [million dollar prize](http://en.wikipedia.org/wiki/Millennium_Prize_Problems#P_versus_NP).

The following table contains the most useful results for the benchmark, comparing each language against itself on ARMv7 and x86-64. 32 bit integers are used where possible, rather than machine-sized ones, to ensure both implementations use the same sized datatypes. OCaml is an exception, as non-machine-sized ints are boxed, so machine-sized ints are faster.

Note that the algorithm finds the length of the longest path in the graph that starts at node zero, but doesn't actually find all the steps in that path; this is purely laziness on my part, as the latter requires more effort to write. In my defence, most results for "longest path algorithm" on Google also seem to only give the length of the longest path, not the path itself.

**Note**: *The x86 laptop died while running it, so the current results are on a new machine: a quad core x86-64 Pentium n3530 at 2.16 ghz.*

Just to clarify: I'm not comparing ARM and x86, I'm comparing language implementations on two common ARM and x86 platforms.

**Results:**

| Language | % x86 |
| :------- | ----: |
| C++Cached | 115.652 |
| C++/gcc | 74.4626 |
| GCCGo | 72.0985 |
| CSharp | 70.6358 |
| LuaJit | 70.1124 |
| C++/clang | 64.1457 |
| Racket | 58.8287 |
| Nim | 55.3526 |
| Go | 54.8455 |
| FSharp | 50.9221 |
| OracleJava | 50.2763 |
| Ocaml | 38.673 |
| Lisp | 32.9466 |
| Java | 30.5991 |
| D | 7.94147 |


The % x86 column refers to the speed of a language on ARM as a percentage of its speed on x86. So if an implementation's % x86 is 50%, then it runs at half the speed on ARM as it does on x86.

F#, Haskell, Rust and Dart send their apologies. F# didn't have an Arch Linux package for ARM, and when I built it myself the compiler and runtime segfaulted upon use. Haskell requires LLVM for codegen on ARM, but it doesn't package it, and doesn't properly support the version of LLVM I'm using, so couldn't compile the vector library (note that this problem may go away in future with plans to package a specific LLVM version with GHC). The Rust package on ARM is only 0.11, and I was too lazy to backport my 0.12 implementation (0.12 is so much prettier; vec[i] instead of vec.get(i), for instance). Dart seemed to have an incredibly convoluted build processes, as it requires the Chromium dependencies, and I ain't got time for that. Their x86 performance results can be seen further below.

An aside: D only barely works. While sudo pacman -S ldc went down without a hitch, when I compiled and ran it the output was garbage unicode characters. Replacing D's writeln with standard C printf fixed this.

**Edit:** Okay, I updated the ldc D compiler earlier today (incidentally, as part of upgrading my system with pacman -Syu), and now it doesn't compile at all. It was previously compiling, and ran at around 90% the speed of C++ on ARM.

**Edit again:** I found a working D compiler, the GCC D compiler, but the executable it produces is really slow.

The OpenJDK's performance on ARM shows how much performance depends on the implementation, not just the language. If you're a low-level person and looking for something useful to which to contribute, consider implementing a JIT compiler for OpenJDK on ARM. Even if it was only half as good as the Oracle one, you'd still be able to put on your resume that you made the ARM JVM five times faster.

**Edit**: *Okay, someone changed the Java implementation to use arrays for the data, instead of classes, effectively unboxing it, and now the OpenJDK implementation is no longer an order of magnitude slower than the Oracle JVM on ARM. Interesting.*

Weird thing: when I made a symlink 'oraclejava' which linked to my local Oracle jvm `java` executable, for some reason it created an 'oraclejava' in /sbin, which pointed to the Openjdk java executable instead! Hence why the benchmark explicitly runs `/usr/bin/oraclejava`.

Now, the seedy part of the benchmark: comparing different languages with each other. They ain't even using the exact same algorithms :O Surprisingly, I found the more functional approach to be faster than the pure imperative approach for Haskell and OCaml, possibly because the functional approach minimises friction with their expensive write barriers.

The imperative approach:

```haskell
getLongestPathImperative :: V.Vector Node -> Int32 -> UMV.IOVector Bool -> IO (Int32)
getLongestPathImperative !nodes !nodeID !visited = do
  UMV.write visited (fromIntegral nodeID) True
  max <- newIORef 0
  Prelude.mapM_  (\ Route{dest, cost} -> do
             isVisited <- UMV.read visited (fromIntegral dest)
             case isVisited of
               True -> return ()
               False -> do
                   dist <- fmap (+ cost) $ getLongestPath nodes dest visited
                   maxVal <- readIORef max
                   if dist > maxVal then writeIORef max dist else return ())
         (nodes V.! (fromIntegral nodeID))
  UMV.write visited (fromIntegral nodeID) False
  readIORef max
```

The more functional approach:

```haskell
getLongestPath4 :: V.Vector Node2 -> Int32 -> UMV.IOVector Bool -> IO (Int32)
getLongestPath4 !nodes !nodeID !visited = do
  UMV.write visited (fromIntegral nodeID) True
  max <- GV.foldM' acc (0::Int32) (nodes V.! (fromIntegral nodeID))
  UMV.write visited (fromIntegral nodeID) False
  return max
    where
      acc :: Int32 -> Route -> IO (Int32)
      acc maxDist Route{dest,cost}  = do
          isVisited <- UMV.read visited (fromIntegral dest)
          case isVisited of
            True -> return maxDist
            False -> do
              dist <- fmap (+ cost) $ getLongestPath4 nodes dest visited
              return $ if dist > maxDist then dist else maxDist
```

Essentially, the former has max as a single mutable variable accessed and modified when iterating over the node's neighbours, while the latter has max folded along the iteration, creating a new max each iteration.

Note though that the latter is still pretty imperative, in the sense that it uses an unboxed mutable vector of bools. I attempted to write a purely functional version, creating a new, modified array of bools every iteration, but this was unfathomably slow and memory-hungry.

Anyway, here's the numbers you probably came here for. The x86-64 device is an Intel dual core i5 M430 2.27GHz laptop, running the latest Arch Linux, and the ARMv7 device is a Galaxy S3 with 2GB of ram and a quad-core 1.3ghz processor, runing the latest Arch Linux in a chroot.

**Note:** *The jscached and c++cached versions here use a slightly different algorithm, so it's not fair to directly compare them to other implementations, but I include them here to show how important algorithm choice is.*

**ARMv7**

| Language | Runtime (ms) |
| :------- | -----------: |
| C++Cached | 115 |
| C++/gcc | 2326 |
| C++/clang | 2499 |
| Nim | 3176 |
| GCCGo | 4670 |
| OracleJava | 5247 |
| CSharp | 5442 |
| Ocaml | 5712 |
| Go | 6181 |
| Lisp | 6896 |
| FSharp | 6995 |
| Java | 8363 |
| LuaJit | 10322 |
| Racket | 11440 |


**x86-64**

| Language | Runtime (ms) |
| :------- | -----------: |
| C++Cached | 112 |
| JavascriptWithCacheAlg | 173 |
| C++/clang | 1618 |
| Nim | 1866 |
| C++/gcc | 1894 |
| Rust | 2262 |
| D | 2299 |
| RustUnsafe | 2312 |
| Ocaml | 2378 |
| Lisp | 2410 |
| OracleJava | 2658 |
| Java | 2725 |
| Julia | 3088 |
| CRYSTAL | 3436 |
| Go | 3700 |
| FSharp | 3749 |
| CSharp | 3875 |
| GCCGo | 4032 |
| Dart | 5635 |
| Javascript | 6463 |
| Racket | 6947 |
| LuaJit | 7310 |
| Haskell | 8298 |


Feel free to submit improvements to the implementations! Just one rule: the graph must be read in at runtime; reading it in and generating the result at compile-time is not allowed.


**Discussion**

*Lua*

Damn is it fast; I wouldn't have thought a dynamic language would run almost faster than Haskell and faster than F#. Mike Pall: the Einstein of just-in-time-compiler-writing.

*Go*

Not really much to say here. The implementation was pretty simple to write, and achieved reasonable performance without any effort put into optimising the code. One thing to note is that it was the only language apart from Rust that required me to explicitly handle the failure case of atoi when parsing in the routes, albeit in a somewhat ugly manner:

```go
    node, err1 := strconv.Atoi(nums[0])
    neighbour, err2 := strconv.Atoi(nums[1])
    cost, err3 := strconv.Atoi(nums[2])
    if err1 != nil || err2 != nil || err3 != nil{
      panic("Error: encountered a line that wasn't three integers")
    }
```

It actually enforced it less strongly than Rust, as technically I could have just ignored the errors returned by assigning them to _, but doing so would generally be considered an abomination in good Go code.

*Rust*

**NOTE:** *The code I had trouble with is fixed in the current Rust nightly; `visited[2] = true` works fine. So consider this rust section outdated.*

~~This was by far the hardest implementation to write, due to the language having changed significantly sincelast I used it. Relatively simple tasks eluded me, for instance:~~

```rust
let mut visited: Vec<bool> = Vec::from_fn(10, |_| false);
visited[2] = true;
```

~~This looks like it should create a mutable vector of 10 bools set to false, and then set the one at index 2 to true. Does it compile? No, it prints:~~

```rust
error: cannot assign to immutable dereference (dereference is implicit, due to indexing)
```

~~Okay, maybe I need to make a mutable dereference of visited[2]:~~

```rust
let mut thirdBool = &(visited[2]);
*thirdBool= false;
```

~~This should work, right? Nope, turns out I `cannot assign to immutable dereference of `&`-pointer `*thirdBool``. Hmm, so I need a mutable dereference, whatever that is, but I've only got a pointer. Okay, how about:~~

```rust
let mut thirdBool = &mut (visited[2]);
```

~~That looks like it should give me an mutable reference to visited[2]. Does it? No, because I `cannot borrow immutable dereference (dereference is implicit, due to indexing) as mutable`. :'( I also checked the Vec docs and the introduction to Rust, but couldn't find any mention of how to set a vector element.~~

~~Needless to say, being the lazy sod that I am, I turned to a simpler solution.~~

```rust
  unsafe{
    let newAddr: uint = (visited as uint) + (2 as uint);
    let newAddrP: *mut bool = newAddr as *mut bool;
    *newAddrP = true;
  }
```

~~Problem solvered! This led me to the somewhat frightening discovery that unsafe is not transitive: when I put the above code in the findPlaces function, that function doesn't need to be marked as unsafe and the code calling it doesn't need to be in an unsafe block. This is probably necessary, in the sense that the standard library uses unsafe code for performance and it wouldn't make sense for all stdlib calls to be marked unsafe, but I nevertheless find it surprising how easy it is. Maybe it could be useful to require a compiler flag for compiling code containing `unsafe`, like C# does.~~

~~That being said, the version I was using is the latest available in the Arch Linux repositories, 0.12, and it's quite possible that the current trunk version makes mutating an element in a vector much easier.~~

I found it slightly disappointing that Rust has chosen to forbid top-level function type inference, of the kind possible in F#, OCaml and Haskell. The argument is that this prevents the interface-breaking that could occur if a change within an exported function altered its signature, but this wouldn't be a problem with a proper module system like OCaml. With Ocaml, the module interface must be specified explicitly, and if function signatures within the module don't match this then there's a compilation error. Imagine a function that takes a channel of mutable references to atomically reference counted options of ints: in Rust I'd have to write something like:

```rust
Chan<&mut Vec<RC<RefCell<Option<int>>>>
```

in the function type signature, which is rather gangly, especially if the signature also needed to include some lifetimes. In OCaml (or Haskell), in contrast, I could just write the function, press a button to have Emacs automatically generate the signature, then copy the signature to the module file (or place it above the function definition, in Haskell's case). An ML style module system would also bring Rust higher kinded types for free (OCaml's higher kinded polymorphism functionality relies on its module system). Even Haskell is now moving towards a ML module system, via Backpack.

*C++*

The C++ implementation was pleasant to write; I think people give modern C++ far too little credit. If you're wondering why it appeared slightly slower than the D on x86, I suspect it's because C++ stores standard vectors of bools as bit vectors, for legacy reasons, which are generally less efficient to read and write than proper bool vectors. It could also be because of some optimisations made possible in the D compiler by the immutability and purity guarantees used in the getLongestPath function.

Also, input streams are hilarious. I wonder people unfamiliar with C++ would think of the following while loop:

**Edit:** someone updated it to use proper fixed-size bitsets, and wow is it fast!

```c
  while (text >> nodeS >> neighbourS >> costS){
    nodes[nodeS].neighbours.push_back(route{neighbourS, costS});
  }
```

*D*

Like Go, writing the D implementation was pretty straightforward. I did however make one mistake that manifested in a rather hilariously unrelated error: I declared the array of nodes with

```d
node[] nodes =  uninitializedArray!(node[])(numNodes);
```

Then, when I attempted to append a new neighbour to one of the nodes on the list, the program failed with an out of memory exception. Why? Turned out, the unitialised array of nodes wasn't zeroed, so the length and capacity parts of each node's neighbour array were presumably full of gibberish, causing the append function to attempt to allocate an absurd amount of memory to append to them. Changing `unitializedArray!` to `minimallyInitializedArray!` fixed this.

*C Sharp*
I learned something useful: iterators are not cost-free. Someone changed the iterator in the inner loop to a for loop, and the performance jumped by 30%!

And wow, the use of unsafe and stuff like `[System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]` really sped it up. Faster than Java on ARM and almost faster than Go and OCaml on x86!

*Racket*

The Racket implementation was the first Lisp implementation I wrote, as the use of Typed Racket made it easier to write, with the type checker catching most of the many errors I made before runtime. I only found one instance of the type system slowing me down: trying to convert a string to an integer. The code I wound up with was:

```racket
(: str->int (String -> Integer))
(define (str->int str) 
  (define n (string->number str))
  (if n (numerator (inexact->exact (real-part n))) 0))
```

First, it calls string->number, which returns a (U Integer nil), a union type. The `if n` is like a form of pattern matching, converting n from type (U Integer nil) to just Integer, which seems pretty neat. The verbose and potentially unnecessary part is `(numerator (inexact->exact (real-part n)))`, which first takes the real part of n (it could be an imaginary number), then converts it to exact (it could be a float), then takes the numerator (it could be a fraction), to finally give an int. A str->int function in the stdlib that returned (U Integer nil) could be a nice alternative to this.

*Common Lisp*

This was a bit more tricky to write than the Racket implementation. Common Lisp doesn't come with a string split algorithm in its standard library, so I borrowed one I found online. It also doesn't allow recursive function definitions in let bindings, instead requiring the use of `labels`. The biggest inconvenience I found, however, was the apparent lack of the equivalent of Racket's "build vector", which builds a vector from a function. Common Lisp has `make-array`, but if that is used with a struct initial element then it just fills every element of the vector with a reference to the same struct instance, causing a modification of one element to effect the rest. I hence had to populate the vector manually:

```lisp
(dotimes (i num-nodes)
	(setf (elt nodes i) (make-node)))
```

Which admittedly isn't much code to write.

I did however find Common Lisp's type system far more useful in terms of obtaining the performance benefits of types. Unlike Typed Racket, Common Lisp uses gradual typing, so you can add type declarations for one function or variable without having to add them to others. I also found developing Common Lisp with Slime and Emacs a lot smoother than developing Racket on DrRacket, largely because SBCL compiles code far quicker.

Also worth noting that I attempted to run the Common Lisp implementation in ClozureCL and it worked fine, albeit was slower. ~~The Racket implementation, in comparison, had no chance of working on Chicken or any other Scheme, as the smaller Scheme standard library means the common api between different Schemes is smaller.~~ *Edit: it's been pointed out to me that Racket isn't actually a Scheme now, so it's not fair to expect Racket code to be portable to Schemes.*

*OCaml*

Ocaml was a delight to write as usual, thanks partially to the fantastic OCaml emacs plugins, Tuareg and Merlin. They provide error checking upon saving, like Eclipse with Java, and easy code testing and reloading via their integration with the OCaml repl. While not as neat as the Haskell implementation, the OCaml implementation was quicker to write, due to not having to deal with monadic IO, and performs impressively fast. Interestingly, I found my initial imperative version, which used an ioref, was actually slightly slower than the functional version, possibly due to the GC's write barrier.

Random aside: I've seen many people reluctant to use OCaml because of it's lack of support for shared memory parallelism. You know what else lacks shared memory parallelism? Node.js! But that hasn't stopped people building responsive web applications and services, as sufficient parallelism can generally be achieved by async io and process pools, both of which OCaml has. So if you've ever considered Node for a webapp, why not consider OCaml too? It also allows code-sharing between the frontend and backend, via the excellent js_of_ocaml.

*FSharp*

It was pretty simple to translate the OCaml implementation into F#, apart from a few minor differences like `array.(myIndex)` changing to `array.[myIndex]`. The F# was however nowhere near as fast as the OCaml. Interestingly, the F# Emacs support was even better, with fsharp-mode enabling some kind of extremely powerful intellisense, although the use of intellisense required the creation of an xml-laden myfile.fsproj file for some reason.

*Java*

Really not much to say here. Verbose, but fairly simple to write, and reasonably fast, although not comparable to the compiled languages.

And wow: someone update it to use arrays instead of classes for nodes (effectively unboxing them), and it's incredibly fast. I'm really looking forward to unboxed classes in Java 9 (or 10 :/).

*Haskell*

The Haskell implementation was generally pleasant to write; the Vector.modify function proved to be extremely convenient for building the node vector. It takes a vector-mutating function and returns either a copy of the vector with that function applied or the same vector mutated by that function, depending on whether or not it is safe to do the latter. This allows a vector to be mutated in pure code, via the ST monad, which is much quicker than having to allocate a new vector.

Interestingly, when I was attempting to modify the code to be more functional (passing max along in a fold rather than mutating it as an ioref), I realised I didn't understand do notation as well as I thought I did.

Can you spot the mistake in the following code? I didn't.

```haskell
do
  UMV.write visited nodeID True
  let max = GV.foldM' acc 0 (nodes V.! nodeID)
  UMV.write visited nodeID False
  return max
```

This lead to the program using all the memory and dying, leading me to think there was a memory leak in acc, although I checked it thoroughly and couldn't find one. Turns out, the above code is actually the equivalent of

```haskell
do
  UMV.write visited nodeID True
  UMV.write visited nodeID False
  return $ GV.foldM' acc 0 (nodes V.! nodeID)
```

To fix it, I needed to change

```haskell
let max = GV.foldM' acc 0 (nodes V.! nodeID)
```

to:

```haskell
max <- GV.foldM' acc 0 (nodes V.! nodeID)
```

One thing I found less pleasant than in OCaml was the autoindentation support. This is not the fault of the plugin itself, but rather the fact that indentation in Haskell affects meaning: whenever the semantics of code could depend on the indentation level, the autoindenter doesn't have one true indentation to select as the indentation depends on what you want the code to do. In a language without significant indentation, like a Lisp or a curly braces language, 'one true indentation' is possible.


*Dart*

Similar to Java, the Dart implementation was generally quite simple to write... apart from the Async nature of IO, which initially caught me off guard.

```dart
readPlacesAndFindPath() {
  var nodes;
  new File('agraph').readAsLines().then((List<String> lines) {
    final int numNodes = int.parse(lines[0]);
    final nodes = new List<Node>.generate(numNodes, (int index) => new Node()); 
    for(int i = 1; i < lines.length; i++){
      final nums = lines[i].split(' ');
      int node = int.parse(nums[0]);
      int neighbour = int.parse(nums[1]);      
      int cost = int.parse(nums[2]);
      nodes[node].neighbours.add(new Route(neighbour,cost));
    }
  });
  return nodes;
}
```

What does the above do? Answer: it returns null, as the `.then` is asynchronous, meaning the function will not wait for nodes to be populated before returning. A stronger type system (or a read of the dart::io documentation) would have caught this.

Although Dart performs quite well, as far as I'm aware Dart has no support for dynamic code generation/modification, unlike most other dynamic and jit-compiled languages. Personally, I'd consider dynamic code generation to be one of the most useful things about dynamic languages, in the sense it's not something that can easily be done in compiled languages, so the lack of the functionality gives me little reason to use Dart (in Common Lisp, Clojure, Erlang or even Java (via JRebel), for instance, it's possible to login to a server and dynamically update its code without need for downtime).

*Nim*

I didn't write the Nim implementation, so can't comment on it, but from looking at the code I find it pretty neat that it's almost as concise as Python yet runs quite fast.



**TLDR**

* D and Nim both work on ARM and can generate fast code, but the D stdlib is buggy (have to use C printf instead of D writeln)
* The OpenJDK's performance on ARM is a steaming pile of crap
* C++ specialises std::vector<bool> to a bitvector by default, which can hurt performance. Using a std::bitset properly is however much faster, as it can be inlined.
* Haskell can be faster than Java, thanks to unboxing
* If you wanna use Haskell on ARM, you must be willing to build whatever version of LLVM it was built with
* Luajit is friggen fast!
* Functional code in Haskell/OCaml can be faster than imperative code using iorefs.
* Iterators in C# are not cost-free like in Java, C++ and many others; the C# was sped up 30% by changing the inner loop to use a for loop instead of an iterator.


**Moral of the story**

There's no algorith for finding the longest path to one's inlaws that doesn't take exponential time, ergo the universe hates us.


**Comments:**

Make pull requests with your comments here. Or, more sensibly, use the [Reddit thread](http://www.reddit.com/r/programming/comments/2pvf68/armv7_vs_x8664_pathfinding_benchmark_of_c_d_go/)
