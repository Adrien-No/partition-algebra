# Description 

The most important parts are located in `Lib/Labelled.ml` and `Lib/Unlabelled.ml`, specifically in the `concat` functions of their respective functor `Make`. These functions handle the concatenation of two diagrams (labelled or unlabelled). Another key function can be found in `Lib/generate_semigroup.ml`, which naively constructs a finite, relatively small semigroup. The rest is mostly technical details.

# Usage
File bin/main.ml is used is where we write our "script" that use the libraries, to print semigroup size or draw diagrams. (A lot of commented examples in this file are usable).

we can "add to draw" a diagram using `Lib.(ε|Un)labelled.print` 

In root, do 
``` bash
make mbin 
```
to execute everything.

And do 
``` bash
make show
```
to print diagrams all concatenated. 

Make sure to include `draw_diagram()` at the end of your script in `bin/main.ml`.

# warnings
- few objects are incorrectly named (e.g "Okada" for labelled Partition, concatenation for a composition)

- ~~Generators E (used for Temperley-Lieb and Okada) seems to be wrongly implemented~~ fixed :)


# dependancies 

```bash
opam install unionFind
```
