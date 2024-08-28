# Description 

The more important part is in Lib/Labelled.ml and Lib/Unlabelled.ml, more specially functions "concat" of theirs functors "Make". 
Theses functions realise the concatenation between two diagrams (labelled or unlabelled). 
An other important function is in Lib/generate_semigroup.ml that build naively a finite quite small semigroup.
The rest is just technical things.

# Usage
File bin/main.ml is used is where we write our "script" that use the libraries, to print semigroup size or draw diagrams. (A lot of commented examples in this file are usable).

we can "add to draw" a diagram with Lib.(ε|Un)labelled.print 

In root, do 
``` bash
make mbin 
```
to execute everythings.

And do 
``` bash
make show
```
to print diagrams all concatened. 

You will need to have "draw_diagram()" at the end of your script in bin/main.ml.


# warnings
- few objects are wrongly named (e.g Okada for labelled Partition, concatenation for a composition)

- ~~Generators E (used for Temperley-Lieb and Okada) seems to be wrongly implemented~~ fixed :)


# dependancies 

```bash
opam install unionFind
```
