# LiquidHaskell Tutorial

**TODO: UPDATE the website with the new code**

**NOTE** The PDF/HTML are sometimes not up-to-date
with the latest LiquidHaskell release. Please clone
the github repository and run locally for best results.

## How to _Do_ The Tutorial

LH is available as a GHC plugin from version 0.8.10.

Thus, **the best way** to do this tutorial is to 

**Step 1** Clone this repository,

```bash
$ git clone --recursive https://github.com/ucsd-progsys/liquidhaskell-tutorial.git
```

**Step 2:** Iteratively edit-compile until it _builds_ without any liquid type errors

```bash
$ cabal v2-build
```

or 

```
$ stack build --fast --file-watch
```

The above workflow will let you use whatever Haskell tooling you use for your 
favorite editor, to automatically display LH errors as well.

## Contents

### Part I: Refinement Types

1. [Introduction](src/Tutorial_01_Introduction.lhs)
2. [Logic & SMT](src/Tutorial_02_Logic.lhs)
3. [Refinement Types](src/Tutorial_03_Basic.lhs)
4. [Polymorphism](src/Tutorial_04_Polymorphism.lhs)
5. [Refined Datatypes](src/Tutorial_05_Datatypes.lhs)

### Part II: Measures

6. [Boolean Measures](src/Tutorial_06_Measure_Bool.lhs)
7. [Numeric Measures](src/Tutorial_07_Measure_Int.lhs)
8. [Set Measures](src/Tutorial_08_Measure_Sets.lhs)

### Part III : Case Studies

9. [Case Study: Okasaki's Lazy Queues](src/Tutorial_09_Case_Study_Lazy_Queues.lhs)
10. [Case Study: Associative Maps](src/Tutorial_10_Case_Study_Associative_Maps.lhs)
11. [Case Study: Pointers & Bytes](src/Tutorial_11_Case_Study_Pointers.lhs)
12. [Case Study: AVL Trees](src/Tutorial_12_Case_Study_AVL.lhs)

### Update

```bash
$ git pull origin master
$ git submodule update --recursive
```

## Building

### Deploy on Github

**Step 1** 
 
Do we really need to install `pandoc-citeproc`?

```
$ mv package.yaml.pandoc package.yaml
$ stack install pandoc template
$ make html
$ make pdf
$ make upload 
```


##### Prerequisites

```bash
$ cd ../ && git clone https://github.com/ucsd-progsys/liquid-client.git
```


#### Dependencies

```bash
$ stack install pandoc pandoc-citeproc template
```

#### Prerequisites

* Install LaTeX dependencies:
  * [Texlive](https://www.tug.org/texlive/)
  * texlive-latex-extra
  * texlive-fonts-extra

#### Producing .pdf Book

```bash
$ make pdf
$ evince dist/pbook.pdf
```

## Solutions to Exercises

Solutions are in *separate* [private repo](https://github.com/ucsd-progsys/liquidhaskell-tutorial-solutions)

## TODO

A work list of TODO items can be found in the [bug tracker](https://github.com/ucsd-progsys/liquidhaskell-tutorial/issues/19)

## Feedback and Gotchas

Editing feedback and various gotchas can be found in [feedback.md](feedback.md)
