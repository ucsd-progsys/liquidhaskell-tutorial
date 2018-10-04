# LiquidHaskell Tutorial

**TODO: UPDATE the website with the new code**

$ stack install pandoc pandoc-citeproc template

**NOTE** The PDF/HTML are sometimes not up-to-date
with the latest LiquidHaskell release. Please clone
the github repository and run locally for best results.

## Contents

### Part I: Refinement Types

1. [Introduction](src/01-intro.lhs)
2. [Logic & SMT](src/02-logic.lhs)
3. [Refinement Types](src/03-basic.lhs)
4. [Polymorphism](src/04-poly.lhs)
5. [Refined Datatypes](src/05-datatypes.lhs)

### Part II: Measures

6. [Boolean Measures](src/06-measure-bool.lhs)
7. [Numeric Measures](src/07-measure-int.lhs)
8. [Elemental Measures](src/08-measure-sets.lhs)

### Part III : Case Studies

9. [Case Study: Okasaki's Lazy Queues](src/09-case-study-lazy-queues.lhs)
10. [Case Study: Associative Maps](src/10-case-study-associative-maps.lhs)
11. [Case Study: Pointers & Bytes](src/11-case-study-pointers.lhs)
12. [Case Study: AVL Trees](src/12-case-study-AVL.lhs)

## Get Started

```bash
$ git clone --recursive https://github.com/ucsd-progsys/liquidhaskell-tutorial.git
$ cd liquidhaskell-tutorial/

$ stack install
$ export PATH=~/.local/bin:$PATH

$ stack exec -- liquid ./src/01-intro.lhs
$ stack exec -- liquid ./src/02-logic.lhs
$ stack exec -- liquid ./src/03-basic.lhs
$ stack exec -- liquid ./src/04-poly.lhs
$ stack exec -- liquid ./src/05-datatypes.lhs
$ stack exec -- liquid ./src/06-measure-bool.lhs
$ stack exec -- liquid ./src/07-measure-int.lhs
$ stack exec -- liquid ./src/08-measure-sets.lhs
$ stack exec -- liquid ./src/09-case-study-lazy-queues.lhs
$ stack exec -- liquid ./src/10-case-study-associative-maps.lhs
$ stack exec -- liquid ./src/11-case-study-pointers.lhs
$ stack exec -- liquid ./src/12-case-study-AVL.lhs
```

### Update

```bash
$ git pull origin master
$ git submodule update --recursive
```

## Building

### Deploy on Github

#### Dependencies

```bash
$ stack install pandoc
```

##### Prerequisites

```bash
$ cd ../ && git clone https://github.com/ucsd-progsys/liquid-client.git
```

##### Actual deployment

```bash
$ git checkout master
$ make html
$ cp -r _site ~/tmp/
$ git checkout gh-pages
$ cp -r ~/tmp/* .
$ git commit -a
$ git push origin gh-pages
```

### Compiling .pdf

#### Dependencies

```bash
$ stack install pandoc pandoc-citeproc template
```

#### Prerequisites

* Install LaTeX dependencies:
  * [Texlive](https://www.tug.org/texlive/)
  * texlive-latex-extra
  * texlive-fonts-extra

To install LaTeX dependencies on `Ubuntu 17.10`, following them:

```bash
$ sudo apt install -y texlive-latex-base texlive-latex-extra texlive-fonts-extra
```

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
