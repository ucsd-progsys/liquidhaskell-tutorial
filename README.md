# LiquidHaskell Tutorial

**TODO: UPDATE the website with the new code**

**NOTE** The PDF/HTML are sometimes not up-to-date
with the latest LiquidHaskell release. Please clone
the github repository and run locally for best results.

## Contents

#### Part I: Refinement Types

1. [Introduction](src/01-intro.lhs)

2. [Logic](src/02-logic.lhs)

3. [Refinement Types](src/03-basic.lhs)

4. [Polymorphism & Higher Order Functions](src/04-poly.lhs)

5. [Refining Data Types](src/05-datatypes.lhs)

#### Part II: Measures

6. [Propositions](src/06-measure-bool.lhs)

7. [Numbers](src/07-measure-int.lhs)

8. [Sets](src/08-measure-sets.lhs)


#### Part III : Case Studies

9. [Case Study: Lazy Queue](src/09-case-study-lazy-queues.lhs)

10. [Case Study: Associative Maps](src/10-case-study-associative-maps.lhs)

11. [Case Study: Pointers without Overflows](src/11-case-study-pointers.lhs)

12. [Case Study: AVL Trees](src/12-case-study-AVL.lhs)

## Get Started

```
$ git clone --recursive https://github.com/ucsd-progsys/liquidhaskell-tutorial.git
$ stack install
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

# Building

## Deploy on Github

### Dependencies

```
$ stack install pandoc
```

#### Prerequisites

~~~~~
$ cd .. && git clone https://github.com/ucsd-progsys/liquid-client.git
~~~~~

#### Actual deployment

~~~~~
$ git checkout master
$ make html
$ cp -r _site ~/tmp/
$ git checkout gh-pages
$ cp -r ~/tmp/* .
$ git commit -a
$ git push origin gh-pages
~~~~~

## Compiling .pdf

### Dependencies

```
$ stack install pandoc pandoc-citeproc
```

#### Prerequisites

* Install LaTeX dependencies:
  * [Texlive](https://www.tug.org/texlive/)
  * texlive-latex-extra
  * texlive-fonts-extra

**Ubuntu 17.10**

```
$ sudo apt install -y texlive-latex-base texlive-latex-extra texlive-fonts-extra
```

#### Producing .pdf Book

~~~~~
$ make pdf
$ evince dist/pbook.pdf
~~~~~

## Solutions to Exercises

Solutions are in *separate* [private repo](https://github.com/ucsd-progsys/liquidhaskell-tutorial-solutions)

## TODO

A work list of TODO items can be found in the [bug tracker](https://github.com/ucsd-progsys/liquidhaskell-tutorial/issues/19)

## Feedback and Gotchas

Editing feedback and various gotchas can be found in [feedback.md](feedback.md)
