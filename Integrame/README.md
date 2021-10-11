# Integrame

## Statement

https://ocw.cs.pub.ro/courses/pp/21/teme/prolog-integrame

## About

This project consists of a *Prolog* application that solves **crossword puzzles**(= (rom.) integrame). The evaluated puzzles only contain questions oriented downwards or to the right.

Here, a crossword puzzle is a *set* of **cells**, each cell having a row and a column. A **cell** can be:
- **black**, which means that they cannot be filled in;
- w/ **questions**, meaning that they can contain one or two questions;

The cells that are not part of the puzzle's definition are not currently filled.

A **question** is represented by:
- a question **text**
- **direction** for the answer
- numerical **identifier**

In the program, a crossword puzzle is described by the **predicate** `integ(H, W, L, Voc)` **(\*)**, where:

- **H** = the *height* of the puzzle;
- **W** = the *width*  of the puzzle;
- **L** = *list* of cells (black or filled in);
- **Voc** = the puzzle's **vocabulary**, i.e. the list of words that can be used to fill in the crossword.

## Implementation

The aim of the program (*integrame.pl*) is to find the possible solutions for a given crossword puzzle. This is being solved by the predicate `rezolva(W, Solutie)`, where *W* is a puzzle having the form described in **(\*)**, and *Solutie* is a list of pairs **(Q, A)**:
- **Q** - the question text
- **A** - the answer

Solving the *rezolva()* predicate is done by dividing the problem into several *sub*predicates, as follows:

- *intrebari/2*, which collects the list of questions in a given puzzle;
- *id_intrebare/3*, which finds the identifier of a question
- *completare/3*, which fills the crossword with the answers to its questions

- *lungime_spatiu/3*, which finds the length of the answer to a given question
- *intersectie/5*, which determines the intersection cell of two given questions

- *solutii_posibile/2*, which finds, for each question, a list of words that are possible solutions

## Utils

*utils.pl* contains useful predicates for testing purposes.

*input.pl* contains the crossword puzzles which are being tested.

*solutie.pl* contains the solutions of the crossword puzzles from *input.pl*.

*checker.pl* tests the correctness of the program under different requirements

## Testing

The program can be automatically tested by running the following commands:

> $ swipl integrame.pl\
?- vmcheck.
