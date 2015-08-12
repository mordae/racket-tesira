#lang scribble/manual

@require[(for-label tesira/util)
         (for-label typed/racket/base)]

@title{Utilities}

@defmodule[tesira/util]

Set of convenience function to simplify some typical tasks.


@defproc[(tesira-get (a-tesira Tesira)
                     (alias Symbol)
                     (attr Symbol)
                     (arg TExpr) ...)
         TExpr]{
  Retrieve a single value.
}

@defproc[(tesira-set (a-tesira Tesira)
                     (alias Symbol)
                     (attr Symbol)
                     (arg TExpr) ...)
         Void]{
  Modify a single value.
}


@; vim:set ft=scribble sw=2 ts=2 et:
