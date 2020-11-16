[[License: GPL v3][https://img.shields.io/badge/License-GPL%20v3-blue.svg]]
[[https://melpa.org/#/gcmh][file:https://melpa.org/packages/gcmh-badge.svg]]
* GCMH - the Garbage Collector Magic Hack

  Enforce a sneaky Garbage Collection strategy to minimize GC interference with
  user activity.

  During normal use a high GC threshold is set.

  When idling GC is triggered and a low threshold is set.

  A more detailed explanation of the rationale behind this can be found at:

  [[http://akrl.sdf.org/]]

  - WARNING

    In case Emacs is used in a system already under severe memory
    pressure be sure to understand how GCMH works and trim
    ~gcmh-high-cons-threshold~ accordingly.  Default value may be to
    big and/or GCMH may not fit your use case.

** Usage

   Add into your .emacs

   #+BEGIN_SRC
   (add-to-list 'load-path "path-to-gcmh-here")
   (gcmh-mode 1)
   #+END_SRC

   If this is done at the beginning of your .emacs start-up time should
   also benefit form it.
