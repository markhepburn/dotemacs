README
======

This is my emacs configuration, placed on github for portability and
general sharing.  It has accumulated a decent amount of cruft over the
years, so please feel free to point out where it can be improved.

Post-Installation
-----------------

There's a couple of things you need to do after checking this out
(where-ever that is; ~/.emacs.d or whatever works for you.  I'll use
`checkoutdir` below):

* Symlink ~/.emacs to init.el (`ln -s checkoutdir/init.el ~/.emacs`).
* Pull in the git sub-modules as well (`cd checkoutdir; git submodule init; git submodule update`).
* Byte-compile js2.el and mic-paren.el if you're using these.
* compile cedet as per instructions; just using `make` should work (else you'll need to disable it somehow, such as by commenting out the load of `custom-c.el`)

General Notes
-------------

The top-level file is init.el; it basically just adds its parent
directory (which should be where you checked this out) to the
load-path, and loads a bunch of mode- and task-specific files.  These
are by convention named custom-<task>.el, mainly so they will be
grouped together in a directory listing.  The exception is the
allowance for platform-specific initialisation, which is loaded by
looking for a file with the same name as the symbol-value of
`system-type`.

For efficiency, I have borrowed a few hints from [Jacob Gabrielson's
post on effective
emacs](http://a-nickels-worth.blogspot.com/2007/11/effective-emacs.html),
in particular setting things up for autoload where possible, and as a
result making heavy use in places of `eval-after-load`.  This might
make it a bit harder to read in places.

There's a bit of defensive coding; in place of `(require 'foo)`
followed by configuration, I try to use `(when (require 'foo nil t)
...)` instead.  The last argument to `require` means to simply return
`nil` but not signal an error in the event that a feature cannot be
loaded.  Yes, this could and possibly should be wrapped as a macro.
