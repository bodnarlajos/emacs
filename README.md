# emacs

It's my emacs configuration where I tried to focus to the fast init.

## use those only you must need
Start minimal set of package
An entry point will indicate a loading for a module

#### Entry points
- filetype: auto-mode-alist
- C-l as hydra menu
- C-b as counsel-buffers
- C-o as counsel-file
- default module as (undo-tree, recentf, rg, ace-window, keys, layouts, default configs)

#### Start
You need to start emacs with this script or something similar one :)

<code>
#!/bin/sh

emacs -Q -l .emacs.d/init-fast.el
</code>

#### Performance
- win10: <1sec
- linux: <0.74sec
