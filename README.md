## kal - calorie-counting unix filter

The idea is to have a file with your daily information in this format, with an
editor that has a hotkey bound to run the file through this program. In this
way you get, in vim, or any editor, the feeling of an Emacs mode dedicated to
calorie tracking. You can start the day by typing

```
d now
t 0 / 2000 (0) = 0%
k 0% 170 3 16 20 chobani "some yogurt drink"
```

And then have kal fill in the details and update them as you add new meals
throughout the day:

```
d Saturday, 18 Nov 2023
t 170 / 2000 (-1830) = 8%
T 3:16:20 = 7:41:51%
k 100%  170   3  16  20 chobani "some yogurt drink"
```

You are not really expected to use this. You are expected to, if you like the
idea, fork it and modify it to track your diet in a way that's useful to you.

## Build

```
$ dune build --profile=release
$ dune install
$ dune install --display=verbose    # if you still don't have it in PATH
$ dune install --prefix=/usr/local  # to install it somewhere else
```

## The versions

These were organically written as I tried different programs. Go has the most
mark on the format as fmt.Sscanf() needed double-quoted strings. nelua-1 is the
very first program I wrote in Nelua, nelua-2 is more recently written and
reflects a greater understanding of the language (using Lua-style string
matching, .neluacfg.lua instead of Makefile, etc.)

The version on Github used to be in Erlang, and is now in OCaml.
