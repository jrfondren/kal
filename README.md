kal
=====

A calorie-counting unix filter.

There are some other variations of this program, and a sample diet file, at
https://lua.minimaltype.com/index.cgi/dir?ci=tip&name=chrestomathy/kal

You are not really expected to *use* this. You are expected to, if you like the
idea, *fork* it and modify it to track your diet in a way that's useful to you.

Build
-----

    $ rebar3 escriptize              # english
    $ rebar3 as russian escriptize   # russian

Test
-----

    $ rebar3 lint
    $ rebar3 as test dialyzer

    $ rebar3 as test shell
    1> kal:test().

Run
-----

    $ ./kal

To use practically, add kal to your path and add the following to your ~/.vimrc or ~/.config/nvim/init.vim, or do likewise with another editor:

    au BufRead,BufNewFile diet map <C-P> m8:w!<Return>:%!kal<Return>'8

And then edit ~/txt/diet normally, using C-P to reformat it on the fly:

    d now
    t 0 / 2000 (0) = 0%
    b 1900 = 0% (0)
    k 0% 160 14 6 6 fisher "mixed nuts"

becomes:

    d Sunday, 19 May 2024
    t 160 / 2000 (1840) = 8%
    b 1900 = 1188% (-1740)
    T 14:6:6 = 26 23%
    k   8%  160  14   6   6 fisher "mixed nuts"

