## Utils: Some tiny utilities to make life a little easier


### List of utilities

- rename: Rename files using your favorite text editor
- hr: Print a horizontal rule in the terminal


### How to Install

First, make sure you have [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
installed, then run these commands in your shell:

    git clone https://github.com/qwfy/utils.git
    cd utils
    stack install
    echo "binaries installed to $(stack path --local-bin)"


### rename: Rename files using your favorite text editor

    $ rename

will fire up your `$EDITOR` and put file names in the current directory in it,
like this:

    file name 1
    file name 2

    ------ Above is the original file names, put new file names below ------

    file name 1
    file name 2

then you can edit lines below the marker:

    file name 1
    file name 2

    ------ Above is the original file names, put new file names below ------

    new file name 1
    new file name 2

after editing, save and quit your editor, `file name 1` will be renamed to
`new file name 1`, etc..

Another example:

    # rename all txt files in the current directory
    $ ls *.txt | rename --stdin

See `rename --help` for more options.


### hr: Print a horizontal rule in the terminal

    $ hr
    ▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
    2016-05-08 10:30:56.091685 UTC
    ▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

Or, if it's really necessary:

    $ hr some memo
    ▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
    2016-05-08 10:30:56.091685 UTC
    ▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

Both the name and idea are stolen from https://github.com/LuRsT/hr
