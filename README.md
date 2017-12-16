## Utils: Some tiny utilities to make life a little easier

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


### rename: Rename files using your favorite editor

    $ rename dir

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

See `rename --help` or `rename dir --help` for more options.
