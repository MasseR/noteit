Noteit
======

Software for quickly inserting and managing notes. It should be able to keep
track of all the files and 'titles', list them sensibly and allow quick opening
and editing.

Creating notes
--------------

Creating new notes should happen with something like `noteit --add` which would
prompt the software to ask for the title, which would then be slugified. If the
user doesn't enter a title, a date will be used in it's place.

The software should create temporary contents which contains the initial
markdown for title. Maybe even something else at some point. If the user hasn't
entered a title, only the username should reflect the date, not the file
contents.

After creating the file, the software should open the users `$EDITOR`, pointed
to that file and wait until the user has finished writing.

Tracking notes
--------------

Every file should be tracked. Some sort of metafile should be created which
tracks filename, title and edit date (this would be perfect for key-value
stores :P). This metafile would then be used when listing files, and maybe for
some other uses in the future. I believe this is more scalable than just
`getDirectoryContents`.

Listing notes
-------------

Listing is simple, it should just read the metafile, take their title and zip
it with `[1..]`. The action would be something like `noteit --list`.

Editing notes
-------------

User would first list the notes and then choose which item to edit. The
selection would be done with numbers. The software would at this point open the
file in the users `$EDITOR` and wait until completion.

Searching
---------

Undefined.
