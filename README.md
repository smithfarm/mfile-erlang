mfile
=====

Ancient dirt-simple database applicaton gets a new lease on life

<b>mfile</b> is an old database application I wrote many moons ago in
C for tracking paper and electronic files. This project involves
completely rewriting the application in Erlang using the Chicago Boss
web framework so it runs in a browser (the original application ran in
a terminal using the ncurses library). The database backend
(PostgreSQL) will not change, but the data will be re-encoded from
ISO-8859-2 to UTF-8.
