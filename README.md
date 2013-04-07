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

Helpful commands
----------------
1. Check if PostgreSQL server is running:

   # systemctl status postgresql.service
   # netstat -na | grep 5432
   # ps ax | grep postgres
   # pgrep -l | grep postgres

   If all of these look OK, then the server is _probably_ running.

Reading list
------------

Hackweek page:
https://github.com/SUSE/hackweek/wiki/mfile-%28Ancient,-dirt-simple-DB-app-gets-new-lease-on-life%29

Blog entry:
http://smithfarm-thebrain.blogspot.cz/2013/03/mfile-now-on-github.html
