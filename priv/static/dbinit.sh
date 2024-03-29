#!/bin/bash
#
# initialize 'mfiledb' (mfile production database)
#
#cd ./priv/static

declare MFILEDB="mfiledb"
declare OWNER="smithfarm"
declare PGCMD="/usr/bin/sudo su postgres -c"

echo "Initializing database '$MFILEDB' with owner '$OWNER'"

$PGCMD "dropdb $MFILEDB"
#$PGCMD "dropuser $OWNER"
#$PGCMD "createuser -W $OWNER"
$PGCMD "createdb $MFILEDB -O $OWNER"
pwd
psql $MFILEDB <dbinit.sql
