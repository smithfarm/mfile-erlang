#!/bin/bash
#
# run as user 'postgres'
declare MFILEDB="mymfiles1"
declare OWNER="smithfarm"
echo $MFILEDB

dropdb $MFILEDB
dropuser $OWNER
createuser -W $OWNER
createdb $MFILEDB -O $OWNER
