#!/bin/bash

# get mfile version number
VERNUM=`head -n 1 VERSION`
TMPFILE=Changelog.tmp

# get release description
echo "Changelog entry for this update:"
read CHGLOGENTRY
#echo -n $CHGLOGENTRY | sed "s/'/''/g" > $TMPFILE
#CHGLOGENTRY=`cat $TMPFILE`

# update Changelog
CMD="sed \"1 i `date +%Y-%m-%d` Version $VERNUM $CHGLOGENTRY\" Changelog >$TMPFILE"
eval $CMD
mv $TMPFILE Changelog

# commit changes to local git repo
CMD="git commit -a -m \"$CHGLOGENTRY\""
eval $CMD

