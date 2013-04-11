#!/bin/bash

# get mfile version number
VERNUM=`head -n 1 VERSION`

# get release description
echo "Changelog entry for this update (ALL SINGLE-QUOTES MUST BE DOUBLED UP!!!!):"
read CHGLOGENTRY

# update Changelog
CMD="sed -e \"1 i `date +%Y-%m-%d` Version $VERNUM $CHGLOGENTRY\" -e \"s/'/''/g\" Changelog >Changelog.new"
eval $CMD
mv Changelog.new Changelog

