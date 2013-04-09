#!/bin/bash

# get mfile version number
VERNUM=`head -n 1 VERSION`

# get release description
echo "Changelog entry for this update:"
read CHGLOGENTRY

# update Changelog
CMD="sed '1 i `date +%Y-%m-%d` Version $VERNUM $CHGLOGENTRY' Changelog"
eval $CMD >Changelog.new
mv Changelog.new Changelog

