#!/bin/bash

# get mfile version number
VERNUM=`head -n 1 VERSION`

# get release description
echo "Changelog entry for this update (ALL SINGLE-QUOTES MUST BE DOUBLED UP!!!!):"
read CHGLOGENTRY

# update Changelog
declare CMD="sed '1 i `date +%Y-%m-%d` Version $VERNUM $CHGLOGENTRY' Changelog >Changelog.new"
eval $CMD
mv Changelog.new Changelog

