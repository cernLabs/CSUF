#!/bin/bash


# the purpose of this program is to regulary push my CSUF to a personal repository on GH

# go to correct dir
cd /home/cern/Desktop/githubbahubba/CSUF

# get date
date=$(date +%Y-%m-%d)

# upload
git add .
git commit -m "regular backup for $date"
git push
