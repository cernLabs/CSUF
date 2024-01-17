# INTRO TO GIT

>> git diff
#this will reveals the differences made in the staging area

recall that
>> ls -a
#reveals to us the hidden files in the repository.

#we can reveal all the changes made in with a file or repo with this cmd
>> git diff HEAD

# / / / / / STORING DATA / / / / /

git commit has 3 parts
 + commit 
  # contains metadata
 + tree 
  # tracks names and locations in the repo
 + blob
  # Binary Large Object, contain a compressed snapshot of the file when the commit happened


>> git log
#this will give us a log of the commits made to repo in chronological order. we use space bar to move through history and 'q' to return to the terminal

hashes

hashes are unique identifiers for every commit

>> git show 789348

# will show the commit with a hash with the first 6 characters 789348

>> git show HEAD~1
# this will compare the second to the last commit with the current commit

recall that!
 + git diff: 
 	compares differences between various states of the repository (working directory, staging area, commits) and is used for analyzing changes before staging or committing.
 + git show: 
 	displays detailed information about a specific commit, including metadata and the changes introduced in that commit.
 	
>> git annotate <file>
# this will show us changes such as author, change made, time of change, and commit hash, for the specified file

# / / / / UNDOING CHANGES BEFORE COMMITING / / / / /

>> git reset HEAD <file>
# this will unstage the spec. file

>> git reset HEAD
# this will unstage all files in the staging area

YOU can UNDO CHANGES in a spec. file in repository using
>> git checkout -- <file>
# note that these changes are done forever.

>> git checkout .
# this undos all changes to every unstaged file in the current dir.

# / / / / RESTORING AND REVERTING / / / /

>> git log -3 
# this will show the last 3 commits

git log --since='Month Day Year'
# will show the commits since the spec. date
e.g. >> git log --since='Apr 2 2022'

>>git checkout dc9d8fac filename.csv
# reverts the file to version from commit with hash dc9d8fac

>> git checkout HEAD~1 filename.csv
reverts the file to the last commit made

>> git clean -n 
# see what files are beign tracked
# tracked files are files that git knows about

>> git clean -f ## THIS CANNOT BE UNDONE!!
# deletes the files that are being tracked

