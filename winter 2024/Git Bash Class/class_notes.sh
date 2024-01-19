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


# / / / / CONFIGURING GIT / / / / 

>> git config --list 
# this gives a list of customizable settings

>> git config --local
# setting for one specific project

>> git config --global 
# settings for all of our projects

>> git config --system 
# settings for every users on this computer


+ LOCAL
#takes precedences over
+ GLOBAL
#takes precedences over 
+ SYSTEM

>> git config --global [[setting]] [[value]]

>> git config --global user.email johnsmith@datacamp.com
# changes email address to johnsmith@datacamp.com

>> git config --global user.name 'John Smith'
# changes username to John Smith

# / / / /  Using an Alias / / / /

>> git config --global alias.ci 'commit -m'
# creates and alias for commting files by executing >>> ci 

>> git commit -m BECOMES >> git ci

>> git config --global --list 
# this will display all the aliases we have created

>> git status 
# checks the state of the files globally


# / / / / BRANCHES / / / /

branches are like the parrallel universes of the files

SOURCE branch
# this is the branch we merge from

DESTINATION branch 
# this is the branch we merge into

>> git branch
# this will display the branches available. An asterisk will indicate which branch we are currently in

>> git checkout -b report
# this will create a new branch called "report"

>> git checkout report
# this will switch us onto the branch named "report"

>> git diff branch1 branch2
# this compares the two branches in the repo

>> git merge branch1 branch2
# this will merge branch1 into branch2

## When merging, note that if there are conflicts in the files, it will fail to merge. follow Git's advice to resolve the conflicts


"<<<<<<< HEAD" 
# the file's contents in the latest commit of the current branch

"========"
# contains the center of the conclict

">>>>>>> update"
# additional content that would be in the update branch

# / / / / CREATING REPOS / / / /

>>> git init reponame
# this creates a repository named 'reponame'
# it will appear as a file in our directory

>> git status
# this lets us know hwat is beaing tracked

"
WARNING: NEVER CREATE A GIT REPO INSIDE ANOTHER GIT REPO! unless you are working on very complex repositories.
"

>> pwd
# this will show the current working directory

# / / / / REMOTES / / / /

>>> git clone path/to/repo

use online services like
 + github
 + bitbucket
 + gitlab
to store remote repos

>>> git remote
# this confirms if we have cloned a project remotely

>>> git remote -v 
# this will display more information regarding 

>>> git remote add george https://github.com/george_datacamp/repo

>>> git fetch origin main
# fetches versions from the remote

>>> git fetch origin report

>>> git merge origin main
# gathers contents from the remote origin repo into your local main branch.

>>> git diff origin main
# you can compare the contents of the content in the remote to the content in the main branch


# / / / / PUSH AND PULL / / / /

>>> git pull 
# this is two commands in one: fetch and merge:
# this command syncs any changes in the remote repo

>>> git pull origin main
# pulls from the remote origin branch to the main local repo branch

>>> git push origin main
# pushes from local main branch to remote origin branch

