#!/bin/bash
git add *
git commit
git remote | xargs -L1 git push --all
#git push -u github main
#git push -u tpgc main
