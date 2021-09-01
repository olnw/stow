## Updating submodules
Each submodule is set to track the HEAD of the master branch.
How this is done:
https://stackoverflow.com/questions/10443627/force-git-submodules-to-always-stay-current/31851819#31851819
To update the submodules, run:
``$ git submodule update --remote``

## Cloning with submodules
To include the submodules when cloning, run:
``$ git clone --recurse-submodules -j8 git@gitlab.com:Basspoon/stow.git``
