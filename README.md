# My stow folder
## Updating submodules
Each submodule is set to track the HEAD of the master branch.<br/>
How this is done:<br/>
https://stackoverflow.com/questions/10443627/force-git-submodules-to-always-stay-current/31851819#31851819<br/>
To update the submodules, run:<br/>
``$ git submodule update --remote``

## Cloning with submodules
To include the submodules when cloning, run:<br/>
``$ git clone --recurse-submodules -j8 git@gitlab.com:Basspoon/stow.git``
