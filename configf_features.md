## configf features to integrate into ini-file
 - [include <filepath>] template
 - [system <executable> <args>.. ]
 - #{scheme}
 - #{system}
 - #{shell}
 - #{getenv}
 - #{get}
 - #{runconfigs-get}
 - #{rget}

## new features to add to ini-file
  - [system-multiline] template
    * allows executing a file, pulls in all lines from stdout

## other nice to have things
 - config file for ini-file to use megatest configf config file syntax
