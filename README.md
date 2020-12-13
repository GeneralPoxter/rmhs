# rmhs

rmhs is a safe rm command written in Haskell

## Installation
Clone the repo, install [stack](https://github.com/commercialhaskell/stack), and run `stack install` (make sure your local bin is in PATH)

## Instructions
### Command usage
```
Usage: rmhs [-V|--version] [-v|--verbose] [-p|--protect] [-u|--unprotect] 
            [-k|--keep] [-f|--force] [-i|--interactive] [FILES/DIRECTORIES...]
  Removes files and directories with protection safeguards. Run with '-h' for
  more info.

Available options:
  -V,--version             Outputs version and exits
  -v,--verbose             Print out files that are being processed
  -p,--protect             Protects specified files
  -u,--unprotect           Removes protection for specified files
  -k,--keep                Keeps protection for files that are removed
                           (overriden if parent directory is removed)
  -f,--force               Removes all specified files, ignoring protection
  -i,--interactive         Prompts before removing protected files
  -h,--help                Show this help text
```
### Manual protection
* Protect the directory `[path to directory]` by creating `[path to directory]/.hsield`  
(note that this will only protect the directory from removal, but not necessarily its contents from individual removal)  
* Protect the file `[filename + extension]` by creating `.hsield-[filename + extension]` in the same parent directory

## Development
[GeneralPoxter](https://github.com/GeneralPoxter) and [Aplet123](https://aplet.me/)
