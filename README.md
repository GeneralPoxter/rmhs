# rmhs

rmhs is a safe rm command written in Haskell.

## Usage
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
  -f,--force               Removes all specified files, ignoring protection
  -i,--interactive         Prompts before removing protected files
  -h,--help                Show this help text
```