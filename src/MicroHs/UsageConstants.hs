module MicroHs.UsageConstants 
( usage, 
  longUsage
) where

usage :: String
usage = "Usage: mhs [-h|?] [--help] [--version] [--numeric-version] [-v] [-q] [-l] [-s] [-r] [-C[R|W]] [-XCPP] [-DDEF] [-IPATH] [-T] [-z] [-iPATH] [-oFILE] [-a[PATH]] [-L[PATH|PKG]] [-PPKG] [-Q PKG [DIR]] [-tTARGET] [-optc OPTION] [-ddump-PASS] [MODULENAME..|FILE]"

longUsage :: String
longUsage = usage ++ "\nOptions:\n" ++ details
  where
    details = "\
      \-h                 Print usage\n\
      \-?                 Print usage\n\
      \--help             Print this message\n\
      \--version          Print the version\n\
      \--numeric-version  Print the version number\n\
      \-v                 Increase verbosity (flag can be repeated)\n\
      \-q                 Decrease verbosity (flag can be repeated)\n\
      \-l                 Show every time a module is loaded\n\
      \-s                 Show compilation speed in lines/s\n\
      \-r                 Run directly\n\
      \-c                 Don not generate executable\n\
      \-CR                Read compilation cache\n\
      \-CW                Write compilation cache\n\
      \-C                 Read and write compilation cache\n\
      \-XCPP              Run cpphs on source files\n\
      \-Dxxx              Pass -Dxxx to cpphs\n\
      \-Ixxx              Pass -Ixxx to cpphs\n\
      \-T                 Generate dynamic function usage statistics\n\
      \-z                 Compress combinator code generated in the .c file\n\
      \-iPATH             Add PATH to module search path\n\
      \-oFILE             Output to FILE\n\
      \                   If FILE ends in .comb produce a combinator file\n\
      \                   If FILE ends in .c produce a C file\n\
      \                   Otherwise compile the combinators together with the runtime system to produce a regular executable\n\
      \-a                 Clear package search path\n\
      \-aPATH             Add PATH to package search path\n\
      \-LPKG              List all modules of package PKG\n\
      \-PPKG              Build package PKG\n\
      \-Q PKG [DIR]       Install package PKG\n\
      \-tTARGET           Select target\n\
      \                   Distributed targets: default, emscripten\n\
      \                   Targets can be defined in targets.conf\n\
      \-optc OPTION       Options for the C compiler\n\
      \--stdin            Use stdin in interactive system\n\
      \-ddump-PASS        Debug, print AST after PASS\n\
      \                   Possible passes: preproc, parse, derive, typecheck, desugar, toplevel, combinator, all\n\
      \"
