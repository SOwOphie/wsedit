# More things I'd like to do here

- Make ctrl-P respect selections
- https://github.com/LadyBoonami/wsedit/issues/30
  - change pipeThrough at Control/Global.hs ln 670
  - bold :: Buffer (Bool, String)
  - old :: String
    - basically whole buffer with \n between lines
    - need to change this to be just the higlighted text
  - readCreateProcessWithExitCode executes the shell or repl cmd the user types after ctrlp
    - sends old as standard input
    - returns (exitCode, stdout, stderr)
  ***pick up reading around ls 700 Control/Global.hs
- custom keymap support
-- including modifying how many lines/columns ctrl/alt move cursor

- learn how to parsec so I can
    - parse theme and keymap files with a pleasant syntax
    - give some hint as to the error if there is an error
