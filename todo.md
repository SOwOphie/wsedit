# Adding custom theme file

```
System.Directory.getHomeDirectory >>= (\x -> return (x ++ "/.config/wsedit/theme/default")) >>= readFile >>= (\x -> return ((unlines . filter (\y -> head y /= '#') . lines) x)) >>= (\x -> return(read x::EdDesign))
```
that big ugly line of code successfully reads a theme file (from ~/.config/wsedit/theme/default), filters out the comments, and ```read```s it as a EdDesign
theme files for default and for brighttheme are also available in ./theme/

- heavily comment the theme file to explain all options
- add new argument
  - add -dct <path/to/file> to argument options
  - update Argument dataType (src/WSEdit/Arguments/Data.hs)
  - update parser (src/WSEdit/Arguments/Parser.hs)
  - update applyArg function (src/WSEdit/Arguments.hs)
    - try to ```read``` the contents of the file
        - if successful, set as theme all good
        - if not print error to console and ask if user wants to continue with default theme
  - update help file (src/WSEdit/Help.hs)

bonus points if it actually works in the end

https://github.com/LadyBoonami/wsedit/issues/31
