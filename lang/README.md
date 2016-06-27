# "How to definition?"

This directory contains pre-defined language definitions.  You can activate them
by copying them to `~/.config/wsedit/` (the folder may not exist yet).
Alternatively, you can create `~/.config/wsedit` as a symlink to the `lang`
directory of this repository (using `ln -s`), so your definitions will
always be up to date, but you will lose the ability to easily modify the
definitions for yourself.

# "Language x is missing!"

Great!  You've found an easy way to help us out!  I promise it won't take longer
than 30 minutes.

1. Take a look around the existing definitions and look at how they are
   structured.

2. Fork the repository on [github](https://github.com/SirBoonami/wsedit), clone
   your fork to your local machine.

3. Start with `template.wsconf`.

4. Create the header:
   * Try to draw some kind of ASCII-Art to represent the file type.
     * If the standard has a logo, take that.
     * If not, think of something (e.g. "C#" for C#).
     * The only allowed character is '#', everything else would be too easy.
     * The resulting image should be 5 characters high and max. 20 characters
       wide.
     * If you think your drawing sucks, take a look at `cabal.wsconf`.  Hardly
       anybody is an ASCII artist here.
   * Place your masterpiece it at both sides of the header, two columns away from
     the border.
   * Put a name in line 5, starting at column 30.

5. **ONLY SET INDENTATION / TAB REPLACEMENT SETTINGS IF THE OFFICIAL LANGUAGE
   DEFINITIONS EXPLICITLY STATE IT** or if you want to spark a crusade (don't do
   that).

6. Declare all syntax symbols:
   * Use one line per definition.
   * __Strings__: `-fs+ab`, where `a` is the character marking the beginning of
     a string, and `b` the one marking the end.
   * __Escape characters__: `-fe+c`, where `c` is the character used to mask
     string delimiters inside a string.  Don't use this if the language uses
     doubled delimiters instead of escape characters (e.g. in Basic, """" is a
     string containing a single quote).
   * __Line comments__: `-flc+x` where `x` is the string used to start a
     comment.
   * __Block comments__: are not supported yet.

7. Find a list of all language keywords and list them down with `-fk+keyword`.
   Again, use one line per definition.

8. Make sure every non-comment line is correctly prefixed with `<ext>:`, where
   `<ext>` stands for the file extension.

9. Test it!

10. Copy your new file to the `lang` subdirectory, commit and push it (make sure
    not to change anything else) and create a pull request on github.

11. Enjoy getting showered with praises =)
