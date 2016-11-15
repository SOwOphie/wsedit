# "How to definition?"

This directory contains pre-defined language definitions.  You can activate them
by copying them to `~/.config/wsedit/` (the folder may not exist yet).
Alternatively, you can create `~/.config/wsedit` as a symlink to the `lang`
directory of this repository (using `ln -s`), so your definitions will
always be up to date, but you will lose the ability to easily modify the
definitions for yourself.

# "Language x is missing!"

Great! You've found an easy way to help us out! I promise it won't take longer
than 30 minutes.

1. Take a look around the existing definitions and look at how they are
   structured.

2. Fork the repository on [github](https://github.com/SirBoonami/wsedit), clone
   your fork to your local machine.

3. Start with `template-lang.wsconf`.

4. Create the header:
   * Try to draw some kind of ASCII-Art to represent the file type.
     * If the standard has a logo, take that.
     * If not, think of something (e.g. "C#" for C#).
     * The convention is to use only the '#' character for drawing.
     * The resulting image should be 5 characters high and max. 20 characters
       wide.
   * Place your masterpiece at both sides of the header, two columns away from
     the border.
   * Put a name in line 5, starting at column 30.
   * Put your name and mail into line 9.

5. **Only set indentation / tab replacement settings if the official language
   definitions explicitly state/encourage it** or if you want to spark a crusade
   (don't do that).

6. Declare all syntax symbols as described in `wsedit -h`, __Language__ section.

7. Test it!

8. Copy your new file to the `lang` subdirectory, commit and push it (make sure
   not to change anything else) and create a pull request on github.

9. Enjoy getting showered with praises =)
