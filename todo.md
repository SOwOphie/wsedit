# Adding custom theme file

- finish commenting the theme file to explain all options
- update help options

https://github.com/LadyBoonami/wsedit/issues/31


#
# detailed explanations for vty attributes can be found at
# https://hackage.haskell.org/package/vty-5.33/docs/Graphics-Vty-Attributes.html#t:Attr
#
# tl:dr below
#
# Vty attribute format:
# Attr {
#    attrSyle      :: Vty Style
#   ,attrForeColor :: Vty Color
#   ,attrBackColor :: Vty Color
#   ,attrURL       :: Text
#      }
# all fields can either be Default or SetTo (value)
#
# Vty Style:
#   style is an 8 bit binary number with the 8 bits corresponding to the 8 style options
#   +-------------------------------------------------------------------+
#   |strikethrough italic bold dim blink reverseVideo underline  standout|
#   +-------------------------------------------------------------------+
# only italic would be 0b01000000, or 0x40, or 64
# only  bold  would be 0b00100000, or 0x20, or 32
# strikethrough dim underline would be 0b10010010, or 0x92, or 146
#
# you can write the number in binary prefixed by 0b, convert it to hexadecimal
#  and prefix with 0x, or convert to decimal
#
# Vty Color format:
#   for 16 standard terminal colors:
#        ISOColor  0 for black
#      , ISOColor  1 for red
#      , ISOColor  2 for green
#      , ISOColor  3 for yellow
#      , ISOColor  4 for blue
#      , ISOColor  5 for magenta
#      , ISOColor  6 for cyan
#      , ISOColor  7 for white
#      , ISOColor  8 for brightBlack
#      , ISOColor  9 for brightRed
#      , ISOColor 10 for brightGreen
#      , ISOColor 11 for brightYellow
#      , ISOColor 12 for brightBlue
#      , ISOColor 13 for brightMagenta
#      , ISOColor 14 for brightCyan
#      , ISOColor 15 for brightWhite
#      ** note ** if you've already customized your terminal's colors, this will
#         show the colors these are mapped to, for other colors use rgbColor
#  for other colors
#      Color240 {0-239}
#      a list of rgb colors these correspond to can be found at
#      https://hackage.haskell.org/package/vty-5.33/docs/src/Graphics.Vty.Attributes.Color240.html#color240CodeToRGB
#
# Url : I'll be honest, I'm not sure what this does.  if you do then add an explanation and send a pull request
#
