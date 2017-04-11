module WSEdit.Keymaps
    ( defaultKM
    ) where


import Graphics.Vty   ( Event (EvKey)
                      , Key ( KBackTab, KBS, KChar, KDel, KDown, KEnd, KEnter
                            , KEsc, KFun, KHome, KIns, KLeft, KRight, KUp
                            )
                      , Modifier (MCtrl, MMeta, MShift)
                      )

import WSEdit.Control.Autocomplete (completeOr)
import WSEdit.Control.Base         ( fetchCursor, moveCursor, moveCursorEnd
                                   , moveViewport, showText
                                   )
import WSEdit.Control.Global       ( forceQuit, quit, save, simulateCrash
                                   , toggleInsOvr, toggleReadOnly, toggleTabRepl
                                   , undo
                                   )
import WSEdit.Control.Marks        ( backwardsToMark, forwardToMark
                                   , toggleJumpMark
                                   )
import WSEdit.Control.Selection    ( copy, deleteSelection, ifMarked
                                   , indentSelection, initMark, paste, searchFor
                                   , unindentSelection
                                   )
import WSEdit.Control.Text         ( cleanse, delLeft, delRight, insertTab
                                   , smartHome, smartNewLine
                                   )
import WSEdit.Data                 (Keymap)
import WSEdit.Data.Algorithms      (clearMark)
import WSEdit.Help                 (keymapHelp)



-- | Default keymap used.
defaultKM :: Keymap
defaultKM =
    [ Just ( EvKey (KFun  1  ) []
           , ( showText (keymapHelp defaultKM) helpKM
             , "Show keymap help."
             )
           )
    , Nothing
    , Just ( EvKey (KChar 'u') [MCtrl]
           , ( moveViewport (-10) 0
             , "Move Viewport up 10 lines."
             )
           )
    , Just ( EvKey (KChar 'd') [MCtrl]
           , ( moveViewport 10 0
             , "Move Viewport down 10 lines."
             )
           )
    , Just ( EvKey (KChar 'l') [MCtrl]
           , ( moveViewport 0 (-10)
             , "Move Viewport left 10 columns."
             )
           )
    , Just ( EvKey (KChar 'r') [MCtrl]
           , ( moveViewport 0 10
             , "Move Viewport right 10 columns."
             )
           )
    , Just ( EvKey (KChar 'u') [MMeta]
           , ( moveViewport (-100) 0
             , "Move Viewport up 100 lines."
             )
           )
    , Just ( EvKey (KChar 'd') [MMeta]
           , ( moveViewport 100 0
             , "Move Viewport down 100 lines."
             )
           )
    , Just ( EvKey (KChar 'l') [MMeta]
           , ( moveViewport 0 (-100)
             , "Move Viewport left 100 columns."
             )
           )
    , Just ( EvKey (KChar 'r') [MMeta]
           , ( moveViewport 0 10
             , "Move Viewport right 100 columns."
             )
           )
    , Nothing
    , Just ( EvKey KUp []
           , ( moveCursor (-1) 0 >> clearMark
             , "Move Cursor up 1 line."
             )
           )
    , Just ( EvKey KDown []
           , ( moveCursor 1 0 >> clearMark
             , "Move Cursor down 1 line."
             )
           )
    , Just ( EvKey KLeft []
           , ( moveCursor 0 (-1) >> clearMark
             , "Move Cursor left 1 column."
             )
           )
    , Just ( EvKey KRight []
           , ( moveCursor 0 1 >> clearMark
             , "Move Cursor right 1 column."
             )
           )
    , Just ( EvKey KUp [MCtrl]
           , ( moveCursor (-10) 0 >> clearMark
             , "Move Cursor up 10 lines."
             )
           )
    , Just ( EvKey KDown [MCtrl]
           , ( moveCursor 10 0 >> clearMark
             , "Move Cursor down 10 lines."
             )
           )
    , Just ( EvKey KLeft [MCtrl]
           , ( moveCursor 0 (-10) >> clearMark
             , "Move Cursor left 10 columns."
             )
           )
    , Just ( EvKey KRight [MCtrl]
           , ( moveCursor 0 10 >> clearMark
             , "Move Cursor right 10 columns."
             )
           )
    , Just ( EvKey KUp [MMeta]
           , ( moveCursor (-100) 0 >> clearMark
             , "Move Cursor up 100 lines."
             )
           )
    , Just ( EvKey KDown [MMeta]
           , ( moveCursor 100 0 >> clearMark
             , "Move Cursor down 100 lines."
             )
           )
    , Just ( EvKey KLeft [MMeta]
           , ( moveCursor 0 (-100) >> clearMark
             , "Move Cursor left 100 columns."
             )
           )
    , Just ( EvKey KRight [MMeta]
           , ( moveCursor 0 100 >> clearMark
             , "Move Cursor right 100 columns."
             )
           )
    , Nothing
    , Just ( EvKey KHome []
           , ( smartHome >> clearMark
             , "Move Cursor to the start of the line (excluding leading whitespace)."
             )
           )
    , Just ( EvKey KEnd []
           , ( moveCursorEnd >> clearMark
             , "Move Cursor to the end of the line."
             )
           )
    , Just ( EvKey (KChar '@') [MCtrl]
           , ( fetchCursor >> clearMark
             , "Fetch cursor to the center of the viewport."
             )
           )
    , Nothing
    , Just ( EvKey KUp [MShift]
           , ( initMark >> moveCursor (-1) 0
             , "Move Cursor up 1 line, selecting text."
             )
           )
    , Just ( EvKey KDown [MShift]
           , ( initMark >> moveCursor 1 0
             , "Move Cursor down 1 line, selecting text."
             )
           )
    , Just ( EvKey KLeft [MShift]
           , ( initMark >> moveCursor 0  (-1)
             , "Move Cursor left 1 column, selecting text."
             )
           )
    , Just ( EvKey KRight [MShift]
           , ( initMark >> moveCursor 0 1
             , "Move Cursor right 1 column, selecting text."
             )
           )
    , Just ( EvKey KHome [MShift]
           , ( initMark >> smartHome
             , "Move Cursor to the start of the line (excluding leading whitespace), selecting text."
             )
           )
    , Just ( EvKey KEnd [MShift]
           , ( initMark >> moveCursorEnd
             , "Move Cursor to the end of the line, selecting text."
             )
           )
    , Just ( EvKey KUp [MShift, MCtrl]
           , ( initMark >> moveCursor (-10) 0
             , "Move Cursor up 10 lines, selecting text."
             )
           )
    , Just ( EvKey KDown [MShift, MCtrl]
           , ( initMark >> moveCursor 10 0
             , "Move Cursor down 10 lines, selecting text."
             )
           )
    , Just ( EvKey KLeft [MShift, MCtrl]
           , ( initMark >> moveCursor 0 (-10)
             , "Move Cursor left 10 columns, selecting text."
             )
           )
    , Just ( EvKey KRight [MShift, MCtrl]
           , ( initMark >> moveCursor 0 10
             , "Move Cursor right 10 columns, selecting text."
             )
           )
    , Just ( EvKey KEsc []
           , ( clearMark
             , "Unselect."
             )
           )
    , Nothing
    , Just ( EvKey (KChar 'y') [MCtrl]
           , ( toggleJumpMark
             , "Toggle a jump mark in the current line."
             )
           )
    , Just ( EvKey (KChar 'n') [MCtrl]
           , ( forwardToMark
             , "Advance to the next jump mark."
             )
           )
    , Just ( EvKey (KChar 'b') [MCtrl]
           , ( backwardsToMark
             , "Go back to the previous jump mark."
             )
           )
    , Nothing
    , Just ( EvKey KBS []
           , ( ifMarked deleteSelection delLeft >> clearMark
             , "Delete character left of the cursor."
             )
           )
    , Just (EvKey KDel []
           , ( ifMarked deleteSelection delRight >> clearMark
             , "Delete charcter under the cursor."
             )
           )
    , Just ( EvKey KIns []
           , ( toggleInsOvr
             , "Toggle insert / overwrite mode."
             )
           )
    , Nothing
    , Just ( EvKey (KChar '\t') []
           , ( ifMarked indentSelection $ completeOr insertTab >> clearMark
             , "Increase the indentation of the current selection OR apply autocomplete OR insert a tab."
             )
           )
    , Just ( EvKey (KChar 't') [MMeta, MCtrl]
           , ( toggleTabRepl
             , "Toggle tab replacement (tabs/spaces)."
             )
           )
    , Just ( EvKey KBackTab []
           , ( ifMarked unindentSelection $ initMark >> unindentSelection >> clearMark
             , "Unindent the current selection OR unindent the current line."
             )
           )
    , Just ( EvKey KEnter []
           , ( smartNewLine >> clearMark
             , "Create a new line with the same indentation as the current one."
             )
           )
    , Nothing
    , Just ( EvKey (KChar 'c' ) [MCtrl]
           , ( copy
             , "Copy current selection."
             )
           )
    , Just ( EvKey (KChar 'x') [MCtrl]
           , ( copy >> deleteSelection >> clearMark
             , "Cut current selection."
             )
           )
    , Just ( EvKey (KChar 'v') [MCtrl]
           , ( ifMarked deleteSelection (return ()) >> paste >> clearMark
             , "Paste."
             )
           )
    , Just ( EvKey (KChar 'f') [MCtrl]
           , ( searchFor
             , "Add the selection to the list of highlighted search terms, or pop \
               \the last one if the selection is empty."
             )
           )
    , Nothing
    , Just ( EvKey (KChar 'r') [MMeta, MCtrl]
           , ( toggleReadOnly
             , "Toggle read-only mode."
             )
           )
    , Nothing
    , Just ( EvKey (KChar 'z') [MCtrl]
           , ( undo >> clearMark
             , "Undo last action."
             )
           )
    , Nothing
    , Just ( EvKey (KChar 's') [MCtrl]
           , ( cleanse >> save
             , "Remove trailing whitespace, ensure that the last line is empty,"
                ++ " then save."
             )
           )
    , Nothing
    , Just ( EvKey (KChar 'q') [MCtrl]
           , ( quit
             , "Quit."
             )
           )
    , Just ( EvKey (KChar 'q') [MMeta, MCtrl]
           , ( forceQuit
             , "Quit, even if unsaved changes are present."
             )
           )
    , Just ( EvKey (KChar '.') [MMeta]
           , ( simulateCrash
             , "Crash the editor and create a state dump."
             )
           )
    ]



-- | Keymap for the help screen.

helpKM :: Keymap
helpKM =
    [ Just ( EvKey (KChar 'u') [MCtrl]
           , ( moveViewport (-10) 0
             , "Move Viewport up 10 lines."
             )
           )
    , Just ( EvKey (KChar 'd') [MCtrl]
           , ( moveViewport 10 0
             , "Move Viewport down 10 lines."
             )
           )
    , Just ( EvKey (KChar 'l') [MCtrl]
           , ( moveViewport 0 (-10)
             , "Move Viewport left 10 columns."
             )
           )
    , Just ( EvKey (KChar 'r') [MCtrl]
           , ( moveViewport 0 10
             , "Move Viewport right 10 columns."
             )
           )
    , Just ( EvKey (KChar 'u') [MMeta]
           , ( moveViewport (-100) 0
             , "Move Viewport up 100 lines."
             )
           )
    , Just ( EvKey (KChar 'd') [MMeta]
           , ( moveViewport 100 0
             , "Move Viewport down 100 lines."
             )
           )
    , Just ( EvKey (KChar 'l') [MMeta]
           , ( moveViewport 0 (-100)
             , "Move Viewport left 100 columns."
             )
           )
    , Just ( EvKey (KChar 'r') [MMeta]
           , ( moveViewport 0 10
             , "Move Viewport right 100 columns."
             )
           )
    , Nothing
    , Just ( EvKey KUp []
           , ( moveCursor (-1) 0 >> clearMark
             , "Move Cursor up 1 line."
             )
           )
    , Just ( EvKey KDown []
           , ( moveCursor 1 0 >> clearMark
             , "Move Cursor down 1 line."
             )
           )
    , Just ( EvKey KLeft []
           , ( moveCursor 0 (-1) >> clearMark
             , "Move Cursor left 1 column."
             )
           )
    , Just ( EvKey KRight []
           , ( moveCursor 0 1 >> clearMark
             , "Move Cursor right 1 column."
             )
           )
    , Just ( EvKey KUp [MCtrl]
           , ( moveCursor (-10) 0 >> clearMark
             , "Move Cursor up 10 lines."
             )
           )
    , Just ( EvKey KDown [MCtrl]
           , ( moveCursor 10 0 >> clearMark
             , "Move Cursor down 10 lines."
             )
           )
    , Just ( EvKey KLeft [MCtrl]
           , ( moveCursor 0 (-10) >> clearMark
             , "Move Cursor left 10 columns."
             )
           )
    , Just ( EvKey KRight [MCtrl]
           , ( moveCursor 0 10 >> clearMark
             , "Move Cursor right 10 columns."
             )
           )
    , Just ( EvKey KUp [MMeta]
           , ( moveCursor (-100) 0 >> clearMark
             , "Move Cursor up 100 lines."
             )
           )
    , Just ( EvKey KDown [MMeta]
           , ( moveCursor 100 0 >> clearMark
             , "Move Cursor down 100 lines."
             )
           )
    , Just ( EvKey KLeft [MMeta]
           , ( moveCursor 0 (-100) >> clearMark
             , "Move Cursor left 100 columns."
             )
           )
    , Just ( EvKey KRight [MMeta]
           , ( moveCursor 0 100 >> clearMark
             , "Move Cursor right 100 columns."
             )
           )
    , Nothing
    , Just ( EvKey (KFun 1) []
           , ( quit
             , "Quit."
             )
           )
    , Just ( EvKey (KEsc) []
           , ( quit
             , "Quit."
             )
           )
    , Just ( EvKey (KChar 'q') []
           , ( quit
             , "Quit."
             )
           )
    , Just ( EvKey (KChar 'q') [MCtrl]
           , ( quit
             , "Quit."
             )
           )
    , Just ( EvKey (KChar 'q') [MMeta, MCtrl]
           , ( forceQuit
             , "Quit, even if unsaved changes are present."
             )
           )
    , Just ( EvKey (KChar '.') [MMeta]
           , ( simulateCrash
             , "Crash the editor and create a state dump."
             )
           )
    ]
