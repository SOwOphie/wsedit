module WSEdit.Keymaps
    ( defaultKM
    ) where


import Graphics.Vty   ( Event (EvKey)
                      , Key ( KBackTab, KBS, KChar, KDel, KDown, KEnd, KEnter
                            , KEsc, KHome, KLeft, KRight, KUp
                            )
                      , Modifier (MCtrl, MMeta, MShift)
                      )

import WSEdit.Control.Autocomplete (completeOr)
import WSEdit.Control.Base         (fetchCursor, moveCursor, moveViewport)
import WSEdit.Control.Global       ( forceQuit, quit, save, simulateCrash
                                   , toggleReadOnly, toggleTabRepl, undo
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
import WSEdit.Data                 (Keymap, clearMark)



-- | Default keymap used.
defaultKM :: Keymap
defaultKM =
    [ (EvKey (KChar 'u' ) [               MCtrl], (                                          moveViewport (-10)       0
                                                  , "Move Viewport up 10 lines."
                                                  )
      )
    , (EvKey (KChar 'd' ) [               MCtrl], (                                          moveViewport   10        0
                                                  , "Move Viewport down 10 lines."
                                                  )
      )
    , (EvKey (KChar 'l' ) [               MCtrl], (                                          moveViewport    0  (-   10)
                                                  , "Move Viewport left 10 columns."
                                                  )
      )
    , (EvKey (KChar 'r' ) [               MCtrl], (                                          moveViewport    0       10
                                                  , "Move Viewport right 10 columns."
                                                  )
      )

    , (EvKey  KUp         [                    ], (                                          moveCursor   (- 1)       0  >> clearMark
                                                  , "Move Cursor up 1 line."
                                                  )
      )
    , (EvKey  KDown       [                    ], (                                          moveCursor      1        0  >> clearMark
                                                  , "Move Cursor down 1 line."
                                                  )
      )
    , (EvKey  KLeft       [                    ], (                                          moveCursor      0  (-    1) >> clearMark
                                                  , "Move Cursor left 1 column."
                                                  )
      )
    , (EvKey  KRight      [                    ], (                                          moveCursor      0        1  >> clearMark
                                                  , "Move Cursor right 1 column."
                                                  )
      )

    , (EvKey  KUp         [               MCtrl], (                                          moveCursor   (-10)       0  >> clearMark
                                                  , "Move Cursor up 10 lines."
                                                  )
      )
    , (EvKey  KDown       [               MCtrl], (                                          moveCursor     10        0  >> clearMark
                                                  , "Move Cursor down 10 lines."
                                                  )
      )
    , (EvKey  KLeft       [               MCtrl], (                                          moveCursor      0  (-   10) >> clearMark
                                                  , "Move Cursor left 10 columns."
                                                  )
      )
    , (EvKey  KRight      [               MCtrl], (                                          moveCursor      0       10  >> clearMark
                                                  , "Move Cursor right 10 columns."
                                                  )
      )

    , (EvKey  KHome       [                    ], (                                          smartHome                   >> clearMark
                                                  , "Move Cursor to the start of the line (excluding leading whitespace)."
                                                  )
      )
    , (EvKey  KEnd        [                    ], (                                          moveCursor      0    65535  >> clearMark
                                                  , "Move Cursor to the end of the line."
                                                  )
      )

    , (EvKey (KChar '@')  [               MCtrl], (                                          fetchCursor                 >> clearMark
                                                  , "Fetch cursor to the center of the viewport."
                                                  )
      )

    , (EvKey  KUp         [       MShift       ], (                              initMark >> moveCursor   (- 1)       0
                                                  , "Move Cursor up 1 line, selecting text."
                                                  )
      )
    , (EvKey  KDown       [       MShift       ], (                              initMark >> moveCursor      1        0
                                                  , "Move Cursor down 1 line, selecting text."
                                                  )
      )
    , (EvKey  KLeft       [       MShift       ], (                              initMark >> moveCursor      0  (-    1)
                                                  , "Move Cursor left 1 column, selecting text."
                                                  )
      )
    , (EvKey  KRight      [       MShift       ], (                              initMark >> moveCursor      0        1
                                                  , "Move Cursor right 1 column, selecting text."
                                                  )
      )

    , (EvKey  KUp         [       MShift, MCtrl], (                              initMark >> moveCursor   (-10)       0
                                                  , "Move Cursor up 10 lines, selecting text."
                                                  )
      )
    , (EvKey  KDown       [       MShift, MCtrl], (                              initMark >> moveCursor     10        0
                                                  , "Move Cursor down 10 lines, selecting text."
                                                  )
      )
    , (EvKey  KLeft       [       MShift, MCtrl], (                              initMark >> moveCursor      0  (-   10)
                                                  , "Move Cursor left 10 columns, selecting text."
                                                  )
      )
    , (EvKey  KRight      [       MShift, MCtrl], (                              initMark >> moveCursor      0       10
                                                  , "Move Cursor right 10 columns, selecting text."
                                                  )
      )

    , (EvKey  KHome       [       MShift       ], (                              initMark >> smartHome
                                                  , "Move Cursor to the start of the line (excluding leading whitespace), selecting text."
                                                  )
      )
    , (EvKey  KEnd        [       MShift       ], (                              initMark >> moveCursor      0    65535
                                                  , "Move Cursor to the end of the line, selecting text."
                                                  )
      )

    , (EvKey (KChar 'y')  [               MCtrl], (                                          toggleJumpMark
                                                  , "Toggle a jump mark in the current line."
                                                  )
      )
    , (EvKey (KChar 'n')  [               MCtrl], (                                          forwardToMark
                                                  , "Advance to the next jump mark."
                                                  )
      )
    , (EvKey (KChar 'b')  [               MCtrl], (                                          backwardsToMark
                                                  , "Go back to the previous jump mark."
                                                  )
      )

    , (EvKey  KBS         [                    ], ( ifMarked deleteSelection                 delLeft                     >> clearMark
                                                  , "Delete character left of the cursor."
                                                  )
      )
    , (EvKey  KDel        [                    ], ( ifMarked deleteSelection                 delRight                    >> clearMark
                                                  , "Delete charcter under the cursor."
                                                  )
      )
    , (EvKey (KChar '\t') [                    ], ( ifMarked indentSelection                (completeOr insertTab        >> clearMark)
                                                  , "Increase the indentation of the current selection OR apply autocomplete OR insert a tab."
                                                  )
      )
    , (EvKey (KChar '\t') [MMeta               ], (                                          toggleTabRepl
                                                  , "Toggle tab replacement (tabs/spaces)."
                                                  )
      )
    , (EvKey  KBackTab    [                    ], ( ifMarked unindentSelection $ initMark >> unindentSelection           >> clearMark
                                                  , "Unindent the current selection OR unindent the current line."
                                                  )
      )
    , (EvKey  KEnter      [                    ], (                                          smartNewLine                >> clearMark
                                                  , "Create a new line with the same indentation as the current one."
                                                  )
      )

    , (EvKey  KEsc        [                    ], (                                                                         clearMark
                                                  , "Unselect."
                                                  )
      )

    , (EvKey (KChar 's' ) [               MCtrl], ( cleanse >> save
                                                  , "Remove trailing whitespace, then save."
                                                  )
      )
    , (EvKey (KChar 'r' ) [MMeta,         MCtrl], ( toggleReadOnly
                                                  , "Toggle read-only mode."
                                                  )
      )

    , (EvKey (KChar 'z' ) [               MCtrl], ( undo                                                                 >> clearMark
                                                  , "Undo last action."
                                                  )
      )

    , (EvKey (KChar 'c' ) [               MCtrl], ( copy
                                                  , "Copy current selection."
                                                  )
      )
    , (EvKey (KChar 'x' ) [               MCtrl], ( copy  >> deleteSelection                                             >> clearMark
                                                  , "Cut current selection."
                                                  )
      )
    , (EvKey (KChar 'v' ) [               MCtrl], ( ifMarked deleteSelection (return ()) >> paste                        >> clearMark
                                                  , "Paste."
                                                  )
      )
    , (EvKey (KChar 'f' ) [               MCtrl], (                                         searchFor
                                                  , "Add the selection to the list of highlighted search terms, or pop the last one"
                                                    ++ " if the selection is empty."
                                                  )
      )

    , (EvKey (KChar 'q' ) [               MCtrl], ( quit
                                                  , "Quit."
                                                  )
      )
    , (EvKey (KChar 'q' ) [MMeta,         MCtrl], ( forceQuit
                                                  , "Quit, even if unsaved changes are present."
                                                  )
      )
    , (EvKey (KChar 'c' ) [MMeta,         MCtrl], ( simulateCrash
                                                  , "Crash the editor."
                                                  )
      )
    ]
