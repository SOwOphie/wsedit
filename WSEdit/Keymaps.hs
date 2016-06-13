module WSEdit.Keymaps
    ( defaultKM
    ) where


import Graphics.Vty   ( Event (EvKey)
                      , Key ( KBackTab, KBS, KChar, KDel, KDown, KEnd, KEnter
                            , KEsc, KHome, KLeft, KRight, KUp
                            )
                      , Modifier (MCtrl, MMeta, MShift)
                      )

import WSEdit.Control ( cleanse, completeOr, copy, deleteSelection, delLeft
                      , delRight, fetchCursor, forceQuit, ifMarked
                      , indentSelection, initMark, insertTab, moveCursor
                      , moveViewport, paste, quit, save, simulateCrash
                      , smartHome, smartNewLine, toggleReadOnly, toggleTabRepl
                      , unindentSelection, undo
                      )
import WSEdit.Data    (Keymap, clearMark)



-- | Default keymap used.
defaultKM :: Keymap
defaultKM =
    [ (EvKey (KChar 'u' ) [               MCtrl],                                          moveViewport (-10)       0              )
    , (EvKey (KChar 'd' ) [               MCtrl],                                          moveViewport   10        0              )
    , (EvKey (KChar 'l' ) [               MCtrl],                                          moveViewport    0  (-   10)             )
    , (EvKey (KChar 'r' ) [               MCtrl],                                          moveViewport    0       10              )

    , (EvKey  KUp         [                    ],                                          moveCursor   (- 1)       0  >> clearMark)
    , (EvKey  KDown       [                    ],                                          moveCursor      1        0  >> clearMark)
    , (EvKey  KLeft       [                    ],                                          moveCursor      0  (-    1) >> clearMark)
    , (EvKey  KRight      [                    ],                                          moveCursor      0        1  >> clearMark)

    , (EvKey  KUp         [               MCtrl],                                          moveCursor   (-10)       0  >> clearMark)
    , (EvKey  KDown       [               MCtrl],                                          moveCursor     10        0  >> clearMark)
    , (EvKey  KLeft       [               MCtrl],                                          moveCursor      0  (-   10) >> clearMark)
    , (EvKey  KRight      [               MCtrl],                                          moveCursor      0       10  >> clearMark)

    , (EvKey  KHome       [                    ],                                          smartHome                   >> clearMark)
    , (EvKey  KEnd        [                    ],                                          moveCursor      0    65535  >> clearMark)

    , (EvKey (KChar '@')  [               MCtrl],                                          fetchCursor                 >> clearMark)

    , (EvKey  KUp         [       MShift       ],                              initMark >> moveCursor   (- 1)       0              )
    , (EvKey  KDown       [       MShift       ],                              initMark >> moveCursor      1        0              )
    , (EvKey  KLeft       [       MShift       ],                              initMark >> moveCursor      0  (-    1)             )
    , (EvKey  KRight      [       MShift       ],                              initMark >> moveCursor      0        1              )

    , (EvKey  KUp         [       MShift, MCtrl],                              initMark >> moveCursor   (-10)       0              )
    , (EvKey  KDown       [       MShift, MCtrl],                              initMark >> moveCursor     10        0              )
    , (EvKey  KLeft       [       MShift, MCtrl],                              initMark >> moveCursor      0  (-   10)             )
    , (EvKey  KRight      [       MShift, MCtrl],                              initMark >> moveCursor      0       10              )

    , (EvKey  KHome       [       MShift       ],                              initMark >> smartHome                               )
    , (EvKey  KEnd        [       MShift       ],                              initMark >> moveCursor      0    65535              )

    , (EvKey  KBS         [                    ], ifMarked deleteSelection                 delLeft                     >> clearMark)
    , (EvKey  KDel        [                    ], ifMarked deleteSelection                 delRight                    >> clearMark)
    , (EvKey (KChar '\t') [                    ], ifMarked indentSelection                (completeOr insertTab        >> clearMark))
    , (EvKey (KChar '\t') [MMeta               ],                                          toggleTabRepl                           )
    , (EvKey  KBackTab    [                    ], ifMarked unindentSelection $ initMark >> unindentSelection           >> clearMark)
    , (EvKey  KEnter      [                    ],                                          smartNewLine                >> clearMark)

    , (EvKey  KEsc        [                    ],                                                                         clearMark)

    , (EvKey (KChar 's' ) [               MCtrl], cleanse >> save                                                                  )
    , (EvKey (KChar 'r' ) [MMeta,         MCtrl], toggleReadOnly                                                                   )

    , (EvKey (KChar 'z' ) [               MCtrl], undo                                                                 >> clearMark)

    , (EvKey (KChar 'c' ) [               MCtrl], copy                                                                             )
    , (EvKey (KChar 'x' ) [               MCtrl], copy  >> deleteSelection                                             >> clearMark)
    , (EvKey (KChar 'v' ) [               MCtrl], ifMarked deleteSelection (return ()) >> paste                        >> clearMark)

    , (EvKey (KChar 'q' ) [               MCtrl], quit                                                                             )
    , (EvKey (KChar 'q' ) [MMeta,         MCtrl], forceQuit                                                                        )
    , (EvKey (KChar 'c' ) [MMeta,         MCtrl], simulateCrash                                                                    )
    ]
