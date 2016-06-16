module WSEdit.Data.Pretty
    ( PrettyEdConfig
    , prettyEdConfig
    , unPrettyEdConfig
    , PrettyEdDesign
    , prettyEdDesign
    , unPrettyEdDesign
    , PrettyKeymap
    , prettyKeymap
    ) where


import Control.Exception (SomeException)
import Graphics.Vty      (Attr, Event, Vty)

import WSEdit.Data       ( EdConfig ( EdConfig, drawBg, dumpEvents, edDesign
                                    , histSize, keymap, purgeOnClose, tabWidth
                                    , vtyObj
                                    )
                         , EdDesign ( EdDesign, dBGChar, dBGFormat, dCharStyles
                                    , dColChar, dColNoFormat, dColNoInterval
                                    , dCurrLnMod, dFrameFormat, dLineNoFormat
                                    , dLineNoInterv, dSelFormat, dStatusFormat
                                    , dTabStr
                                    )
                         , EdState
                         , Keymap
                         )
import WSEdit.Util       (CharClass, withSnd)



-- | 'Show'able representation of 'EdConfig'.
data PrettyEdConfig = PrettyEdConfig
    { pVtyObj       :: ()
    , pEdDesign     :: PrettyEdDesign
    , pKeymap       :: PrettyKeymap
    , pHistSize     :: Int
    , pTabWidth     :: Int
    , pDrawBg       :: Bool
    , pDumpEvents   :: Bool
    , pPurgeOnClose :: Bool
    }
    deriving (Read, Show)

-- | Create a 'PrettyEdConfig' from an 'EdConfig'.
prettyEdConfig :: EdConfig -> PrettyEdConfig
prettyEdConfig c = PrettyEdConfig
    { pVtyObj       = ()
    , pEdDesign     = prettyEdDesign $ edDesign     c
    , pKeymap       = prettyKeymap   $ keymap       c
    , pHistSize     =                  histSize     c
    , pTabWidth     =                  tabWidth     c
    , pDrawBg       =                  drawBg       c
    , pDumpEvents   =                  dumpEvents   c
    , pPurgeOnClose =                  purgeOnClose c
    }

-- | Restore an 'EdConfig' from a 'PrettyEdConfig'.
unPrettyEdConfig :: Vty -> Keymap -> (Attr -> Attr) -> PrettyEdConfig -> EdConfig
unPrettyEdConfig v k f p = EdConfig
    { vtyObj       = v
    , edDesign     = unPrettyEdDesign f $ pEdDesign p
    , keymap       = k
    , histSize     = pHistSize                    p
    , tabWidth     = pTabWidth                    p
    , drawBg       = pDrawBg                      p
    , dumpEvents   = pDumpEvents                  p
    , purgeOnClose = pPurgeOnClose                p
    }




-- | 'Show'able representation of 'EdDesign".
data PrettyEdDesign = PrettyEdDesign
    { pDFrameFormat   :: Attr
    , pDStatusFormat  :: Attr
    , pDLineNoFormat  :: Attr
    , pDLineNoInterv  :: Int
    , pDColNoInterval :: Int
    , pDColNoFormat   :: Attr
    , pDBGChar        :: Char
    , pDColChar       :: Maybe Char
    , pDBGFormat      :: Attr
    , pDCurrLnMod     :: ()
    , pDTabStr        :: String
    , pDSelFormat     :: Attr
    , pDCharStyles    :: [(CharClass, Attr)]
    }
    deriving (Read, Show)

-- | Create a 'PrettyEdDesign' from an 'EdDesign'.
prettyEdDesign :: EdDesign -> PrettyEdDesign
prettyEdDesign d = PrettyEdDesign
    { pDFrameFormat   = dFrameFormat   d
    , pDStatusFormat  = dStatusFormat  d
    , pDLineNoFormat  = dLineNoFormat  d
    , pDLineNoInterv  = dLineNoInterv  d
    , pDColNoInterval = dColNoInterval d
    , pDColNoFormat   = dColNoFormat   d
    , pDBGChar        = dBGChar        d
    , pDColChar       = dColChar       d
    , pDBGFormat      = dBGFormat      d
    , pDCurrLnMod     = ()
    , pDTabStr        = dTabStr        d
    , pDSelFormat     = dSelFormat     d
    , pDCharStyles    = dCharStyles    d
    }

-- | Restore an 'EdConfig' from a 'PrettyEdConfig'.
unPrettyEdDesign :: (Attr -> Attr) -> PrettyEdDesign -> EdDesign
unPrettyEdDesign f p = EdDesign
    { dFrameFormat   = pDFrameFormat   p
    , dStatusFormat  = pDStatusFormat  p
    , dLineNoFormat  = pDLineNoFormat  p
    , dLineNoInterv  = pDLineNoInterv  p
    , dColNoInterval = pDColNoInterval p
    , dColNoFormat   = pDColNoFormat   p
    , dBGChar        = pDBGChar        p
    , dColChar       = pDColChar       p
    , dBGFormat      = pDBGFormat      p
    , dCurrLnMod     = f
    , dTabStr        = pDTabStr        p
    , dSelFormat     = pDSelFormat     p
    , dCharStyles    = pDCharStyles    p
    }



-- | 'Show'able representation of 'Keymap'.
type PrettyKeymap = [(Event, String)]

-- | Create a 'PrettyKeymap' from a 'Keymap'.
prettyKeymap :: Keymap -> PrettyKeymap
prettyKeymap = map (withSnd snd)
