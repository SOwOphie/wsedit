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

import Graphics.Vty      (Attr, Event, Vty)

import WSEdit.Data       ( EdConfig ( EdConfig, drawBg, dumpEvents, edDesign
                                    , escape, histSize, keymap, keywords
                                    , lineComment, purgeOnClose, strDelim
                                    , tabWidth, vtyObj
                                    )
                         , EdDesign ( EdDesign, dBGChar, dBGFormat, dCharStyles
                                    , dColChar, dColNoFormat, dColNoInterval
                                    , dCommentFormat, dCurrLnMod, dFrameFormat
                                    , dKeywordFormat, dLineNoFormat
                                    , dLineNoInterv, dSearchFormat, dSelFormat
                                    , dStatusFormat, dStrFormat, dTabExt
                                    , dTabStr
                                    )
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
    , pLineComment  :: [String]
    , pStrDelim     :: [(Char, Char)]
    , pKeywords     :: [String]
    , pEscape       :: Maybe Char
    }
    deriving (Eq, Read, Show)

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
    , pLineComment  =                  lineComment  c
    , pStrDelim     =                  strDelim     c
    , pKeywords     =                  keywords     c
    , pEscape       =                  escape       c
    }

-- | Restore an 'EdConfig' from a 'PrettyEdConfig'.
unPrettyEdConfig :: Vty -> Keymap -> (Attr -> Attr) -> PrettyEdConfig -> EdConfig
unPrettyEdConfig v k f p = EdConfig
    { vtyObj       = v
    , edDesign     = unPrettyEdDesign f $ pEdDesign p
    , keymap       = k
    , histSize     = pHistSize                      p
    , tabWidth     = pTabWidth                      p
    , drawBg       = pDrawBg                        p
    , dumpEvents   = pDumpEvents                    p
    , purgeOnClose = pPurgeOnClose                  p
    , lineComment  = pLineComment                   p
    , strDelim     = pStrDelim                      p
    , keywords     = pKeywords                      p
    , escape       = pEscape                        p
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
    , pDTabExt        :: Char
    , pDSelFormat     :: Attr
    , pDCharStyles    :: [(CharClass, Attr)]
    , pDCommentFormat :: Attr
    , pDStrFormat     :: Attr
    , pDKeywordFormat :: Attr
    , pDSearchFormat  :: Attr
    }
    deriving (Eq, Read, Show)

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
    , pDTabExt        = dTabExt        d
    , pDSelFormat     = dSelFormat     d
    , pDCharStyles    = dCharStyles    d
    , pDCommentFormat = dCommentFormat d
    , pDStrFormat     = dStrFormat     d
    , pDKeywordFormat = dKeywordFormat d
    , pDSearchFormat  = dSearchFormat  d
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
    , dTabExt        = pDTabExt        p
    , dSelFormat     = pDSelFormat     p
    , dCharStyles    = pDCharStyles    p
    , dCommentFormat = pDCommentFormat p
    , dStrFormat     = pDStrFormat     p
    , dKeywordFormat = pDKeywordFormat p
    , dSearchFormat  = pDSearchFormat  p
    }



-- | 'Show'able representation of 'Keymap'.
type PrettyKeymap = [(Event, String)]

-- | Create a 'PrettyKeymap' from a 'Keymap'.
prettyKeymap :: Keymap -> PrettyKeymap
prettyKeymap = map (withSnd snd)
