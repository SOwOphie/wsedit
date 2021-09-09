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

import Graphics.Vty
    ( Attr
    , Event
    , Vty
    )
import System.IO
    ( NewlineMode
    )

import WSEdit.Data
    ( EdConfig
        ( EdConfig
        , addnIdChars
        , atomicSaves
        , blockComment
        , brackets
        , chrDelim
        , drawBg
        , dumpEvents
        , edDesign
        , encoding
        , escapeO
        , escapeS
        , histSize
        , initJMarks
        , keymap
        , keyPrfxs
        , keywords
        , lineComment
        , mStrDelim
        , newlineMode
        , preserveWsp
        , purgeOnClose
        , readEnc
        , strDelim
        , tabWidth
        , vtyObj
        , wriCheck
        )
    , EdDesign
        ( EdDesign
        , dBGChar
        , dBGFormat
        , dBrMod
        , dCharStyles
        , dColChar
        , dColNoFormat
        , dColNoInterval
        , dCurrLnMod
        , dFrameFormat
        , dHLStyles
        , dJumpMarkFmt
        , dLineNoFormat
        , dLineNoInterv
        , dStatusFormat
        , dTabStr
        )
    , HighlightMode
    , Keymap
    , ReadEnc
    )
import WSEdit.Util
    ( CharClass
    , withSnd
    )



-- | 'Show'able representation of 'EdConfig'.
data PrettyEdConfig = PrettyEdConfig
    { pVtyObj       :: ()
    , pEdDesign     :: PrettyEdDesign
    , pKeymap       :: PrettyKeymap
    , pHistSize     :: Int
    , pTabWidth     :: Int
    , pDrawBg       :: Bool
    , pDumpEvents   :: Bool
    , pAtomicSaves  :: Bool
    , pWriCheck     :: Bool
    , pPurgeOnClose :: Bool
    , pPreserveWsp  :: Bool
    , pInitJMarks   :: [Int]
    , pNewlineMode  :: NewlineMode
    , pEncoding     :: Maybe String
    , pReadEnc      :: ReadEnc
    , pLineComment  :: [String]
    , pBlockComment :: [(String, String)]
    , pStrDelim     :: [(String, String)]
    , pMStrDelim    :: [(String, String)]
    , pChrDelim     :: [(String, String)]
    , pKeywords     :: [String]
    , pKeyPrfxs     :: [String]
    , pEscapeO      :: [String]
    , pEscapeS      :: [String]
    , pBrackets     :: [(String, String)]
    , pAddnIdChars  :: [Char]
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
    , pAtomicSaves  =                  atomicSaves  c
    , pWriCheck     =                  wriCheck     c
    , pPurgeOnClose =                  purgeOnClose c
    , pPreserveWsp  =                  preserveWsp  c
    , pInitJMarks   =                  initJMarks   c
    , pNewlineMode  =                  newlineMode  c
    , pEncoding     =                  encoding     c
    , pReadEnc      =                  readEnc      c
    , pLineComment  =                  lineComment  c
    , pBlockComment =                  blockComment c
    , pStrDelim     =                  strDelim     c
    , pMStrDelim    =                  mStrDelim    c
    , pChrDelim     =                  chrDelim     c
    , pKeywords     =                  keywords     c
    , pKeyPrfxs     =                  keyPrfxs     c
    , pEscapeO      =                  escapeO      c
    , pEscapeS      =                  escapeS      c
    , pBrackets     =                  brackets     c
    , pAddnIdChars  =                  addnIdChars  c
    }

-- | Restore an 'EdConfig' from a 'PrettyEdConfig'.
unPrettyEdConfig :: Vty -> Keymap -> PrettyEdConfig -> EdConfig
unPrettyEdConfig v k p = EdConfig
    { vtyObj       = v
    , edDesign     = unPrettyEdDesign $ pEdDesign p
    , keymap       = k
    , histSize     = pHistSize                    p
    , tabWidth     = pTabWidth                    p
    , drawBg       = pDrawBg                      p
    , dumpEvents   = pDumpEvents                  p
    , atomicSaves  = pAtomicSaves                 p
    , wriCheck     = pWriCheck                    p
    , purgeOnClose = pPurgeOnClose                p
    , preserveWsp  = pPreserveWsp                 p
    , initJMarks   = pInitJMarks                  p
    , newlineMode  = pNewlineMode                 p
    , encoding     = pEncoding                    p
    , readEnc      = pReadEnc                     p
    , lineComment  = pLineComment                 p
    , blockComment = pBlockComment                p
    , strDelim     = pStrDelim                    p
    , mStrDelim    = pMStrDelim                   p
    , chrDelim     = pChrDelim                    p
    , keywords     = pKeywords                    p
    , keyPrfxs     = pKeyPrfxs                    p
    , escapeO      = pEscapeO                     p
    , escapeS      = pEscapeS                     p
    , brackets     = pBrackets                    p
    , addnIdChars  = pAddnIdChars                 p
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
    , pDCurrLnMod     :: Attr
    , pDBrMod         :: Attr
    , pDJumpMarkFmt   :: Attr
    , pDTabStr        :: (Char, Char, Char)
    , pDCharStyles    :: [(CharClass    , Attr)]
    , pDHLStyles      :: [(HighlightMode, Attr)]
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
    , pDCurrLnMod     = dCurrLnMod     d
    , pDBrMod         = dBrMod         d
    , pDJumpMarkFmt   = dJumpMarkFmt   d
    , pDTabStr        = dTabStr        d
    , pDCharStyles    = dCharStyles    d
    , pDHLStyles      = dHLStyles      d
    }

-- | Restore an 'EdConfig' from a 'PrettyEdConfig'.
unPrettyEdDesign :: PrettyEdDesign -> EdDesign
unPrettyEdDesign p = EdDesign
    { dFrameFormat   = pDFrameFormat   p
    , dStatusFormat  = pDStatusFormat  p
    , dLineNoFormat  = pDLineNoFormat  p
    , dLineNoInterv  = pDLineNoInterv  p
    , dColNoInterval = pDColNoInterval p
    , dColNoFormat   = pDColNoFormat   p
    , dBGChar        = pDBGChar        p
    , dColChar       = pDColChar       p
    , dBGFormat      = pDBGFormat      p
    , dCurrLnMod     = pDCurrLnMod     p
    , dBrMod         = pDBrMod         p
    , dJumpMarkFmt   = pDJumpMarkFmt   p
    , dTabStr        = pDTabStr        p
    , dCharStyles    = pDCharStyles    p
    , dHLStyles      = pDHLStyles      p
    }



-- | 'Show'able representation of 'Keymap'.
type PrettyKeymap = [Maybe (Event, String)]

-- | Create a 'PrettyKeymap' from a 'Keymap'.
prettyKeymap :: Keymap -> PrettyKeymap
prettyKeymap = map (fmap $ withSnd snd)
