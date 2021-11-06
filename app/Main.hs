{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

-- aeson
import Data.Aeson
import Data.Aeson.Types

-- base
import Control.Monad (forever, zipWithM)
import Data.Function ((&))
import Data.Maybe (fromMaybe, mapMaybe)
import Numeric
import GHC.Generics
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

-- bytestring
import qualified Data.ByteString.Lazy

-- deepseq
import Control.DeepSeq

-- default
import Data.Default

-- diagrams-*
import qualified Diagrams.Backend.SVG         as D
import qualified Diagrams.Backend.SVG.CmdLine as D
import qualified Diagrams.Prelude             as D
import qualified Diagrams.TwoD.Text           as D
import qualified Diagrams.TwoD.Vector         as D

-- fixed-prec
import Data.Number.FixedPrec

-- svg-builder
import qualified Graphics.Svg.Core

-- text
import qualified Data.Text                    as T

-- utf8-string
import qualified Data.ByteString.Lazy.UTF8 as BLU

main :: IO ()
main = do
  inputs <- lines <$> getContents
  let handleParse :: Int -> String -> IO [Tick]
      handleParse i line =
        case parseTick line of
          Left err -> do
            hPutStrLn stderr $ "Error on input #" ++ show i ++ ":"
            hPutStrLn stderr $ "  Input: " ++ line
            hPutStrLn stderr $ "  Message: " ++ err
            pure []
          Right tick -> do
            hPutStrLn stderr $ "Input #" ++ show i ++ " parsed correctly."
            pure [tick]

  validTicks <- concat <$> zipWithM handleParse [1..] inputs

  let diagram = renderTicks def validTicks

  args <- getArgs
  let filename :: String
      filename = case args of
                   [] -> "/dev/stdout"
                   name:_ -> name

  writeRepToFile filename diagram

parseTick :: String -> Either String Tick
parseTick str = eitherDecode (BLU.fromString str)

type P20 = PPlus10 P10

type InternalFloat = FixedPrec P20

instance FromJSON InternalFloat where
    parseJSON = withScientific "InternalFloat" (pure . realToFrac)

instance Default (FixedPrec P20) where
    def = 0

instance NFData (FixedPrec P20) where
    rnf creal = deepseq () ()

-- type Representation Dias = D.QDiagram D.B D.V2 Double D.Any

renderTick = tickToDiagram
renderTickStatic = tickToDiagramStatic
renderTicks renderSettings ticks =
    foldMap (renderTick renderSettings) ticks
writeRepToFile path rep = do
    let options = D.SVGOptions (D.mkWidth 2000) Nothing (T.pack "") [] True
    let svgDoc = D.renderDia D.SVG options (D.frame 0.1 rep)
    let bs = Graphics.Svg.Core.renderBS svgDoc
    Data.ByteString.Lazy.writeFile path bs

tickToDiagram :: RenderSettings -> Tick -> D.Diagram D.B
tickToDiagram renderSettings@RenderSettings{ heightMultiplier, textMultiplier } tick =
    let staticTick = tickToDiagramStatic renderSettings tick
    in
    case offset tick of
        Vertical y ->
            staticTick
                & D.translate (D.r2 (realToFrac $ postPos tick, realToFrac y))
        Radial rad ->
            staticTick
                & D.translate (D.r2 (0, realToFrac rad))
                & D.rotateBy (negate $ realToFrac $ postPos tick)

tickToDiagramStatic :: RenderSettings -> Tick -> D.Diagram D.B
tickToDiagramStatic RenderSettings{ heightMultiplier, textMultiplier } tick =
    let Tick { prePos, postPos, info } = tick
        TickInfo { _start, _end, _mlabel } = info
        startV2 = D.r2 (0, heightMultiplier * _start)
        endV2   = D.r2 (0, heightMultiplier * _end)
        diffV2  = endV2 - startV2
        tickDia :: D.Diagram D.B
        tickDia = laserline [diffV2] & D.translate (fmap realToFrac startV2)
        labelDia :: D.Diagram D.B
        labelDia = fromMaybe mempty $ do
            Label {..} <- _mlabel
            let labelOffset :: D.V2 InternalFloat
                labelOffset
                  = _anchorOffset * D.r2 (1, heightMultiplier)
                  + case _tickAnchor of
                      Pct p -> startV2 + diffV2 * D.V2 p p
                      FromTopAbs x -> endV2 + D.r2 (0, heightMultiplier * x)
                      FromBottomAbs x -> startV2 + D.r2 (0, heightMultiplier * x)
            pure $
                D.alignedText (realToFrac $ _xPct _textAnchor) (realToFrac $ _yPct _textAnchor) _text
                  & D.fontSizeL (realToFrac $ heightMultiplier * textMultiplier * _fontSize) & D.fc D.black
                  & D.font "Comfortaa"
                  & D.translate (fmap realToFrac labelOffset)
     in D.lc D.red tickDia <> labelDia

laserline :: [D.V2 InternalFloat] -> D.Diagram D.B
laserline positions = D.fromOffsets ((fmap . fmap) (realToFrac :: InternalFloat -> Double) positions) & D.lineWidth D.ultraThin

data OffsetF a = Radial a | Vertical a
    deriving (Show, Functor, Generic)

instance NFData a => NFData (OffsetF a)

type Offset = OffsetF InternalFloat
type Offsetter = OffsetF (InternalFloat -> InternalFloat)

applyOffsetter :: Offsetter -> InternalFloat -> Offset
applyOffsetter offsetter x = fmap ($ x) offsetter

data TickF info = Tick
    { prePos  :: InternalFloat
    , postPos :: InternalFloat
    , offset  :: Offset
    , info    :: info
    }
    deriving (Show, Functor, Generic)

instance NFData info => NFData (TickF info)

type Tick = TickF TickInfo

instance FromJSON Tick where
  parseJSON = withObject "Tick" $ \v -> do
    prePos <- v .: "pre_pos"
    postPos <- v .: "post_pos"
    let offset = Vertical 0
    let _start = 0
    _end <- v .: "height"
    pure $ Tick { prePos, postPos, offset, info = TickInfo { _start, _end, _mlabel = Nothing } }

truePos :: TickF a -> InternalFloat
truePos Tick { postPos, offset } =
    case offset of
        Vertical _ -> postPos
        Radial r -> postPos * 2 * pi * r

instance Eq (TickF a) where
    a == b = postPos a == postPos b
instance Ord (TickF a) where
    compare a b = compare (postPos a) (postPos b)

deinfo :: TickF info -> TickF ()
deinfo = fmap (const ())

instance Default info => Default (TickF info) where
    def =
        Tick
            { prePos = 0
            , postPos = 0
            , offset = Vertical 0
            , info = def
            }

data TickInfo = TickInfo
    { _start  :: InternalFloat
    , _end    :: InternalFloat
    , _mlabel :: Maybe Label
    }
    deriving (Show, Generic)

instance NFData TickInfo

instance Default TickInfo where
    def =
        TickInfo
            { _start = 0
            , _end = 1
            , _mlabel = Nothing
            }

data Label = Label
    { _fontSize     :: InternalFloat
    , _text         :: String
    , _textAnchor   :: TextAnchor
    , _tickAnchor   :: TickAnchor
    , _anchorOffset :: D.V2 InternalFloat
    }
    deriving (Show, Generic)

instance NFData Label

instance Default Label where
    def =
        Label
            { _fontSize = 0
            , _text = ""
            , _textAnchor = TextAnchor { _xPct = 0, _yPct = 0 }
            , _tickAnchor = FromTopAbs 0
            , _anchorOffset = D.V2 0 0
            }

data TextAnchor = TextAnchor
    { _xPct :: InternalFloat
    , _yPct :: InternalFloat
    }
    deriving (Show, Generic)

instance NFData TextAnchor

data TickAnchor = Pct InternalFloat | FromTopAbs InternalFloat | FromBottomAbs InternalFloat
    deriving (Show, Generic)

instance NFData TickAnchor

-- COMMON ANCHORINGS

labelCenterOver :: InternalFloat -> Label -> Label
labelCenterOver margin label = label
    { _textAnchor = TextAnchor { _xPct = 0.5, _yPct = 0 }
    , _tickAnchor = FromTopAbs 0
    , _anchorOffset = D.V2 0 margin
    }

labelCenterUnder :: InternalFloat -> Label -> Label
labelCenterUnder margin label = label
    { _textAnchor = TextAnchor { _xPct = 0.5, _yPct = 1 }
    , _tickAnchor = FromTopAbs 0
    , _anchorOffset = D.V2 0 $ negate margin
    }

labelRight :: InternalFloat -> Label -> Label
labelRight margin label = label
    { _textAnchor = TextAnchor { _xPct = 0, _yPct = 1 }
    , _tickAnchor = FromTopAbs 0
    , _anchorOffset = D.V2 margin 0
    }

labelRightCenter :: InternalFloat -> Label -> Label
labelRightCenter margin label = label
    { _textAnchor = TextAnchor { _xPct = 0, _yPct = 0.5 }
    , _tickAnchor = Pct 0.5
    , _anchorOffset = D.V2 margin 0
    }

labelRightAbove :: InternalFloat -> InternalFloat -> Label -> Label
labelRightAbove marginX marginY label = label
    { _textAnchor = TextAnchor { _xPct = 0, _yPct = 0 }
    , _tickAnchor = FromTopAbs 0
    , _anchorOffset = D.V2 marginX marginY
    }

-- Rendering settings
data RenderSettings = RenderSettings
    { heightMultiplier :: InternalFloat
    , textMultiplier :: InternalFloat
    , padding :: InternalFloat
    , lineWidth :: InternalFloat
    , xPow :: Int
    , yPow :: Int
    }

instance Default RenderSettings where
  def = RenderSettings
    { heightMultiplier = 1
    , textMultiplier = 1
    , padding = 0
    , lineWidth = 0
    , xPow = 3
    , yPow = 3
    }
