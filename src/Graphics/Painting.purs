-- | This module defines a type `Painting` for creating vector graphics.
module Graphics.Painting
  ( Point
  , Shape
  , MeasurableText
  , ImageDataTransform
  , ImageDataRep
  , path
  , closed
  , rectangle
  , circle
  , arc
  , FillStyle
  , fillColor
  , OutlineStyle
  , ImageSource(..)
  , CanvasComposite(..)
  , outlineColor
  , lineWidth
  , Shadow
  , shadowOffset
  , shadowBlur
  , shadowColor
  , shadow
  , composite
  , drawImage
  , drawImageScale
  , drawImageFull
  , pushPixels
  , pushPixelsFull
  , Painting
  , filled
  , outlined
  , clipped
  , scale
  , translate
  , rotate
  , text
  , everywhere
  , measurableTextToMetrics
  , render
  ) where

import Prelude
import Color (Color, cssStringHSLA, toHexString)
import Control.Alt ((<|>))
import Data.Foldable (class Foldable, for_)
import Data.List (List(..), singleton, (:), fromFoldable)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Foreign.Object (Object, lookup)
import Graphics.Canvas (CanvasImageSource, Context2D, ImageData, setGlobalCompositeOperation)
import Graphics.Canvas as Canvas
import Graphics.Drawing (Font)
import Graphics.Drawing.Font (fontString)
import Math (pi)
import Web.HTML (HTMLCanvasElement, HTMLImageElement, HTMLVideoElement)
import Web.HTML.HTMLMediaElement (setCurrentTime)
import Web.HTML.HTMLVideoElement (toHTMLMediaElement)

-- | A `Point` consists of `x` and `y` coordinates.
type Point
  = { x :: Number, y :: Number }

-- | A single shape.
data Shape
  -- | A path is a list of points joined by line segments
  = Path Boolean (List Point)
  -- | A rectangle consisting of the numbers left, top, width and height
  | Rectangle Canvas.Rectangle
  -- | A circular arc consisting of the numbers center-x, center-y, start angle, end angle and radius
  | Arc Canvas.Arc
  -- | A composite shape
  | Composite (List Shape)

derive instance eqShape :: Eq Shape

instance semigroupShape :: Semigroup Shape where
  append (Composite ds) d = Composite (ds <> singleton d)
  append d (Composite ds) = Composite (d : ds)
  append d1 d2 = Composite (Cons d1 (Cons d2 Nil))

instance monoidShape :: Monoid Shape where
  mempty = Composite mempty

-- | Create a path.
path :: forall f. (Foldable f) => f Point -> Shape
path = Path false <<< fromFoldable

-- | Create a _closed_ path.
closed :: forall f. (Foldable f) => f Point -> Shape
closed = Path true <<< fromFoldable

-- | Create a rectangle from the left, top, width and height parameters.
rectangle :: Number -> Number -> Number -> Number -> Shape
rectangle x y width height = Rectangle { x, y, width, height }

-- | Create a circle from the left, top and radius parameters.
circle :: Number -> Number -> Number -> Shape
circle x y = arc x y 0.0 (pi * 2.0)

-- | Create a circular arc from the left, top, start angle, end angle and
-- | radius parameters.
arc :: Number -> Number -> Number -> Number -> Number -> Shape
arc x y start end radius = Arc { x, y, start, end, radius }

data CanvasComposite
  -- Composite Operations
  = SourceOver
  | SourceIn
  | SourceOut
  | SourceAtop
  | DestinationOver
  | DestinationIn
  | DestinationOut
  | DestinationAtop
  | Lighter
  | Copy
  | Xor
  -- Blend Modes
  | Multiply
  | Screen
  | Overlay
  | Darken
  | Lighten
  | ColorDodge
  | ColorBurn
  | HardLight
  | SoftLight
  | Difference
  | Exclusion
  | Hue
  | Saturation
  | Color
  | Luminosity

derive instance canvasCompositeEq :: Eq CanvasComposite

cchack :: CanvasComposite -> Canvas.Composite
cchack SourceOver = Canvas.SourceOver

cchack SourceIn = Canvas.SourceIn

cchack SourceOut = Canvas.SourceOut

cchack SourceAtop = Canvas.SourceAtop

cchack DestinationOver = Canvas.DestinationOver

cchack DestinationIn = Canvas.DestinationIn

cchack DestinationOut = Canvas.DestinationOut

cchack DestinationAtop = Canvas.DestinationAtop

cchack Lighter = Canvas.Lighter

cchack Copy = Canvas.Copy

cchack Xor = Canvas.Xor

cchack Multiply = Canvas.Multiply

cchack Screen = Canvas.Screen

cchack Overlay = Canvas.Overlay

cchack Darken = Canvas.Darken

cchack Lighten = Canvas.Lighten

cchack ColorDodge = Canvas.ColorDodge

cchack ColorBurn = Canvas.ColorBurn

cchack HardLight = Canvas.HardLight

cchack SoftLight = Canvas.SoftLight

cchack Difference = Canvas.Difference

cchack Exclusion = Canvas.Exclusion

cchack Hue = Canvas.Hue

cchack Saturation = Canvas.Saturation

cchack Color = Canvas.Color

cchack Luminosity = Canvas.Luminosity

instance showCanvasComposite :: Show CanvasComposite where
  show = show <<< cchack

data PatternRepeat
  = Repeat
  | RepeatX
  | RepeatY
  | NoRepeat

derive instance eqPatternRepeat :: Eq PatternRepeat

prhack :: PatternRepeat -> Canvas.PatternRepeat
prhack Repeat = Canvas.Repeat

prhack RepeatX = Canvas.RepeatX

prhack RepeatY = Canvas.RepeatY

prhack NoRepeat = Canvas.NoRepeat

data ImageSource
  = FromImage { name :: String }
  | FromVideo { name :: String, currentTime :: Maybe Number }
  | FromCanvas { name :: String }

derive instance eqImageSource :: Eq ImageSource

data Pattern
  = Pattern ImageSource PatternRepeat

derive instance eqPattern :: Eq Pattern

-- | Encapsulates fill color etc.
data FillStyle
  = ColorFill Color
  | GradientFill Gradient
  | PatternFill Pattern

type GradientColorStop
  = { color :: Color, position :: Number }

data Gradient
  = LinearGradient
    { x0 :: Number
    , y0 :: Number
    , x1 :: Number
    , y1 :: Number
    }
    (List GradientColorStop)
  | RadialGradient
    { x0 :: Number
    , y0 :: Number
    , r0 :: Number
    , x1 :: Number
    , y1 :: Number
    , r1 :: Number
    }
    (List GradientColorStop)

derive instance eqGradient :: Eq Gradient

derive instance eqFillStyle :: Eq FillStyle

-- | Set the fill color.
fillColor :: Color -> FillStyle
fillColor = ColorFill

-- | Encapsulates outline color etc.
newtype OutlineStyle
  = OutlineStyle
  { color :: Maybe Color
  , lineWidth :: Maybe Number
  }

-- | Set the outline color.
outlineColor :: Color -> OutlineStyle
outlineColor c = OutlineStyle { color: Just c, lineWidth: Nothing }

-- | Set the line width.
lineWidth :: Number -> OutlineStyle
lineWidth c = OutlineStyle { color: Nothing, lineWidth: Just c }

instance semigroupOutlineStyle :: Semigroup OutlineStyle where
  append (OutlineStyle f1) (OutlineStyle f2) =
    OutlineStyle
      { color: f1.color <|> f2.color
      , lineWidth: f1.lineWidth <|> f2.lineWidth
      }

instance monoidOutlineStyle :: Monoid OutlineStyle where
  mempty =
    OutlineStyle
      { color: Nothing
      , lineWidth: Nothing
      }

derive instance eqOutlineStyle :: Eq OutlineStyle

-- | Encapsulates shadow settings etc.
newtype Shadow
  = Shadow
  { color :: Maybe Color
  , blur :: Maybe Number
  , offset :: Maybe { x :: Number, y :: Number }
  }

derive instance eqShadow :: Eq Shadow

-- | Set the shadow color.
shadowColor :: Color -> Shadow
shadowColor c = Shadow { color: Just c, blur: Nothing, offset: Nothing }

-- | Set the shadow blur.
shadowBlur :: Number -> Shadow
shadowBlur b = Shadow { color: Nothing, blur: Just b, offset: Nothing }

-- | Set the shadow blur.
shadowOffset :: Number -> Number -> Shadow
shadowOffset x y = Shadow { color: Nothing, blur: Nothing, offset: Just { x: x, y: y } }

instance semigroupShadow :: Semigroup Shadow where
  append (Shadow s1) (Shadow s2) =
    Shadow
      { color: s1.color <|> s2.color
      , blur: s1.blur <|> s2.blur
      , offset: s1.offset <|> s2.offset
      }

instance monoidShadow :: Monoid Shadow where
  mempty =
    Shadow
      { color: Nothing
      , blur: Nothing
      , offset: Nothing
      }

-- | Apply a scale transformation by providing the x and y scale factors.
scaleMeasurableText :: Number -> Number -> MeasurableText -> MeasurableText
scaleMeasurableText sx sy = MTScale { scaleX: sx, scaleY: sy }

-- | Apply a translation by providing the x and y distances.
translateMeasurableText :: Number -> Number -> MeasurableText -> MeasurableText
translateMeasurableText tx ty = MTTranslate { translateX: tx, translateY: ty }

-- | Apply a rotation by providing the angle.
rotateMeasurableText :: Number -> MeasurableText -> MeasurableText
rotateMeasurableText = MTRotate

-- | Render some text.
textMeasurableText :: Font -> String -> MeasurableText
textMeasurableText = MTText

newtype ImageDataTransform
  = ImageDataTransform (ImageDataRep -> ImageDataRep)

type ImageDataRep
  = { width :: Number, height :: Number, pixels :: Array Int }

pushPixels :: Number -> Number -> Number -> Number -> (ImageDataRep -> ImageDataRep) -> Number -> Number -> Painting -> Painting
pushPixels a b c d e = PushPixels a b c d (ImageDataTransform e)

pushPixelsFull :: Number -> Number -> Number -> Number -> (ImageDataRep -> ImageDataRep) -> Number -> Number -> Number -> Number -> Number -> Number -> Painting -> Painting
pushPixelsFull a b c d e = PushPixelsFull a b c d (ImageDataTransform e)

-- give up once we get here...
instance eqImageDataTransform :: Eq ImageDataTransform where
  eq a b = true

data Painting
  = Fill Shape FillStyle
  | Outline Shape OutlineStyle
  | Text Font Number Number FillStyle String
  | Many (List Painting)
  | DrawImage ImageSource Number Number
  | DrawImageScale ImageSource Number Number Number Number
  | DrawImageFull ImageSource Number Number Number Number Number Number Number Number
  | Scale { scaleX :: Number, scaleY :: Number } Painting
  | Translate { translateX :: Number, translateY :: Number } Painting
  | Rotate Number Painting
  | Clipped Shape Painting
  | PushPixels Number Number Number Number ImageDataTransform Number Number Painting
  | PushPixelsFull Number Number Number Number ImageDataTransform Number Number Number Number Number Number Painting
  | WithShadow Shadow Painting
  | WithComposite CanvasComposite Painting

data MeasurableText
  = MTText Font String
  | MTScale { scaleX :: Number, scaleY :: Number } MeasurableText
  | MTTranslate { translateX :: Number, translateY :: Number } MeasurableText
  | MTRotate Number MeasurableText

derive instance eqMeasurableText :: Eq MeasurableText

instance ordMeasurableText :: Ord MeasurableText where
  compare (MTTranslate { translateX: sx0, translateY: sy0 } f0) (MTTranslate { translateX: sx1, translateY: sy1 } f1) = let c0 = compare sx0 sx1 in if c0 == EQ then let c1 = compare sy0 sy1 in if c1 == EQ then compare f0 f1 else c1 else c0
  compare (MTTranslate { translateX: sx0, translateY: sy0 } _) _ = LT
  compare (MTScale { scaleX: sx0, scaleY: sy0 } f0) (MTScale { scaleX: sx1, scaleY: sy1 } f1) = let c0 = compare sx0 sx1 in if c0 == EQ then let c1 = compare sy0 sy1 in if c1 == EQ then compare f0 f1 else c1 else c0
  compare (MTScale { scaleX: sx0, scaleY: sy0 } _) _ = LT
  compare (MTRotate n0 f0) (MTRotate n1 f1) = let c0 = compare n0 n1 in if c0 == EQ then compare f0 f1 else c0
  compare (MTRotate n _) _ = LT
  compare (MTText f0 s0) (MTText f1 s1) = let c0 = compare (fontString f0) (fontString f1) in if c0 == EQ then compare s0 s1 else c0
  compare (MTText f0 s0) _ = LT

instance semigroupPainting :: Semigroup Painting where
  append (Many ds) d = Many (ds <> singleton d)
  append d (Many ds) = Many (d : ds)
  append d1 d2 = Many (Cons d1 (Cons d2 Nil))

instance monoidPainting :: Monoid Painting where
  mempty = Many mempty

derive instance eqPainting :: Eq Painting

-- | Fill a `Shape`.
filled :: FillStyle -> Shape -> Painting
filled = flip Fill

-- | Draw the outline of a `Shape`.
outlined :: OutlineStyle -> Shape -> Painting
outlined = flip Outline

-- | Clip a `Painting` to a `Shape`.
clipped :: Shape -> Painting -> Painting
clipped = Clipped

-- | Apply a `Shadow` to a `Painting`.
shadow :: Shadow -> Painting -> Painting
shadow = WithShadow

composite :: CanvasComposite -> Painting -> Painting
composite = WithComposite

drawImage :: ImageSource -> Number -> Number -> Painting
drawImage = DrawImage

drawImageScale :: ImageSource -> Number -> Number -> Number -> Number -> Painting
drawImageScale = DrawImageScale

drawImageFull :: ImageSource -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Painting
drawImageFull = DrawImageFull

-- | Apply a scale transformation by providing the x and y scale factors.
scale :: Number -> Number -> Painting -> Painting
scale sx sy = Scale { scaleX: sx, scaleY: sy }

-- | Apply a translation by providing the x and y distances.
translate :: Number -> Number -> Painting -> Painting
translate tx ty = Translate { translateX: tx, translateY: ty }

-- | Apply a rotation by providing the angle.
rotate :: Number -> Painting -> Painting
rotate = Rotate

-- | Render some text.
text :: Font -> Number -> Number -> FillStyle -> String -> Painting
text = Text

-- | Modify a `Painting` by applying a transformation to every sub-painting.
everywhere :: (Painting -> Painting) -> Painting -> Painting
everywhere f = go
  where
  go (Many ds) = f (Many (map go ds))

  go (Scale s d) = f (Scale s (go d))

  go (Translate t d) = f (Translate t (go d))

  go (Rotate r d) = f (Rotate r (go d))

  go (Clipped s d) = f (Clipped s (go d))

  go (PushPixels a b c d fn a' b' p) = f (PushPixels a b c d fn a' b' $ f p)

  go (PushPixelsFull a b c d fn a' b' c' d' e' f' p) = f (PushPixelsFull a b c d fn a' b' c' d' e' f' $ f p)

  go (WithShadow s d) = f (WithShadow s (go d))

  go (WithComposite s d) = f (WithComposite s (go d))

  go other = f other

renderMeasurableText :: Canvas.Context2D -> MeasurableText -> Effect Canvas.TextMetrics
renderMeasurableText ctx = go
  where
  go (MTScale s d) =
    Canvas.withContext ctx do
      _ <- Canvas.scale ctx s
      go d

  go (MTTranslate t d) =
    Canvas.withContext ctx do
      _ <- Canvas.translate ctx t
      go d

  go (MTRotate r d) =
    Canvas.withContext ctx do
      _ <- Canvas.rotate ctx r
      go d

  go (MTText font s) =
    Canvas.withContext ctx do
      _ <- Canvas.setFont ctx (fontString font)
      Canvas.measureText ctx s

foreign import htmlCanvasElemntToImageSource :: HTMLCanvasElement -> Effect CanvasImageSource

foreign import htmlVideoElemntToImageSource :: HTMLVideoElement -> Effect CanvasImageSource

foreign import htmlImageElemntToImageSource :: HTMLImageElement -> Effect CanvasImageSource

type ImageSources
  = { canvases :: Object HTMLCanvasElement
    , images :: Object HTMLImageElement
    , videos :: Object HTMLVideoElement
    }

imageSourcesToImageSource :: ImageSources -> ImageSource -> Effect (Maybe CanvasImageSource)
imageSourcesToImageSource sources = go
  where
  go (FromImage { name }) =
    let
      img = lookup name sources.images
    in
      case img of
        Nothing -> pure Nothing
        Just x -> Just <$> htmlImageElemntToImageSource x

  go (FromCanvas { name }) =
    let
      img = lookup name sources.canvases
    in
      case img of
        Nothing -> pure Nothing
        Just x -> Just <$> htmlCanvasElemntToImageSource x

  go (FromVideo { name, currentTime }) =
    let
      img = lookup name sources.videos
    in
      case img of
        Nothing -> pure Nothing
        Just v -> do
          for_ currentTime (flip setCurrentTime (toHTMLMediaElement v))
          Just <$> htmlVideoElemntToImageSource v

foreign import newImageData :: Context2D -> ImageData -> ImageDataRep -> Effect ImageData

foreign import imageDataToRep :: ImageData -> Effect ImageDataRep

measurableTextToMetrics ::
  Canvas.Context2D ->
  List MeasurableText ->
  Effect (Map MeasurableText Canvas.TextMetrics)
measurableTextToMetrics ctx =
  map Map.fromFoldable
    <<< sequence
    <<< map \i -> Tuple i <$> renderMeasurableText ctx i

-- | Render a `Painting` to a canvas.
render ::
  Canvas.Context2D ->
  ImageSources ->
  Painting ->
  Effect Unit
render ctx sources = go
  where
  go (Fill sh style) =
    void
      $ Canvas.withContext ctx do
          applyFillStyle style
          Canvas.fillPath ctx
            $ renderShape sh

  go (Outline sh style) =
    void
      $ Canvas.withContext ctx do
          applyOutlineStyle style
          Canvas.strokePath ctx
            $ renderShape sh

  go (Many ds) = for_ ds go

  go (Scale s d) =
    void
      $ Canvas.withContext ctx do
          _ <- Canvas.scale ctx s
          go d

  go (Translate t d) =
    void
      $ Canvas.withContext ctx do
          _ <- Canvas.translate ctx t
          go d

  go (Rotate r d) =
    void
      $ Canvas.withContext ctx do
          _ <- Canvas.rotate ctx r
          go d

  go (Clipped sh d) =
    void
      $ Canvas.withContext ctx do
          renderShape sh
          _ <- Canvas.clip ctx
          go d

  go (WithShadow sh d) =
    void
      $ Canvas.withContext ctx do
          applyShadow sh
          go d

  go (WithComposite cc d) =
    void
      $ Canvas.withContext ctx do
          setGlobalCompositeOperation ctx (cchack cc)
          go d

  go (Text font x y style s) =
    void
      $ Canvas.withContext ctx do
          _ <- Canvas.setFont ctx (fontString font)
          applyFillStyle style
          Canvas.fillText ctx s x y

  go (DrawImage isrc a b) =
    void
      $ Canvas.withContext ctx do
          src <- imageSourcesToImageSource sources isrc
          for_ src \src' -> Canvas.drawImage ctx src' a b

  go (DrawImageScale isrc a b c d) =
    void
      $ Canvas.withContext ctx do
          src <- imageSourcesToImageSource sources isrc
          for_ src \src' -> Canvas.drawImageScale ctx src' a b c d

  go (PushPixels a b c d (ImageDataTransform fn) a' b' p) =
    void
      $ Canvas.withContext ctx do
          go p
          imgData <- Canvas.getImageData ctx a b c d
          asRep <- imageDataToRep imgData
          imgData' <- newImageData ctx imgData (fn asRep)
          Canvas.putImageData ctx imgData' a' b'

  go (PushPixelsFull a b c d (ImageDataTransform fn) a' b' c' d' e' f' p) =
    void
      $ Canvas.withContext ctx do
          go p
          imgData <- Canvas.getImageData ctx a b c d
          asRep <- imageDataToRep imgData
          imgData' <- newImageData ctx imgData (fn asRep)
          Canvas.putImageDataFull ctx imgData' a' b' c' d' e' f'

  go (DrawImageFull isrc a b c d e f g h) =
    void
      $ Canvas.withContext ctx do
          src <- imageSourcesToImageSource sources isrc
          for_ src \src' -> Canvas.drawImageFull ctx src' a b c d e f g h

  applyShadow :: Shadow -> Effect Unit
  applyShadow (Shadow s) = do
    for_ s.color \color -> Canvas.setShadowColor ctx (cssStringHSLA color)
    for_ s.blur \blur -> Canvas.setShadowBlur ctx blur
    for_ s.offset \offset -> do
      _ <- Canvas.setShadowOffsetX ctx offset.x
      Canvas.setShadowOffsetY ctx offset.y

  applyFillStyle :: FillStyle -> Effect Unit
  applyFillStyle (ColorFill color) = Canvas.setFillStyle ctx (cssStringHSLA color)

  applyFillStyle (GradientFill (LinearGradient info stops)) = do
    gdt <- Canvas.createLinearGradient ctx info
    void $ sequence
      $ map
          ( \{ color, position } ->
              Canvas.addColorStop gdt position (toHexString color)
          )
          stops
    Canvas.setGradientFillStyle ctx gdt

  -- todo: merge with above?
  applyFillStyle (GradientFill (RadialGradient info stops)) = do
    gdt <- Canvas.createRadialGradient ctx info
    void $ sequence
      $ map
          ( \{ color, position } ->
              Canvas.addColorStop gdt position (toHexString color)
          )
          stops
    Canvas.setGradientFillStyle ctx gdt

  applyFillStyle (PatternFill (Pattern isrc repeat)) = do
    src <- imageSourcesToImageSource sources isrc
    for_ src \img -> do
      pattern <- Canvas.createPattern ctx img (prhack repeat)
      Canvas.setPatternFillStyle ctx pattern

  applyOutlineStyle :: OutlineStyle -> Effect Unit
  applyOutlineStyle (OutlineStyle fs) = do
    for_ fs.color $ \color -> Canvas.setStrokeStyle ctx (cssStringHSLA color)
    for_ fs.lineWidth $ \width -> Canvas.setLineWidth ctx width

  renderShape :: Shape -> Effect Unit
  renderShape (Path _ Nil) = pure unit

  renderShape (Path cl (Cons p rest)) = do
    _ <- Canvas.moveTo ctx p.x p.y
    for_ rest \pt -> Canvas.lineTo ctx pt.x pt.y
    when cl $ void $ Canvas.closePath ctx

  renderShape (Rectangle r) = void $ Canvas.rect ctx r

  renderShape (Arc a) = void $ Canvas.arc ctx a

  renderShape (Composite ds) = for_ ds renderShape
