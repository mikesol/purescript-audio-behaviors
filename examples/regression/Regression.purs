-- useful for regression testing
module FRP.Behavior.Audio.Example.Regression where

import Prelude
import Data.Array (fold)
import Data.List (List(..))
import Data.Map as M
import Data.Maybe (fromMaybe)
import Data.NonEmpty ((:|))
import Data.Profunctor (lcmap)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (D2)
import Effect (Effect)
import Effect.Class.Console (log)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioParameter, AudioUnit, Exporter, Run, defaultExporter, evalPiecewise, g'add_, g'delay_, g'gain_, graph_, playBufWithOffset_, runInBrowser, speaker)
import Type.Data.Graph (SNil, type (:/))
import Type.Proxy (Proxy(..))

sounds =
  [ Tuple 105 1.3107256235827665
  , Tuple 104 1.1829251700680272
  , Tuple 102 1.2127891156462585
  ] ::
    Array (Tuple Int Number)

kr = (20.0) / 1000.0 :: Number

epwf :: Array (Tuple Number Number) -> Number -> AudioParameter
epwf = evalPiecewise kr

fromCloud :: String -> String
fromCloud s = "https://klank-share.s3-eu-west-1.amazonaws.com/in-a-sentimental-mood/Samples/" <> s

fromSounds :: Int -> Number
fromSounds i = fromMaybe 0.0 (M.lookup i soundsMap)

soundsMap :: M.Map Int Number
soundsMap = M.fromFoldable sounds

type PlayerSenOpts
  = { tag :: String
    }

atT :: forall a. Number -> (Number -> a) -> (Number -> a)
atT t = lcmap (_ - t)

playerSen :: Int -> (Number -> PlayerSenOpts) -> Number -> List (AudioUnit D2)
playerSen name' opts' time =
  if time + kr >= 0.0 && time < len then
    pure
      $ ( graph_ (opts.tag <> "_graph")
            { aggregators:
                { out: Tuple (g'add_ (opts.tag <> "_out")) (Proxy :: Proxy ("combine" :/ SNil))
                , combine: Tuple (g'add_ (opts.tag <> "_cbn")) (Proxy :: Proxy ("gain" :/ "senn" :/ SNil))
                , gain: Tuple (g'gain_ (opts.tag <> "_gnlp") 0.4) (Proxy :: Proxy ("del" :/ SNil))
                }
            , processors:
                { del: Tuple (g'delay_ (opts.tag <> "_dl") 0.5) (Proxy :: Proxy "combine")
                }
            , generators:
                { senn:
                    (playBufWithOffset_ (opts.tag <> "_playerSen") "moo" 1.0 0.0)
                }
            }
        )
  else
    Nil
  where
  len = (fromSounds name')

  opts = opts' len

  name = "Sen-B4-" <> show name' <> "-l"

data SenInfo
  = SenInfo Int Number

playerSen_ :: Int -> (Number -> PlayerSenOpts) -> Number -> Behavior (AudioUnit D2)
playerSen_ name opts time = pure $ speaker (zero :| playerSen name opts time)

senSpread :: Number -> String -> Array (Number → List (AudioUnit D2))
senSpread os tg =
  map
    ( \(SenInfo x y) ->
        ( atT (y + os)
            $ playerSen x
                ( \l ->
                    { tag: tg <> "sen" <> (show x) <> (show y)
                    }
                )
        )
    )
    [ SenInfo 105 0.0
    , SenInfo 104 0.0
    , SenInfo 102 0.6
    ]

scene :: Number -> Behavior (AudioUnit D2)
scene time =
  pure
    $ speaker
        ( zero
            :| fold
                ( map ((#) time)
                    ( (senSpread 1.0 "A")
                    )
                )
        )

run :: Run Unit Unit
run = runInBrowser scene

exporter =
  defaultExporter
    { use = \_ a -> log $ show a.audio
    } ::
    Exporter Unit Unit

main :: Effect Unit
main = pure unit
