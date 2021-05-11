module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Bits
import Data.Char
import Data.Word
import Disguise.Cairo
import Disguise.Gtk.Main
import Disguise.Gtk.Event
import System.Environment

main :: IO ()
main = do
  args <- getArgs

  case args of
    (contentType : codestring : []) -> displayCodestring codestring
    _ -> ioError (userError "Invalid arguments. Expected:\n\n  flicker text/x-flicker <flickercode>")

fromJust' :: String -> Maybe a -> IO a
fromJust' err Nothing = ioError (userError ("Error: " ++ err))
fromJust' err (Just x) = return x

displayCodestring :: String -> IO ()
displayCodestring codestring = do
  nibbles <- fromJust' "Invalid flickercode" $ parseNibbles $ "0FFF" ++ codestring

  style <- defaultStyle

  let endlessNibbles = concat (repeat nibbles)
  run style processor (syncIO (initModel endlessNibbles) updateModel widget) ClockEvent

parseNibbles :: String -> Maybe [Word8]
parseNibbles (a:b:xs) = do
  a' <- parseNibble a
  b' <- parseNibble b
  rest <- parseNibbles xs
  return $ b' : a' : rest
parseNibbles [] = Just []
parseNibbles _ = Nothing

parseNibble :: Char -> Maybe Word8
parseNibble = parseNibble' . toLower
  where
    parseNibble' '0' = Just 0
    parseNibble' '1' = Just 1
    parseNibble' '2' = Just 2
    parseNibble' '3' = Just 3
    parseNibble' '4' = Just 4
    parseNibble' '5' = Just 5
    parseNibble' '6' = Just 6
    parseNibble' '7' = Just 7
    parseNibble' '8' = Just 8
    parseNibble' '9' = Just 9
    parseNibble' 'a' = Just 10
    parseNibble' 'b' = Just 11
    parseNibble' 'c' = Just 12
    parseNibble' 'd' = Just 13
    parseNibble' 'e' = Just 14
    parseNibble' 'f' = Just 15
    parseNibble' _ = Nothing

-- GUI

data Model = Model
  { modelNibbles :: [Word8]
  , modelClock :: Integer
  , modelScaling :: Double
  , modelInterval :: Integer
  }

data MyEvent = GtkEvent Event | ClockEvent

processor :: Processor MyEvent
processor = (GtkEvent <$> keyProcessor) <> clockProcessor

clockProcessor :: Processor MyEvent
clockProcessor = Processor $ \window handler -> do
  forkIO $ forever $ do
    threadDelay 500
    handler ClockEvent
  return ()

initModel :: [Word8] -> Model
initModel nibbles = Model
  { modelNibbles = nibbles
  , modelClock = 0
  , modelScaling = 1
  , modelInterval = 20
  }

clockBit :: Integer -> Integer -> Bool
clockBit clock interval = even (clock `div` interval)

updateModel :: MyEvent -> Model -> IO Model
updateModel ClockEvent model =
  let clock = modelClock model + 1
  in if not (clockBit (modelClock model) (modelInterval model)) && clockBit clock (modelInterval model)
     then return $ model { modelClock = clock, modelNibbles = tail (modelNibbles model) }
     else return $ model { modelClock = clock }
updateModel (GtkEvent (KeyEvent keyval)) model = case keyName keyval of
  "Up" -> return $ model { modelScaling = modelScaling model + 0.1 }
  "Down" -> return $ model { modelScaling = modelScaling model - 0.1 }
  "Left" -> return $ model { modelInterval = modelInterval model + 10 }
  "Right" -> return $ model { modelInterval = max (modelInterval model - 10) 10 }
  _ -> return model

widget :: Model -> IO (CairoWidget (V Dim) (V Dim) (StyleT IO))
widget model =
  let nibble = head (modelNibbles model)
      currentBits =
        ( clockBit (modelClock model) (modelInterval model)
        , nibble `testBit` 0
        , nibble `testBit` 1
        , nibble `testBit` 2
        , nibble `testBit` 3
        )
      scaling = modelScaling model
  in return $ alignLeft $ alignTop $ pad 10 $ box $ pad 10 $
       fixh (scaling * 80) $ fixw (scaling * 160) $ stretchV $ stretchH $ ui currentBits

ui :: (Bool, Bool, Bool, Bool, Bool) -> CairoWidget (F Dim) (F Dim) (StyleT IO)
ui (b1,b2,b3,b4,b5) = flickercode
  where
    flickercode = flicker b1 `leftOf` flicker b2 `leftOf` flicker b3 `leftOf` flicker b4 `leftOf` flicker b5
    flicker True = withStyling reverseColors $ fill (fixh 3 $ fixw 1 $ space)
    flicker False = fill (fixh 3 $ fixw 1 $ space)
