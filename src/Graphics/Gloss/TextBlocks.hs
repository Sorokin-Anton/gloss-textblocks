{-# LANGUAGE TemplateHaskell #-}
module Graphics.Gloss.TextBlocks where
import Graphics.Gloss (Picture)
import Data.Text
import SDL.Font
import Data.FileEmbed (embedFile)
import Graphics.Gloss.SDL.Surface
import SDL

robotoMono :: PointSize -> IO Font
robotoMono n = do
  SDL.Font.initialize
  decode $(embedFile "static/RobotoMono-Medium.ttf") n


simpleTextBlock :: Font -> Text -> IO ((Float, Float), Picture)
simpleTextBlock f t = bitmapOfSurface NoCache =<< blended f (V4 0 0 0 255) t
