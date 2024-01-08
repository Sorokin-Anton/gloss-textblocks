{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Graphics.Gloss
import Graphics.Gloss.TextBlocks (simpleTextBlock, robotoMono)
import Data.String
import Graphics.Gloss.Interface.IO.Game
import qualified Data.Map as M
import Data.IORef
import SDL.Font (Font)


data Model = Model {str :: String, scalar :: Float}

update :: Event -> Model -> IO Model
update (EventKey (Char c) Down _ _) m = pure m{str = str m <> [c]}
update (EventKey (SpecialKey KeyUp) Down _ _) m = pure m{scalar = scalar m * 1.2}
update (EventKey (SpecialKey KeyDown) Down _ _) m = pure m{scalar = scalar m / 1.2 }
update _ x = pure x

mkDrawerWithCaching :: Font ->  IO (String -> IO Picture)
mkDrawerWithCaching font = do
  ref <- newIORef mempty
  pure $ \s -> do
    m <- readIORef ref
    case m M.!? s of
      Just k -> pure k
      Nothing -> do
        pic <- snd <$> simpleTextBlock font (fromString s)
        modifyIORef' ref  (M.insert s pic)
        pure pic

main :: IO ()
main = do
  font <- robotoMono 40
  drawer <- mkDrawerWithCaching font
  let draw Model{str, scalar} = scale scalar scalar <$> drawer str
  playIO FullScreen white 60 (Model (['а'..'я'] <> "\n" <> ['А'..'Я']<> "\n" <> ['a'..'z']) 1) draw update (const pure)
