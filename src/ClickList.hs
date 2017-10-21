module Main where

import           Control.Monad      (void)

import           Concur.Core        (Widget)
import           Concur.VDOM        (HTML, button, el, el_, initConcur,
                                     runWidgetInBody)
import qualified GHCJS.VDOM.Element as E

slowList :: [Int] -> Widget HTML Int
slowList = el E.div [] . fmap buttonize
  where
    buttonize n = el_ E.div [] $ do
      button $ show n
      return n

main :: IO ()
main = void $ do
  initConcur
  runWidgetInBody $ slowList [1..5000]
