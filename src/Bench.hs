{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE ExtendedDefaultRules    #-}
{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE Strict         #-}
module Main where

import           Control.Applicative    ((<|>))
import           Control.Monad          (forever, void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State    (StateT, execStateT, get, lift, put)

import Data.JSString (JSString)
import qualified Data.JSString                as JSS

import qualified GHCJS.VDOM.Element as E
import qualified GHCJS.VDOM.Attribute as A

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Data.Monoid ((<>))

import           System.Random          as R
import           Text.Read              (readMaybe)

import           Concur.Core
import           Concur.VDOM

data RowData = RowData
  { rowIdx :: Int
  , rowTitle :: JSString
  } deriving (Show, Eq)

data Model = Model
  { modelRows :: V.Vector RowData
  , modelHighlightedRowIndex :: Maybe Int
  , modelLastIdx :: Int
  } deriving (Show, Eq)

type Widget' a = StateT Model (Widget HTML) ()

data Action = CreateRows Int
            | AppendRows Int
            | UpdateRows Int
            | ClearRows
            | SwapRows
            | HighlightRow Int
            | RemoveRow Int
            deriving (Show, Eq)

adjectives :: V.Vector JSString
adjectives = V.fromList ["pretty", "large", "big", "small", "tall", "short", "long", "handsome",
                         "plain", "quaint", "clean", "elegant", "easy", "angry", "crazy", "helpful",
                         "mushy", "odd", "unsightly", "adorable", "important", "inexpensive",
                         "cheap", "expensive", "fancy"];

colours :: V.Vector JSString
colours = V.fromList ["red", "yellow", "blue", "green", "pink", "brown", "purple", "brown", "white", "black", "orange"];

nouns :: V.Vector JSString
nouns = V.fromList ["table", "chair", "house", "bbq", "desk", "car", "pony", "cookie", "sandwich", "burger", "pizza", "mouse", "keyboard"];

initModel :: Model
initModel = Model{modelRows=V.empty, modelHighlightedRowIndex=Nothing, modelLastIdx=1}

main :: IO ()
main = void $ do
  initConcur
  runWidgetInBody $ flip execStateT initModel $ forever $ do
    m <- get
    axn <- lift $ viewModel m
    m' <- lift $ updateModel axn m
    put m'

updateModel :: Action -> Model -> Widget HTML Model

updateModel (CreateRows n) model@Model{modelLastIdx=lastIdx} = loadWithIO [E.div [A.class_ "loading"] [E.text "LOADING..."]] $ do
  newRows <- generateRows n lastIdx
  return model{modelRows=(newRows), modelLastIdx=(lastIdx + n)}

updateModel (AppendRows n) model@Model{modelRows=existingRows, modelLastIdx=lastIdx} = loadWithIO [E.div [A.class_ "loading"] [E.text "LOADING..."]] $ do
  newRows <- generateRows n (modelLastIdx model)
  return model{modelRows=(existingRows V.++ newRows), modelLastIdx=(lastIdx + n)}

updateModel (ClearRows) model = return model{modelRows=V.empty}

updateModel (UpdateRows n) model@Model{modelRows=currentRows} = return model{modelRows=updatedRows}
  where
    updatedRows = V.accumulate
      (\r@RowData{rowTitle=rt} s -> r{rowTitle=(rt <> s)})
      currentRows
      (V.generate (quot (V.length currentRows) n) (\x -> (x*n, JSS.pack " !!!")))

updateModel SwapRows model@Model{modelRows=currentRows} = return $
  if (V.length currentRows >=10)
  then model{modelRows=swappedRows}
  else model
  where
    swappedRows = V.modify (\v -> MV.swap v 4 9) currentRows

updateModel (HighlightRow idx) model = return model{modelHighlightedRowIndex=Just idx}

updateModel (RemoveRow idx) model@Model{modelRows=currentRows} = return model{modelRows=(firstPart V.++ (V.drop 1 remainingPart))}
  where (firstPart, remainingPart) = V.splitAt idx currentRows

generateRows :: Int -> Int -> IO (V.Vector RowData)
generateRows n lastIdx = V.generateM n $ \x -> do
  adjIdx <- R.randomRIO (0, (V.length adjectives) - 1)
  colorIdx <- R.randomRIO (0, (V.length colours) - 1)
  nounIdx <- R.randomRIO (0, (V.length nouns) - 1)
  pure RowData{rowIdx=(lastIdx + x), rowTitle=(adjectives V.! adjIdx) <> " " <> (colours V.! colorIdx) <> " " <> (nouns V.! nounIdx)}

viewModel :: Model -> Widget HTML Action
viewModel m = el E.div [A.id "main"]
  [
    el E.div [A.class_ "container"]
    [
      viewJumbotron
    , viewTable m
    , el E.span [A.class_ "preloadicon glyphicon glyphicon-remove"] []
    ]
  ]

viewTable :: Model -> Widget HTML Action
viewTable m@Model{modelHighlightedRowIndex=idx} =
  el E.table [A.class_ "table table-hover table-striped test-data"]
  [
    el E.tbody [A.id "tbody"] (V.toList $ V.imap viewRow (modelRows m))
  ]
  where
    conditionalDanger i = if (Just i==idx) then [A.class_ "danger", A.key i] else [A.key i]
    viewRow i r = el E.tr (conditionalDanger i)
      [
        el E.td [A.class_ "col-md-1"] [text (show (rowIdx r))]
      , el E.td [A.class_ "col-md-4"]
        [
          const (HighlightRow i) <$> el_ E.span [A.class_ "lbl"] (text $ JSS.unpack $ rowTitle r)
        ]
      , el E.td [A.class_ "col-md-1"]
        [
          el E.a [A.class_ "remove"]
          [
           const (RemoveRow i) <$> button' [A.class_ "glyphicon glyphicon-remove remove"] "X"
          ]
        ]
      , el E.td [A.class_ "col-md-6"] []
      ]

viewJumbotron :: Widget HTML Action
viewJumbotron =
  el E.div [A.class_ "jumbotron"]
  [
    el E.div [A.class_ "row"]
    [
      el E.div [A.class_ "col-md-6"]
      [
        el E.h1 [] [text "Concur - VDOM - Benchmark"]
      ]
    , el E.div [A.class_ "col-md-6"]
      [
        el E.div [A.class_ "row"]
        [
          el E.div [A.class_ "col-sm-6 smallpad"]
          [
            const (CreateRows 10) <$> button' [A.class_ "btn btn-primary btn-block", A.id "run"] "Create 10 rows"
          ]
        , el E.div [A.class_ "col-sm-6 smallpad"]
          [
            const (CreateRows 100) <$> button' [A.class_ "btn btn-primary btn-block", A.id "run"] "Create 100 rows"
          ]
        , el E.div [A.class_ "col-sm-6 smallpad"]
          [
            const (CreateRows 1000) <$> button' [A.class_ "btn btn-primary btn-block", A.id "run"] "Create 1,000 rows"
          ]
        , el E.div [A.class_ "col-sm-6 smallpad"]
          [
            const (CreateRows 10000) <$> button' [A.class_ "btn btn-primary btn-block", A.id "runlots"] "Create 10,000 rows"
          ]
        , el E.div [A.class_ "col-sm-6 smallpad"]
          [
            const (AppendRows 1000) <$> button' [A.class_ "btn btn-primary btn-block", A.id "add"] "Add 1,000 rows"
          ]
        , el E.div [A.class_ "col-sm-6 smallpad"]
          [
            const (UpdateRows 10) <$> button' [A.class_ "btn btn-primary btn-block", A.id "update"] "Update every 10th row"
          ]
        , el E.div [A.class_ "col-sm-6 smallpad"]
          [
            const ClearRows <$> button' [A.class_ "btn btn-primary btn-block", A.id "clear"] "Clear"
          ]
        , el E.div [A.class_ "col-sm-6 smallpad"]
          [
            const SwapRows <$> button' [A.class_ "btn btn-primary btn-block", A.id "swaprows"] "Swap rows"
          ]
        ]
      ]
    ]
  ]
