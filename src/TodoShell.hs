module TodoShell where

import Brick
import Brick.BChan
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as BL
import ClassyPrelude hiding (on)
import Control.Concurrent
import Control.Monad
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Graphics.Vty as V

data Tick = Tick

data Name
  = HorizontalBox
  | VerticalBox
  | VerticalList
  deriving (Eq, Show, Ord)

app :: App () Tick Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const theMap
    }

drawUI :: () -> [Widget Name]
drawUI _ =
  [ vBox
      [ C.hCenter heading,
        C.hCenter listOfItems
      ]
  ]

listOfItems :: Widget Name
listOfItems =
  withAttr verticalAttrName $
    hLimitPercent 70 $
      viewport VerticalBox Vertical $ vBox $ map renderListItem myData

-- BL.renderList renderListItem True (BL.list VerticalList myData 2)

renderListItem :: (Int, Char) -> Widget Name
renderListItem (index, c) =
  C.hCenter $
    padTopBottom 2 $
      padLeftRight 5 $
        hBox [str $ show index, str " - ", str [c]]

-- renderListItem :: Bool -> (Int, Char) -> Widget Name
-- renderListItem b (index, c) = C.hCenter $ hBox [str $ show index, str [c]]

heading :: Widget Name
heading =
  withAttr horizontalAttrName $
    withBorderStyle BS.unicodeBold $
      padLeftRight 10 $
        padTopBottom 2 $ hBox [txt "TodoShell"]

horizontalAttrName :: AttrName
horizontalAttrName = attrName "HorizontalBox"

verticalAttrName :: AttrName
verticalAttrName = attrName "VerticalBox"

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (horizontalAttrName, V.blue `on` V.white),
      (verticalAttrName, V.white `on` V.blue)
    ]

handleEvent :: () -> BrickEvent Name Tick -> EventM Name (Next ())
handleEvent () (VtyEvent (V.EvKey V.KEsc [])) = halt ()
handleEvent () (VtyEvent (V.EvKey V.KUp [])) = do
  let vp = viewportScroll VerticalBox
  vScrollBy vp 1
  continue ()
handleEvent () (VtyEvent (V.EvKey V.KDown [])) = do
  let vp = viewportScroll VerticalBox
  vScrollBy vp (-1)
  continue ()
handleEvent _ _ = continue ()

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $
    forever $ do
      writeBChan chan Tick
      threadDelay 100000
  let g = ()
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) app g

myData :: [(Int, Char)]
myData = zip [1 ..] ['A' .. 'Z']

myDataSeq :: Seq (Int, Char)
myDataSeq = Seq.fromList myData