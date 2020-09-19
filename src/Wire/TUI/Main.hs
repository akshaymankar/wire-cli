module Wire.TUI.Main where

import qualified Brick as Brick
import Brick.Forms (Form)
import qualified Brick.Forms as Form
import Data.Text (Text)
import qualified Graphics.Vty as Vty

data State = State {t :: Text}

type Event = ()

type Name = ()

defaultName :: ()
defaultName = ()

app :: Brick.App State Event Name
app =
  Brick.App
    { Brick.appDraw = const [Form.renderForm loginForm],
      Brick.appChooseCursor = const . const Nothing,
      Brick.appHandleEvent = handleEvent,
      Brick.appStartEvent = pure,
      Brick.appAttrMap = \_ -> Brick.attrMap Vty.defAttr []
    }

loginWidget :: Brick.Widget ()
loginWidget =
  Brick.Widget
    { Brick.hSize = Brick.Greedy,
      Brick.vSize = Brick.Greedy,
      Brick.render = (pure (Brick.emptyResult {Brick.image = loginImage}))
    }

handleEvent :: State -> Brick.BrickEvent Name Event -> Brick.EventM Name (Brick.Next State)
handleEvent s = \case
  Brick.VtyEvent (Vty.EvKey (Vty.KChar 'q') []) -> Brick.halt s
  _ -> Brick.continue s

loginImage :: Vty.Image
loginImage = Vty.string Vty.defAttr "welcome to wire-tui"

loginForm :: Form State Event Name
loginForm = Form.newForm [\s -> Form.editTextField something defaultName (Just 20) s] (State "foo")

something :: Functor f => (Text -> f Text) -> State -> f State
something f s = State <$> (f (t s))
