module PicoUI

import public Control.Monad.Free
import public Control.MonadRec
import Control.Monad.Trans

data WidgetF : (viewdsl : Type) -> (eventdsl : Type) -> (eff : Type -> Type) -> (a : Type) -> Type where
  Pure     : List viewdsl -> a -> WidgetF viewdsl eventdsl eff a
  Effect   : eff a -> WidgetF viewdsl eventdsl eff a
  Event    : eventdsl -> a -> WidgetF viewdsl eventdsl eff a

Functor eff => Functor (WidgetF viewdsl eventdsl eff) where
  map f (Pure view a) = Pure view (f a)
  map f (Effect effa) = Effect (map f effa)
  map f (Event ev a) = Event ev (f a)

public export
data Widget : (viewdsl : Type) -> (eventdsl : Type) -> (eff : Type -> Type) -> (a : Type) -> Type where
  MkWidget : Free (WidgetF viewdsl eventdsl eff) a -> Widget viewdsl eventdsl eff a

unwrapWidget : Widget viewdsl eventdsl eff a -> Free (WidgetF viewdsl eventdsl eff) a
unwrapWidget (MkWidget a) = a

effectWidget : eff a -> Widget viewdsl eventdsl eff a
effectWidget effa = MkWidget . lift $ Effect effa

export
Functor (Widget viewdsl eventdsl eff) where
  map f (MkWidget a) = MkWidget (f <$> a)

export
Applicative (Widget viewdsl eventdsl eff) where
  pure = MkWidget . pure
  (<*>) (MkWidget f) (MkWidget a) = MkWidget (f <*> a)

export
Monad (Widget viewdsl eventdsl eff) where
  (>>=) (MkWidget a) f = MkWidget (a >>= unwrapWidget . f)

export
MonadTrans (Widget viewdsl eventdsl) where
  lift = effectWidget

export
voidWidget : Widget viewdsl eventdsl eff ()
voidWidget = MkWidget . lift $ Pure [] ()

export
pureWidget : List viewdsl -> a -> Widget viewdsl eventdsl eff a
pureWidget viewdsl a = MkWidget . lift $ Pure viewdsl a

export
eventWidget : (ev : eventdsl) -> Widget viewdsl eventdsl eff ()
eventWidget ev = MkWidget . lift $ Event ev ()

export
runWidget : 
  Applicative eff => MonadRec eff => 
  (List viewdsl -> eff ()) ->         -- Render function
  (eventdsl -> eff Bool) ->           -- Event processing function
  Widget viewdsl eventdsl eff a ->    -- UI widget
  eff a
runWidget viewdslf eventdslf = run . unwrapWidget

  where

  run : Free (WidgetF viewdsl eventdsl eff) a -> eff a
  run = runM $ \case
    Pure viewdsl a => do
      viewdslf viewdsl
      pure a
    Effect effa => effa
    Event ev a => do
      r <- eventdslf ev
      pure a

export
launchWidget : Applicative eff => MonadRec eff => (List viewdsl -> eff ()) -> (eventdsl -> eff Bool) -> Widget viewdsl eventdsl eff a -> eff ()
launchWidget viewdslf eventdslf w = do
  () <$ runWidget viewdslf eventdslf w
  launchWidget viewdslf eventdslf w
