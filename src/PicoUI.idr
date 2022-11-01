module PicoUI

import public Control.Monad.Free
import public Control.MonadRec
import Control.Monad.Trans
import Control.Monad.State

public export
data WidgetType = Normal | Open | Close

data WidgetF : (viewdsl : Type) -> (eventdsl : Type) -> (eff : Type -> Type) -> (a : Type) -> Type where
  Effect   : eff a -> WidgetF viewdsl eventdsl eff a
  Eventful : WidgetType -> viewdsl -> List eventdsl -> a -> WidgetF viewdsl eventdsl eff a
  Pure     : WidgetType -> viewdsl -> a -> WidgetF viewdsl eventdsl eff a

Functor eff => Functor (WidgetF viewdsl eventdsl eff) where
  map f (Effect effa) = Effect (map f effa)
  map f (Eventful ty view ev a) = Eventful ty view ev (f a)
  map f (Pure ty view a) = Pure ty view (f a)

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

-- export
-- voidWidget : Widget viewdsl eventdsl eff ()
-- voidWidget = MkWidget . lift $ Pure [] ()

export
widget : WidgetType -> viewdsl -> Widget viewdsl eventdsl eff ()
widget ty viewdsl = MkWidget . lift $ Pure ty viewdsl ()

export
eventfulWidget : WidgetType -> viewdsl -> List eventdsl -> Widget viewdsl eventdsl eff (List eventdsl)
eventfulWidget ty viewdsl events = MkWidget . lift $ Eventful ty viewdsl events []

export
container : viewdsl -> viewdsl -> Widget viewdsl eventdsl eff () -> Widget viewdsl eventdsl eff ()
container openw closew w = do
  widget Open openw
  w
  widget Close closew

-- export
-- event : (ev : eventdsl) -> Widget viewdsl eventdsl eff Bool
-- event ev = MkWidget . lift $ Event ev False

export
runWidget : 
  Applicative eff => MonadRec eff => 
  (WidgetType -> viewdsl -> eff ()) ->              -- Render function
  (eventdsl -> eff Bool) ->           -- Event processing function
  Widget viewdsl eventdsl eff a ->    -- UI widget
  eff (Bool, a)
runWidget viewdslf eventdslf w = runStateT False ((run . unwrapWidget) w)

  where

  run : Free (WidgetF viewdsl eventdsl eff) a -> StateT Bool eff a
  run = runM $ \case
    Pure ty viewdsl a => do
      lift (viewdslf ty viewdsl)
      pure a
    Effect effa => lift effa
    Eventful ty viewdsl events a => do
      lift (viewdslf ty viewdsl)
      pure a

export
launchWidget : Applicative eff => MonadRec eff => (WidgetType -> viewdsl -> eff ()) -> (eventdsl -> eff Bool) -> Widget viewdsl eventdsl eff a -> eff ()
launchWidget viewdslf eventdslf w = do
  result <- runWidget viewdslf eventdslf w
  -- if result is true, launchWidget recursively
  pure ()
  -- launchWidget viewdslf eventdslf w
