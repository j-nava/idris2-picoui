module PicoCSS

import Generics.Derive
import public PicoUI

PicoCSSPtr : Type
PicoCSSPtr = AnyPtr

%foreign """
  browser:lambda:async (__p1, __p2, ty, id, callback) => {
    let snabbdom = await import('../../node_modules/snabbdom/build/index.js');
    const patch = snabbdom.init([
      // Init patch function with chosen modules
      snabbdom.classModule, // makes it easy to toggle classes
      snabbdom.propsModule, // for setting properties on DOM elements
      snabbdom.styleModule, // handles styling on elements with support for animations
      snabbdom.eventListenersModule, // attaches event listeners
    ]);
    const peek = (arr) => arr[arr.length - 1];
    const update = (arr, f) => {
      arr[arr.length - 1] = f(arr[arr.length - 1]);
      return arr;
    };
    const add = (arr, d) => update(arr, (a) => { a.push(d); return a; });
    const container = document.getElementById(id);
    let events = [];
    let elements = [];
    elements.push([]);
    return {
      patch: (vnode) => patch(container, vnode),
      peek: () => peek(elements),
      update: (f) => update(elements, f),
      add: (d) => add(elements, d),
      container: container,
      elements: elements,
      h: snabbdom.h,
      id: id,
      ty: ty,
      events: events,
      callback: (picoCSS) => { callback(picoCSS)()(); }
    };
  }
  """
prim__picoCSSInit : HasIO m => String -> String -> (PicoCSSPtr -> m ()) -> PrimIO PicoCSSPtr

jsFun : String -> String -> String
jsFun params body = 
  "browser:lambda:async (picoCSS_async" ++
  (if params == "" then "" else ("," ++ params)) ++
  ") => {const picoCSS = await picoCSS_async;" ++
  body ++
  "}"

%foreign jsFun "" """
    const vnode = picoCSS.h(picoCSS.ty, { id: picoCSS.id }, picoCSS.peek());
    picoCSS.patch(vnode);
  """
prim__render : PicoCSSPtr -> PrimIO ()

%foreign jsFun "" """
    picoCSS.elements.push([]);
  """
prim__begin : PicoCSSPtr -> PrimIO ()

%foreign jsFun "tag" """
    const children = picoCSS.peek();
    picoCSS.elements.pop();
    picoCSS.add(picoCSS.h(tag, {}, children));
  """
prim__end : PicoCSSPtr -> String -> PrimIO ()

%foreign jsFun "heading,text" "picoCSS.add(picoCSS.h(heading, text));"
prim__heading : PicoCSSPtr -> String -> String -> PrimIO ()

%foreign jsFun "text" "picoCSS.add(picoCSS.h('span', text));"
prim__text : PicoCSSPtr -> String -> PrimIO ()

%foreign jsFun "id, caption" "picoCSS.add(picoCSS.h('button', { on: { click: () => { picoCSS.events.push('gg'); picoCSS.callback(picoCSS); } } }, caption));"
prim__button : PicoCSSPtr -> (id : Int) -> (caption : String) -> PrimIO ()

%foreign jsFun "" """
    const eventCount = picoCSS.events.length;
    picoCSS.events = [];
    return eventCount > 0;
  """
prim__checkEvents : PicoCSSPtr -> PrimIO Bool

public export
data ItemId = Id String

export
data ElementType = Root | Leaf | Element | GridColumn

public export
data Heading = Normal | H1 | H2 | H3 | H4 | H5 | H6 | Small

public export
data PicoCSS : Type where
  Header : PicoCSS
  Main   : PicoCSS
  Footer : PicoCSS

  Container : PicoCSS
  ContainerFluid : PicoCSS

  Grid : PicoCSS
  GridCol : PicoCSS

  Text : Heading -> String -> PicoCSS

  Button : ItemId -> String -> PicoCSS

public export
data PicoCSSEvent
  = None
  | ButtonClick ItemId

export
header : Widget PicoCSS PicoCSSEvent m () -> Widget PicoCSS PicoCSSEvent m ()
header w = do
  widget Open Header
  w
  widget Close Header

export
main : Widget PicoCSS PicoCSSEvent m () -> Widget PicoCSS PicoCSSEvent m ()
main w = do
  widget Open Main
  w
  widget Close Main

export
footer : Widget PicoCSS PicoCSSEvent m () -> Widget PicoCSS PicoCSSEvent m ()
footer w = do
  widget Open Footer
  w
  widget Close Footer

export
text : Heading -> String -> Widget PicoCSS PicoCSSEvent m ()
text heading t = widget Normal (Text heading t)

export
button : ItemId -> String -> Widget PicoCSS PicoCSSEvent m (List PicoCSSEvent)
button itemId caption = eventfulWidget Normal (Button itemId caption) [(ButtonClick itemId)]

export
runPicoCSS : MonadRec m => HasIO m => Widget PicoCSS PicoCSSEvent m () -> m ()
runPicoCSS ui = do
  picoCSS <- primIO (prim__picoCSSInit "body" "app" eventCallback)
  run picoCSS

  where

  uiEvent : PicoCSSEvent -> m Bool
  uiEvent = \case
    None => pure False
    ButtonClick (Id id) => do
      pure True

  mutual
    uiElem : PicoCSSPtr -> WidgetType -> PicoCSS -> m ()
    uiElem picoCSS ty = \case
      Header => case ty of
        Open => primIO $ prim__begin picoCSS
        Close => primIO $ prim__end picoCSS "header"
        _ => pure ()
      Footer => case ty of
        Open => primIO $ prim__begin picoCSS
        Close => primIO $ prim__end picoCSS "footer"
        _ => pure ()
      Main => case ty of
        Open => primIO $ prim__begin picoCSS
        Close => primIO $ prim__end picoCSS "main"
        _ => pure ()
      Text heading text => case heading of
        H1 => primIO $ prim__heading picoCSS "h1" text
        H2 => primIO $ prim__heading picoCSS "h2" text
        H3 => primIO $ prim__heading picoCSS "h3" text
        H4 => primIO $ prim__heading picoCSS "h4" text
        H5 => primIO $ prim__heading picoCSS "h5" text
        H6 => primIO $ prim__heading picoCSS "h6" text
        Small => primIO $ prim__heading picoCSS "small" text
        Normal => primIO $ prim__text picoCSS text
      Button (Id id) caption => primIO $ prim__button picoCSS 1 caption
      -- FormField title => do
      --   primIO $ prim__formField title
      --   pure ()
      -- FormSubmit title => do
      --   -- uiButton title
      --   pure ()
      _ => pure ()

    -- uiElems : PicoCSSPtr -> List (Item a) -> m ()
    -- uiElems picoCSS = traverse_ (uiElem picoCSS)

  run : PicoCSSPtr -> m ()
  run picoCSS = do
    events <- primIO $ prim__checkEvents picoCSS
    putStrLn (show events)
    launchWidget (uiElem picoCSS) uiEvent ui
    primIO $ prim__render picoCSS

  eventCallback : PicoCSSPtr -> m ()
  eventCallback picoCSS = do
    putStrLn "CALLBACK"
    run picoCSS
