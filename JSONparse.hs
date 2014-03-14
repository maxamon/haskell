import Text.JSON
import Text.JSON.Generic
import qualified Data.List as L

data DataValues = CoordValues {
    xValue :: Int
} | ObjectValues {
    idValue :: Int
} deriving (Eq, Show, Data, Typeable)

data Command = Command {
    cmd :: String,
    dataValues :: DataValues
} deriving (Eq, Show, Data, Typeable)

toJSO :: [(String, JSValue)] -> JSValue
toJSO = JSObject . toJSObject

instance JSON DataValues where
    showJSON (CoordValues xv yv h w) = toJSO [("xValue", showJSON xv)]
    showJSON (ObjectValues s)  = toJSO [("idValue",  showJSON s)]
    readJSON (JSObject obj)    = case o of
      [("xValue", JSRational _ xv)] -> Ok CoordValues {xValue = floor xv}
      [("idValue", JSString s)] -> Ok ObjectValues  {idValue = getInt s}
      _ -> Error "Wrong dataValues"
      where
        o = fromJSObject obj
        getInt s = read (fromJSString s)::Int
    readJSON (_) = Error "Wrong dataValues: Internal error"

instance JSON Command where
    showJSON (Command s d) = toJSO [("cmd", showJSON s), ("dataValues",  showJSON d)]
    readJSON (JSObject obj) = case o of
      [("cmd", JSString s), ("dataValues", obj')] -> formResp s obj'
      _ -> Error "Wrong command"
      where
        o = fromJSObject obj
        formResp s _obj = Ok Command {
                cmd = fromJSString s, 
                dataValues = case readJSON _obj of
                                (Ok res) -> res
                                (Error err) -> WrongValues {errorText = err}
                }
    readJSON (_) = Error "Wrong command: Internal error"
