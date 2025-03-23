port module Port exposing (..)

port setFavorite : String -> Cmd msg
port removeFavorite : String -> Cmd msg
port getFavorites : () -> Cmd msg

port receiveFavorites : (List String -> msg) -> Sub msg

port sendTTS : String -> Cmd msg

port toggleDialog : () -> Cmd msg
