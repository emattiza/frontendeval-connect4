module Board exposing (..)

import Array exposing (Array)
import Disk exposing (Disk)
import Stack exposing (Stack, mkSizedStack)


type Board
    = Board (Array (Stack Disk))


type MkBoardError
    = StackError Stack.MkStackError
    | BoardNonPositiveError


newBoard : Int -> Int -> Result MkBoardError Board
newBoard columns height =
    if columns >= 1 then
        case mkSizedStack height of
            Ok stack ->
                Ok <| Board <| Array.repeat columns stack

            Err err ->
                Err <| StackError err

    else
        Err <| BoardNonPositiveError
