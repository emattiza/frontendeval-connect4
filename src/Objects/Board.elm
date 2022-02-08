module Objects.Board exposing
    ( Board
    , DiskPushError
    , MkBoardError
    , columnInBoard
    , columns
    , newBoard
    , pushDisk
    )

import Array exposing (Array)
import List exposing (range)
import Objects.Disk exposing (Disk)
import Objects.Stack as Stack exposing (Stack, mkSizedStack)
import Set


type Point
    = Point Int Int


type Board
    = Board Int Int (Array (Stack Disk))


type MkBoardError
    = StackError Stack.MkStackError
    | BoardNonPositiveError


type DiskPushError
    = StackFull Stack.StackError
    | StackNotThere Int


newBoard : Int -> Int -> Result MkBoardError Board
newBoard cols height =
    if cols >= 1 then
        case mkSizedStack height of
            Ok stack ->
                Ok <| Board cols height <| Array.repeat cols stack

            Err err ->
                Err <| StackError err

    else
        Err <| BoardNonPositiveError


columns : Board -> Int
columns (Board cols _ _) =
    cols


columnInBoard : Board -> Int -> Result DiskPushError Board
columnInBoard board column =
    let
        availableColumns =
            Set.fromList <| range 1 (columns board)
    in
    if Set.member column availableColumns then
        Ok board

    else
        Err <| StackNotThere column


pushDisk : Disk -> Int -> Board -> Result DiskPushError Board
pushDisk disk column board =
    case columnInBoard board column of
        Ok (Board width height array) ->
            case Array.get column array of
                Just col ->
                    let
                        freshStack =
                            Stack.push disk col
                    in
                    case freshStack of
                        Ok stack ->
                            Ok <| Board width height <| Array.set column stack array

                        Err err ->
                            Err <| StackFull err

                Nothing ->
                    Err <| StackNotThere column

        Err err ->
            Err err


isBoardWinning : Board -> Maybe Disk
isBoardWinning (Board width height array) =
    let
        gridBoard =
            Array.map Stack.toArray array

        getGridref x y =
            Maybe.andThen
                (\stack -> Array.get y stack)
                (Array.get x gridBoard)

        allPoints =
            List.map2 Point (range 0 width) (range 0 height)

        allDisks =
            List.map (\(Point i j) -> getGridref i j) allPoints
    in
    Nothing
