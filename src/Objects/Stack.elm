module Objects.Stack exposing (mkSizedStack, Stack, pop, push, StackError, MkStackError, toList, toArray)

import Array exposing (Array)


type Stack a
    = Stack Int (Array a)


type MkStackError
    = NonPositiveSizedStackErr


type StackError
    = PushedToFullStack
    | PoppedFromEmptyStack


mkSizedStack : Int -> Result MkStackError (Stack a)
mkSizedStack size =
    if size > 0 then
        Ok <| Stack size Array.empty

    else
        Err NonPositiveSizedStackErr


pop : Stack a -> Result StackError (Stack a)
pop (Stack size array) =
    let
        currSize =
            Array.length array
    in
    if currSize >= 0 then
        Ok <| Stack size <| Array.slice 0 currSize array

    else
        Err <| PoppedFromEmptyStack


push : a -> Stack a -> Result StackError (Stack a)
push element (Stack size array) =
    let
        currSize =
            Array.length array
    in
    if currSize < size then
        Ok <| Stack size <| Array.push element array

    else
        Err <| PushedToFullStack

toList : Stack a -> List a
toList (Stack _ array) = Array.toList array

toArray : Stack a -> Array a
toArray (Stack _ array) = array