module Helpers exposing (defaultTrue, flip)


flip : (a -> b -> c) -> b -> a -> c
flip fn b a =
    fn a b


defaultTrue : (a -> Bool) -> Maybe a -> Bool
defaultTrue testFn maybeFilter =
    Maybe.map testFn maybeFilter |> Maybe.withDefault True
