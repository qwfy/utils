module Open
    ( (|>)
    , fromJust
    , isJust
    , collectJust
    ) where

import Data.Maybe (fromJust, isJust)

infixl 0 |>
x |> f = f x

collectJust :: [Maybe a] -> [a]
collectJust xs =
    xs |> filter isJust |> map fromJust
