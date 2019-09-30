module CIS194.Homework07.Exercise01 ( (+++) ) where

import CIS194.Homework07.JoinList ( JoinList(..) )

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) = mappend
