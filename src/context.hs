module Context where

import Data.List
import Control.Alternative

type Label = String
data State a = State [a]
data Context a = Context [Decl] a

instance Functor Context where
    fmap f (Context state x) = Context (f x) state

getLabel :: Declaration a => State a -> Label -> Maybe a
getLabel (State decls) l = find (label . (l ==)) decls

class Declaration a where
    label :: a -> Label
    -- value for variable
    -- partial expression where the only variable left is the argument
    resolve :: a -> State -> b


