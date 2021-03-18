{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |

module Utils.NameResolver (getCname, isPrim) where

import RIO
import Types.Types (Name)
import RIO.Text
import qualified Unbound.Generics.LocallyNameless as Un
import Data.List as L
import Data.Maybe as M

prims = [
  -- Type predicates
  ("cons?", "consp", 1), ("null?", "nullp", 1), ("list?", "listp", 1), ("vector?", "vectorp", 1), ("procedure?", "procedurep", 1), ("number?", "numberp", 1),

  ("char?", "charp", 1), ("boolean?", "booleanp", 1), ("symbol?", "symbolp", 1), ("string?", "stringp", 1), ("integer?", "integerp", 1),
  -- Object Equality
  ("eq?", "eqp", 2), ("eqv?", "eqp", 2), ("equal?", "equalp", 2),
  -- Conversion
  ("string->symbol", "string2symbol", 1), ("symbol->string", "symbol2string", 1), ("list->vector", "list2vector", 1), ("vector->list", "vector2list", 1),
  -- Chars
  ("chareq?", "chareqp", 2), ("char<?", "charltp", 2), ("char>?", "chargtp", 2), ("char<=?", "charltoep", 2), ("char>=?", "chargtoep", 2),
  -- Numbers
  ("=", "eqsign", 1), ("<", "ltsign", 1), ("<=", "ltoesign", 1), (">", "gtsign", 1), (">=", "gtoesign", 1), ("+", "plus", 42), ("*", "times", 42), ("-", "minus1", 1), ("/", "division1", 1),
  -- String
  ("make-string", "make_string", 43), ("string-length", "string_length", 1), ("string-ref", "string_ref", 2), ("string-set!", "string_set", 3),
  -- Cons
  ("cons", "cons", 2), ("car", "car", 1), ("cdr", "cdr", 1), ("length", "length", 1), ("set-car!", "set_car", 1), ("set-cdr!", "set_cdr", 1),
  -- Vector
  ("make-vector", "make_vector",43), ("vector-length", "vector_length", 1), ("vector-ref", "vector_ref", 2), ("vector-set!", "vector_set", 3),
  -- Printing
  ("display", "display", 1)
  ]

class IsVarName a where
  getName :: a -> Text

instance IsVarName String where
  getName a = pack a

instance IsVarName Text where
  getName = id

instance IsVarName Name where
  getName = getName . Un.name2String

isPrim :: (IsVarName a) => a -> Bool
isPrim a = L.any ((getName a ==) . (\(n, _, _) -> getName n)) prims

getCname :: String -> String
getCname str = M.fromJust $ do
  (_, cname, _) <- L.find ((str==) . (\(n, _, _) -> n)) prims
  return cname
