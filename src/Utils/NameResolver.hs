{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |

module Utils.NameResolver (getCname, isPrim, isOverloaded, isVariadic, primsAndAritys) where

import RIO
import RIO.ByteString
import Types.Types (Name, PrimName(..), UniqName (UName))
import Data.List as L
import Data.Maybe as M

type Aritys = [Int]

prims :: [(Name, Name, Aritys)]
prims = [
  -- Type predicates
  ("cons?", "consp", [1]), ("null?", "nullp", [1]), ("list?", "listp", [1]), ("vector?", "vectorp", [1]), ("procedure?", "procedurep", [1]), ("number?", "numberp", [1]),

  ("char?", "charp", [1]), ("boolean?", "booleanp", [1]), ("symbol?", "symbolp", [1]), ("string?", "stringp", [1]), ("integer?", "integerp", [1]),
  -- Object Equality
  ("eq?", "eqp", [2]), ("eqv?", "eqp", [2]), ("equal?", "equalp", [2]),
  -- Conversion
  ("string->symbol", "string2symbol", [1]), ("symbol->string", "symbol2string", [1]), ("list->vector", "list2vector", [1]), ("vector->list", "vector2list", [1]),
  -- Chars
  ("chareq?", "chareqp", [2]), ("char<?", "charltp", [2]), ("char>?", "chargtp", [2]), ("char<=?", "charltoep", [2]), ("char>=?", "chargtoep", [2]),
  -- Numbers
  ("=", "eqsign", [1]), ("<", "ltsign", [1]), ("<=", "ltoesign", [1]), (">", "gtsign", [1]), (">=", "gtoesign", [1]), ("+", "plus", [0,1]), ("*", "times", [0,1]), ("-", "minus1", [1]), ("/", "division1", [1]),
  -- String
  ("make-string", "make_string", [1,2]), ("string-length", "string_length", [1]), ("string-ref", "string_ref", [2]), ("string-set!", "string_set", [3]),
  -- Cons
  ("cons", "cons", [2]), ("car", "car", [1]), ("cdr", "cdr", [1]), ("length", "length", [1]), ("set-car!", "set_car", [1]), ("set-cdr!", "set_cdr", [1]),
  -- Vector
  ("make-vector", "make_vector",[1,2]), ("vector-length", "vector_length", [1]), ("vector-ref", "vector_ref", [2]), ("vector-set!", "vector_set", [3]),
  -- Printing
  ("display", "display", [1])
  ]

varargs = ["+" , "-", "*", "/", "=", ">", ">=", "<", "<="]

overloaded = ["+", "*", "make-string", "make-vector"]


class IsVarName a where
  getName :: a -> Name

instance IsVarName Name where
  getName = id

instance IsVarName UniqName where
  getName (UName n _) = n

instance IsVarName Text where
  getName = encodeUtf8

isPrim :: (IsVarName a) => a -> Bool
isPrim a = L.any ((getName a ==) . (\(n, _, _) -> getName n)) prims

isOverloaded :: PrimName -> Bool
isOverloaded (PName (sn, _)) = sn `L.elem` overloaded

isVariadic :: PrimName -> Bool
isVariadic (PName (sn, _)) = sn `L.elem` varargs

getCname :: ByteString -> ByteString
getCname str = head $ do
  (sname, cname, _) <- prims
  if sname == str || cname == str then
    return cname
  else []

primsAndAritys :: [(ByteString, Aritys)]
primsAndAritys = fmap (\(_, snd, thd)-> (snd, thd)) prims
