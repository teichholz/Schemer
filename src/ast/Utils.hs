-- |

module Utils where
import Data.List as L
import Data.Maybe as M
import Data.IORef as IOR

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

is_prim :: String -> Bool
is_prim str = L.any ((str==) . (\(n, _, _) -> n)) prims

get_cname :: String -> String
get_cname str = M.fromJust $ do
  (_, cname, _) <- L.find ((str==) . (\(_, cn, _) -> cn)) prims
  return cname

gensym :: IORef Int -> IO String
gensym counter = do
  count <- IOR.readIORef counter
  IOR.modifyIORef counter (+1)
  return $ "gen" <> show count
