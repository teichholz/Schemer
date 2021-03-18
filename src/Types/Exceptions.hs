{-# LANGUAGE ExistentialQuantification #-}
-- |

module Types.Exceptions where

import RIO
import Data.Typeable (cast)


-------------------------------------------------------------------------------
-- Root exception
-------------------------------------------------------------------------------
data ScException =
  forall e . Exception e => ScException e

instance Show ScException where
  show (ScException e) = show e

instance Exception ScException

compilerExceptionToException :: Exception e => e -> SomeException
compilerExceptionToException = toException . ScException

compilerExceptionFromException :: Exception e => SomeException -> Maybe e
compilerExceptionFromException x = do
    ScException a <- fromException x
    cast a

-------------------------------------------------------------------------------
-- Parse exceptions
-------------------------------------------------------------------------------
newtype ParseException = ParseException String
    deriving Show

instance Exception ParseException where
    toException   = compilerExceptionToException
    fromException = compilerExceptionFromException
