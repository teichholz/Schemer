-- | AST for Scheme expressions

module Ast where
import SParser

data Expr =
  Ident String
  | App (Expr, [Expr])
  deriving (Show, Eq, Ord)



parse_expression :: Sexp -> Expr
parse_expression (Atom ident) = Ident ident

parse_expression (List (hd:tl)) = undefined
