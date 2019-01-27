module SyntaxTree where

type Variable = String
type Atom = String
data Term = Wildcard | Var Variable | Compose Atom [Term]
type Clause = (Term, [Term])
type Clauses = [Clause]
type Program = Clauses
type Goal = [Term]

instance Show Term where
  show Wildcard       = "_"
  show (Var v)        = v
  show (Compose f []) = f
  show (Compose f ts) = f ++ "(" ++ (foldr (++) [] (map show ts)) ++ ")"
