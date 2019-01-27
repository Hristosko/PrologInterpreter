module Interpreter
    (
    module Parser,
    Solution,
    findSolutions,
    printSolution
    ) where

{-
  Module Interpreter.
  Implements an interpreter for prolog.
-}

import Parser
import Control.Monad

type Substitution = [(Variable, Term)]

--
-- checks if the term has a variable with the same name as the given one
--
hasVariable :: Variable -> Term -> Bool
hasVariable _ Wildcard = False
hasVariable v (Var t) = t == v
hasVariable v (Compose _ ts) = any (hasVariable v) ts

--
-- applies the substitution to the term
--
substitute :: Substitution -> Term -> Term
substitute _   Wildcard     = Wildcard
substitute sub (Var x) = case lookup x sub of
  Just y  -> y
  Nothing -> (Var x)
substitute sub (Compose a ts) = Compose a (map (substitute sub) ts)

--
-- returns the composition of the two substitutions
-- This is neccessary as the second one can override the first one.
-- For exmple X->Y and Y->a, we should obtain X->a, Y->a
--
composeSubstitutions :: Substitution -> Substitution -> Substitution
composeSubstitutions sub1 sub2 = (map (\(v, t) -> (v, substitute sub2 t)) sub1) ++ sub2

--
-- unification algorithm.
-- Returns Nothing if there is no possible unification.
-- If thre is one, returns the substitution that has to be made
-- in the first term to obtain the second one.
--
unify :: Term -> Term -> Maybe Substitution
unify Wildcard        _                                 = Just []
unify _               Wildcard                          = Just []
unify (Var x)         (Var y)   | x == y                = Just []
unify (Var x)         t         | not (hasVariable x t) = Just [(x, t)]
unify t               (Var x)                           = unify (Var x) t
unify (Compose x xs)  (Compose y ys) | x == y           = unifyLists xs ys
unify _               _                                 = Nothing

unifyLists :: [Term] -> [Term] -> Maybe Substitution
unifyLists [] [] = Just []
unifyLists (x:xs) (y:ys) = case unify x y of
  Just sub -> case unifyLists (map (substitute sub) xs) (map (substitute sub) ys) of
    Just sub' -> Just $ composeSubstitutions sub sub'
    Nothing   -> Nothing
  Nothing  -> Nothing

--
-- The solution is a substitution that has to be applied to the variables in the
-- initial goal.
--
data Solution = Solution Substitution

--
-- clear print for the solution.
-- X = t
-- where X is a variable in the initial goal and t is the result.
-- Note X = X is a valid result, should be interpreted as "For all values of X"
--
instance Show Solution where
  show (Solution [])             = ""
  show (Solution ((v,t) : sols)) = v ++ " = " ++ show t ++ "\n" ++ show (Solution sols)

--
-- Finds all variables in the goal and initializes a solutions "For All"
--
initSolution :: Goal -> Solution
initSolution g = Solution $ foldr (++) [] (map initSolution' g)
  where
    initSolution' Wildcard = []
    initSolution' (Var v)  = [(v, Var v)]
    initSolution' (Compose _ ts) = foldr (++) [] (map initSolution' ts)

--
-- Adds new substitution to the current solution.
-- Simply applies the substitution to all results in the current solution.
--
addToSolution :: Solution -> Substitution -> Solution
addToSolution (Solution sol) sub = Solution $ map (addToSolution' sub) sol
  where
    addToSolution' s (v,t) = (v, (substitute s t))

--
-- search tree for the resolution
-- Each Leaf contains a valid solution.
--
data SLDTree = Leaf Solution | Node [SLDTree] deriving (Show)

--
-- Buils a search tree.
--
solve :: Program -> Solution -> Goal -> [SLDTree]
solve _    _   []   = []
solve []   _   _    = []
solve prog sol goal = foldr (++) [] (map solve' prog)
  where
    solve' t = case solveClause t goal of
      -- if thre is no unification, thereis no solution in this path
      Nothing          -> []
      -- if the new goal is empty, there ia a solution
      Just ([],   sub) -> [Leaf $ addToSolution sol sub]
      -- add the substitution to the current solution
      -- solve for the nre goal
      Just (goal', sub)  -> [Node $ solve prog (addToSolution sol sub) goal']

--
-- picks the first clause and the first term in the goal,
-- if they can be unified - returns the new goal and the made substitution
-- otherwise returns Nothing
--
solveClause :: Clause -> Goal -> Maybe (Goal, Substitution)
solveClause (t, ts) (g:gs) = case unify t g of
  Nothing  -> Nothing
  Just sub -> Just ((map (substitute sub) (gs ++ ts)), sub)

--
-- returns the search tree
--
searchTree :: Program -> Goal -> SLDTree
searchTree prog goal = Node $ solve prog (initSolution goal) goal


--
-- dfs in the search tree
--
findSolutions :: Program -> Goal -> [Solution]
findSolutions prog goal = traverseTree $ searchTree prog goal
  where
    traverseTree (Leaf sub)     = [sub]
    traverseTree (Node trees) = foldr (++) [] (map traverseTree trees )


--
-- prints all solutions one by one.
-- The user has to press ENTER after each print in order to see the next solution.
-- Any other key will interupt the process.
--
printSolution []     = putStr "False\n\n"
printSolution (s:ss) = do
                          print s
                          c <- getChar
                          if c == '\n'
                            then
                              if not (null ss)
                                then printSolution ss
                                else return()
                            else return()
