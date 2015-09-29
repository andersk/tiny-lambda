{-|
Module      : TinyLambda
Description : Tiny normalizing interpreter for the untyped λ-calculus
Copyright   : © Anders Kaseorg, 2015
License     : BSD3
Maintainer  : Anders Kaseorg <andersk@mit.edu>

This is an version of my solution to
<http://codegolf.stackexchange.com/questions/284/write-an-interpreter-for-the-untyped-lambda-calculus>
expanded for readability.

It accepts one λ-calculus term from standard input, in this format:

@
((λ f. (λ x. (f x))) (λ y. (λ x. y)))
@

terminated by a newline, and prints the normalized term (if any) to
standard output in the same format.
-}

module TinyLambda (
  Term (..),
  -- * Displaying terms
  Name, Fresh,
  firstFresh, nextFresh,
  Display,
  displayVariable, displayLambda, displayApplication,
  -- * Constructing terms
  Env, OpenTerm,
  irreducible, applyIrreducible,
  makeVariable, makeLambda, makeApplication,
  -- * Parsing terms
  parse, parseParenthesized,
  -- * Top-level interaction
  emptyEnv,
  interaction, main,
  ) where

-- | Nameless internal representation of terms (this avoids problems
-- with variable capture).
data Term = Term {
  -- | How to apply this term to another term.
  apply :: Term -> Term,
  -- | How to display this term given a supply of fresh variables.
  display :: Display
  }

-- Displaying terms

-- | A variable name.
type Name = String

-- | A supply of fresh variables (we just keep the next fresh variable
-- and compute the rest later).
type Fresh = Name

-- | The first fresh variable (we use @x@).
firstFresh :: Fresh
firstFresh = "x"

-- | Get more fresh variables (we use @xx@, @xxx@, @xxxx@, …).
nextFresh :: Fresh -> Fresh
nextFresh var = "x" ++ var

-- | A function for displaying a term given a supply of fresh
-- variables.
type Display = Fresh -> String

-- | Display a variable: @"x"@.
displayVariable :: Name -> Display
displayVariable var _ = var

-- | Display a lambda abstraction: @"(λ x. body)"@.  We always use the
-- next fresh variable.
displayLambda :: (Display -> Display) -> Display
displayLambda dispBody freshVar =
  "(λ " ++ freshVar ++ ". " ++
  dispBody (displayVariable freshVar) (nextFresh freshVar) ++
  ")"

-- | Display an application: @"(fun arg)"@.
displayApplication :: Display -> Display -> Display
displayApplication dispFun dispArg freshVar =
  "(" ++ dispFun freshVar ++ " " ++ dispArg freshVar ++ ")"

-- Constructing terms

-- | An environment mapping variable names to (closed) terms.
type Env = [(Name, Term)]

-- | A term that may have free variables to be looked up in the
-- environment.
type OpenTerm = Env -> Term

-- | Given its display function, construct an atomic term that has no
-- further reductions.
irreducible :: Display -> Term
irreducible disp =
  Term {apply=applyIrreducible disp, display=disp}

-- | Given its display function, apply an irreducible term to another
-- term.
applyIrreducible :: Display -> Term -> Term
applyIrreducible disp arg =
  irreducible (displayApplication disp (display arg))

-- | Construct a variable term, which looks up its value in the
-- environment.
makeVariable :: Name -> OpenTerm
makeVariable var env = value
  where Just value = lookup var env

-- | Construct a lambda abstraction term.
makeLambda :: Name -> OpenTerm -> OpenTerm
makeLambda var body env =
  Term {apply=app, display=displayLambda dispBody}
  where app :: Term -> Term
        app arg = body ((var, arg) : env)
        dispBody :: Display -> Display
        dispBody dispVar = display (app (irreducible dispVar))

-- | Construct an application term using the first term’s 'apply'
-- method.
makeApplication :: OpenTerm -> OpenTerm -> OpenTerm
makeApplication fun arg env = apply (fun env) (arg env)

-- Parsing terms

-- | Parser for a term.
parse :: ReadS OpenTerm
parse str =
  readParen True parseParenthesized str ++
  [(makeVariable var, str') |
   (var, str') <- lex str]

-- | Helper parser for a parenthesized part of a term (lambda or
-- application).
parseParenthesized :: ReadS OpenTerm
parseParenthesized ('λ' : str) =
  [(makeLambda var body, str'') |
   (var, '.' : str') <- lex str,
   (body, str'') <- parse str']
parseParenthesized str =
  [(makeApplication fun arg, str'') |
   (fun, str') <- parse str,
   (arg, str'') <- parse str']

-- Top-level interaction

-- | The empty environment for closing terms.
emptyEnv :: Env
emptyEnv = []

-- | Parse an input term with a newline, and display a normalized
-- output term with a newline.
interaction :: String -> String
interaction input =
  concat [display (openTerm emptyEnv) firstFresh ++ [newline] |
          (openTerm, [newline]) <- parse input]

-- | Entry point.
main :: IO ()
main = interact interaction
