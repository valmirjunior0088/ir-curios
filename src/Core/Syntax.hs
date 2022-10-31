module Core.Syntax
  ( Variable (Free)
  , unwrap
  , Scope
  , unbound
  , Type
  , Term (..)
  , Walk
  , abstract
  , instantiate
  , open
  , free
  )
  where

import Data.List (nub)
import Control.Monad.Reader (Reader, runReader, asks, local)

data Variable =
  Free String |
  Bound Int
  deriving (Show, Eq)

unwrap :: Variable -> String
unwrap = \case
  Free name -> name
  Bound _ -> error "bound variable -- should not happen"

newtype Scope a = Scope a

instance Show a => Show (Scope a) where
  show (Scope scope) = "{" ++ show scope ++ "}"

instance Eq a => Eq (Scope a) where
  (==) (Scope one) (Scope other) = one == other

unbound :: a -> Scope a
unbound = Scope

type Type = Term

data Term =
  Global String |
  Local Variable |

  Type |

  FunctionType Type (Scope Type) |
  Function (Scope Term) |
  Apply Term Term |

  PairType Type (Scope Type) |
  Pair Term Term |
  Split Term (Scope (Scope Term)) |
  
  LabelType [String] |
  Label String |
  Match Term [(String, Term)]
  deriving (Show, Eq)

type Depth = Reader Int

class Walk a where
  walk :: (Variable -> Depth Term) -> a -> Depth a

with :: Walk a => (Int -> Variable -> Term) -> a -> a
with action subject = runReader (walk go subject) 0 where
  go variable = asks action <*> pure variable

instance Walk Term where
  walk action = \case 
    Global name -> pure (Global name)
    Local variable -> action variable
    Type -> pure Type
    FunctionType input scope -> FunctionType <$> walk action input <*> walk action scope
    Function body -> Function <$> walk action body
    Apply function argument -> Apply <$> walk action function <*> walk action argument
    PairType input scope -> PairType <$> walk action input <*> walk action scope
    Pair left right -> Pair <$> walk action left <*> walk action right
    Split scrutinee body -> Split <$> walk action scrutinee <*> walk action body
    LabelType set -> pure (LabelType set)
    Label label -> pure (Label label)
    Match scrutinee branches -> Match <$> walk action scrutinee <*> mapM (mapM $ walk action) branches

instance Walk a => Walk (Scope a) where
  walk action (Scope scope) = Scope <$> local succ (walk action scope)

abstract :: Walk a => String -> a -> Scope a
abstract target subject = Scope (with go subject) where
  go depth = \case
    Free name | name == target -> Local (Bound depth)
    variable -> Local variable

instantiate :: Walk a => Term -> Scope a -> a
instantiate term (Scope subject) = with go subject where
  go depth = \case
    Bound index | index == depth -> term
    variable -> Local variable

open :: Walk a => String -> Scope a -> a
open name = instantiate (Local $ Free name)

free :: Term -> [String]
free = \case 
  Global _ -> []
  Local (Free name) -> [name]
  Local _ -> []
  Type -> []
  FunctionType input (Scope output) -> nub (free input ++ free output)
  Function (Scope body) -> nub (free body)
  Apply function argument -> nub (free function ++ free argument)
  PairType input (Scope output) -> nub (free input ++ free output)
  Pair left right -> nub (free left ++ free right)
  Split scrutinee (Scope (Scope body)) -> nub (free scrutinee ++ free body)
  LabelType _ -> []
  Label _ -> []
  Match scrutinee branches -> nub (free scrutinee ++ concatMap (free . snd) branches)
