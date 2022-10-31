module Core.Check
  ( check
  )
  where

import Core.Syntax (Variable (..), unwrap, Type, Term, instantiate, open)
import qualified Core.Syntax as Syntax

import Core.Bindings (Bindings)
import qualified Core.Bindings as Bindings

import Util ((!!!), unique, (<==>), (.&&.))
import Data.Functor ((<&>))
import Control.Monad (unless)
import Control.Monad.State (MonadState (..), StateT, evalStateT)
import Control.Monad.Except (MonadError (..), Except, runExcept)
import GHC.Generics (Generic)
import Data.Generics.Product (the)
import Control.Lens (use, (.=), (%=))

data CheckState = CheckState
  { seed :: Integer
  , globals :: Bindings
  , locals :: Bindings
  }
  deriving (Show, Generic)

emptyState :: Bindings -> CheckState
emptyState globals = CheckState
  { seed = 0
  , globals = globals
  , locals = Bindings.empty
  }

newtype Check a =
  Check (StateT CheckState (Except String) a)
  deriving (Functor, Applicative, Monad, MonadState CheckState, MonadError String)

runCheck :: Check a -> Bindings -> Either String a
runCheck (Check action) globals = runExcept (evalStateT action $ emptyState globals)

region :: Check a -> Check a
region action = do
  locals <- use (the @"locals")
  result <- action
  (the @"locals") .= locals
  return result

fresh :: Check String
fresh = do
  seed <- use (the @"seed")
  (the @"seed") .= succ seed
  return (show seed)

reduce :: Term -> Check Term
reduce = \case
  Syntax.Global name -> do
    definition <- Bindings.definition name <$> use (the @"globals")
    
    case definition of
      Just term -> reduce term
      _ -> return (Syntax.Global name)
  
  Syntax.Local variable -> do
    definition <- Bindings.definition (unwrap variable) <$> use (the @"locals")
    
    case definition of
      Just term -> reduce term
      _ -> return (Syntax.Local variable)

  Syntax.Apply function argument -> reduce function >>= \case
    Syntax.Function body -> reduce (instantiate argument body)
    _ -> return (Syntax.Apply function argument)

  Syntax.Split scrutinee body -> reduce scrutinee >>= \case
    Syntax.Pair left right -> reduce (instantiate right (instantiate left body))
    _ -> return (Syntax.Split scrutinee body)

  Syntax.Match scrutinee branches -> reduce scrutinee >>= \case
    Syntax.Label label -> reduce (label !!! branches)
    _ -> return (Syntax.Match scrutinee branches)
  
  term -> return term

equals :: [(Term, Term)] -> Term -> Term -> Check Bool
equals history one other = do
  one' <- reduce one
  other' <- reduce other

  let
    isEqual = one' == other'
    isRemembered = (one', other') `elem` history
    go = equals ((one', other') : history)

  if isEqual || isRemembered then return True else case (one', other') of
    (Syntax.FunctionType input scope, Syntax.FunctionType input' scope') -> do
      name <- fresh

      let
        output = open name scope
        output' = open name scope'

      go input input' .&&. go output output'

    (Syntax.Function body, Syntax.Function body') -> do
      name <- fresh

      let
        output = open name body
        output' = open name body'

      go output output'
    
    (Syntax.Apply function argument, Syntax.Apply function' argument') ->
      go function function' .&&. go argument argument'
    
    (Syntax.PairType input scope, Syntax.PairType input' scope') -> do
      name <- fresh

      let
        output = open name scope
        output' = open name scope'

      go input input' .&&. go output output'
    
    (Syntax.Pair left right, Syntax.Pair left' right') ->
      go left left' .&&. go right right'
    
    (Syntax.Split scrutinee body, Syntax.Split scrutinee' body') -> do
      left <- fresh
      right <- fresh

      let
        output = open right (open left body)
        output' = open right (open left body')

      go scrutinee scrutinee' .&&. go output output'

    (Syntax.Match scrutinee branches, Syntax.Match scrutinee' branches') -> do
      let
        labelsAreEqual = map fst branches <==> map fst branches'

        contains (label, target) targets = case lookup label targets of
          Nothing -> return False
          Just body -> go body target

        bodiesAreEqual = and <$> mapM (`contains` branches) branches'

      go scrutinee scrutinee' .&&. pure labelsAreEqual .&&. bodiesAreEqual
    
    (_, _) -> return False

equal :: Term -> Term -> Check Bool
equal = equals []

bind :: Type -> Check String
bind tipe = do
  name <- fresh
  (the @"locals") %= Bindings.declare name tipe
  return name

constrain :: String -> Term -> Check ()
constrain name term = do
  (the @"locals") %= Bindings.define name term

infers :: Term -> Check Type
infers = \case
  Syntax.Global name -> do
    declaration <- Bindings.declaration name <$> use (the @"globals")

    case declaration of
      Nothing -> throwError ("unknown global `" ++ name ++ "`")
      Just tipe -> return tipe

  Syntax.Local variable -> do
    declaration <- Bindings.declaration (unwrap variable) <$> use (the @"locals")

    case declaration of
      Nothing -> error "unknown local variable -- should not happen"
      Just tipe -> return tipe

  Syntax.Type -> return Syntax.Type

  Syntax.FunctionType input scope -> region $ do
    checks Syntax.Type input
    
    name <- bind input
    checks Syntax.Type (open name scope)

    return Syntax.Type

  Syntax.Function _ -> throwError "functions don't have an inferable type"

  Syntax.Apply function argument -> infers function >>= reduce >>= \case
    Syntax.FunctionType input scope -> do
      checks input argument 
      return (instantiate argument scope)
    
    _ -> throwError "function application type mismatch"

  Syntax.PairType input scope -> region $ do
    checks Syntax.Type input 

    name <- bind input
    checks Syntax.Type (open name scope) 

    return Syntax.Type

  Syntax.Pair _ _ -> throwError "pairs don't have an inferable type"

  Syntax.Split _ _ -> throwError "split expressions don't have an inferable type"

  Syntax.LabelType set -> do
    unless (unique set) (throwError "label type has repeated labels")
    return Syntax.Type

  Syntax.Label _ -> throwError "labels don't have an inferable type"

  Syntax.Match _ _ -> throwError "match expressions don't have an inferable type"

checks :: Type -> Term -> Check ()
checks tipe = \case
  Syntax.Function body -> reduce tipe >>= \case
    Syntax.FunctionType input scope -> region $ do
      name <- bind input
      checks (open name scope) (open name body)
    
    _ -> throwError "function type mismatch"
  
  Syntax.Pair left right -> reduce tipe >>= \case
    Syntax.PairType input scope -> do
      checks input left
      checks (instantiate left scope) right
    
    _ -> throwError "pair type mismatch"
  
  Syntax.Split scrutinee body -> infers scrutinee >>= reduce >>= \case
    Syntax.PairType input scope -> region $ do
      left <- bind input
      right <- bind (open left scope)

      reduce scrutinee >>= \case
        Syntax.Local variable -> constrain (unwrap variable) pair where
          leftComponent = Syntax.Local (Free left)
          rightComponent = Syntax.Local (Free right)
          pair = Syntax.Pair leftComponent rightComponent

        _ -> return ()
      
      checks tipe (open right (open left body))
      
    _ -> throwError "split expression scrutinee type mismatch"
  
  Syntax.Label label -> reduce tipe >>= \case
    Syntax.LabelType labels ->
      unless (label `elem` labels) (throwError "label does not belong to label type")
    
    _ -> throwError "label type mismatch"
  
  Syntax.Match scrutinee branches -> do
    let labels = map fst branches

    unless (unique labels)
      (throwError "match expression has repeated branch labels")
    
    infers scrutinee >>= reduce >>= \case
      Syntax.LabelType set -> do
        unless (labels <==> set)
          (throwError "match expression branch labels do not match scrutinee label type")
        
        go <- reduce scrutinee <&> \case
          Syntax.Local variable -> \(label, body) -> region $ do
            constrain (unwrap variable) (Syntax.Label label)
            checks tipe body
          
          _ -> \(_, body) -> checks tipe body
        
        mapM_ go branches

      _ -> throwError "match expression scrutinee type mismatch"

  term -> do
    tipe' <- infers term
    areEqual <- equal tipe tipe'
    unless areEqual (throwError "type mismatch")

check :: Bindings -> Type -> Term -> Either String ()
check globals tipe term = runCheck (checks tipe term) globals
