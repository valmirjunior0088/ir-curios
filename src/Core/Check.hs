module Core.Check
  ( check
  )
  where

import Core.Syntax
  ( Variable (..)
  , unwrap
  , PrimitiveType (..)
  , Primitive (..)
  , Operation (..)
  , Type
  , Term (..)
  , instantiate
  , open
  )

import Core.Bindings (Bindings)
import qualified Core.Bindings as Bindings

import Util ((!!!), unique, (<==>), (.&&.))
import Data.Functor ((<&>))
import Control.Monad (unless, zipWithM)
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
  Global name -> do
    definition <- Bindings.definition name <$> use (the @"globals")

    case definition of
      Just term -> reduce term
      _ -> return (Global name)

  Local variable -> do
    definition <- Bindings.definition (unwrap variable) <$> use (the @"locals")

    case definition of
      Just term -> reduce term
      _ -> return (Local variable)

  Apply function argument -> reduce function >>= \case
    Function body -> reduce (instantiate argument body)
    _ -> return (Apply function argument)

  Split scrutinee body -> reduce scrutinee >>= \case
    Pair left right -> reduce (instantiate right (instantiate left body))
    _ -> return (Split scrutinee body)

  Match scrutinee branches -> reduce scrutinee >>= \case
    Label label -> reduce (label !!! branches)
    _ -> return (Match scrutinee branches)

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
    (FunctionType input scope, FunctionType input' scope') -> do
      name <- fresh

      let
        output = open name scope
        output' = open name scope'

      go input input' .&&. go output output'

    (Function body, Function body') -> do
      name <- fresh

      let
        output = open name body
        output' = open name body'

      go output output'

    (Apply function argument, Apply function' argument') ->
      go function function' .&&. go argument argument'

    (PairType input scope, PairType input' scope') -> do
      name <- fresh

      let
        output = open name scope
        output' = open name scope'

      go input input' .&&. go output output'

    (Pair left right, Pair left' right') ->
      go left left' .&&. go right right'

    (Split scrutinee body, Split scrutinee' body') -> do
      left <- fresh
      right <- fresh

      let
        output = open right (open left body)
        output' = open right (open left body')

      go scrutinee scrutinee' .&&. go output output'

    (Match scrutinee branches, Match scrutinee' branches') -> do
      let
        labelsAreEqual = map fst branches <==> map fst branches'

        contains (label, target) targets = case lookup label targets of
          Nothing -> return False
          Just body -> go body target

        bodiesAreEqual = and <$> mapM (`contains` branches) branches'

      go scrutinee scrutinee' .&&. pure labelsAreEqual .&&. bodiesAreEqual

    (Operate operation parameters, Operate operation' parameters') -> do
      let
        operationsAreEqual = operation == operation'
        parametersAreEqual = and <$> zipWithM go parameters parameters'

      pure operationsAreEqual .&&. parametersAreEqual

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
  Global name -> do
    declaration <- Bindings.declaration name <$> use (the @"globals")

    case declaration of
      Nothing -> throwError ("unknown global `" ++ name ++ "`")
      Just tipe -> return tipe

  Local variable -> do
    declaration <- Bindings.declaration (unwrap variable) <$> use (the @"locals")

    case declaration of
      Nothing -> error "unknown local variable -- should not happen"
      Just tipe -> return tipe

  Type -> return Type

  FunctionType input scope -> region $ do
    checks Type input

    name <- bind input
    checks Type (open name scope)

    return Type

  Function _ -> throwError "functions don't have an inferable type"

  Apply function argument -> infers function >>= reduce >>= \case
    FunctionType input scope -> do
      checks input argument
      return (instantiate argument scope)

    _ -> throwError "function application type mismatch"

  PairType input scope -> region $ do
    checks Type input

    name <- bind input
    checks Type (open name scope)

    return Type

  Pair _ _ -> throwError "pairs don't have an inferable type"

  Split _ _ -> throwError "split expressions don't have an inferable type"

  LabelType set -> do
    unless (unique set) (throwError "label type has repeated labels")
    return Type

  Label _ -> throwError "labels don't have an inferable type"

  Match _ _ -> throwError "match expressions don't have an inferable type"

  PrimitiveType _ -> return Type

  Primitive primitive -> case primitive of
    Int32 _ -> return (PrimitiveType Int32Type)
    Flt32 _ -> return (PrimitiveType Flt32Type)

  Operate Int32Add [one, other] -> do
    checks (PrimitiveType Int32Type) one
    checks (PrimitiveType Int32Type) other
    return (PrimitiveType Int32Type)

  Operate Flt32Add [one, other] -> do
    checks (PrimitiveType Flt32Type) one
    checks (PrimitiveType Flt32Type) other
    return (PrimitiveType Flt32Type)

  Operate _ _ -> throwError "invalid operation format"

checks :: Type -> Term -> Check ()
checks tipe = \case
  Function body -> reduce tipe >>= \case
    FunctionType input scope -> region $ do
      name <- bind input
      checks (open name scope) (open name body)

    _ -> throwError "function type mismatch"

  Pair left right -> reduce tipe >>= \case
    PairType input scope -> do
      checks input left
      checks (instantiate left scope) right

    _ -> throwError "pair type mismatch"

  Split scrutinee body -> infers scrutinee >>= reduce >>= \case
    PairType input scope -> region $ do
      left <- bind input
      right <- bind (open left scope)

      reduce scrutinee >>= \case
        Local variable -> constrain (unwrap variable) pair where
          leftComponent = Local (Free left)
          rightComponent = Local (Free right)
          pair = Pair leftComponent rightComponent

        _ -> return ()

      checks tipe (open right (open left body))

    _ -> throwError "split expression scrutinee type mismatch"

  Label label -> reduce tipe >>= \case
    LabelType labels ->
      unless (label `elem` labels) (throwError "label does not belong to label type")

    _ -> throwError "label type mismatch"

  Match scrutinee branches -> do
    let labels = map fst branches

    unless (unique labels)
      (throwError "match expression has repeated branch labels")

    infers scrutinee >>= reduce >>= \case
      LabelType set -> do
        unless (labels <==> set)
          (throwError "match expression branch labels do not match scrutinee label type")

        go <- reduce scrutinee <&> \case
          Local variable -> \(label, body) -> region $ do
            constrain (unwrap variable) (Label label)
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
