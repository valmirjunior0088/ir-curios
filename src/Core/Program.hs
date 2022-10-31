module Core.Program
  ( Entry (..)
  , Program (..)
  , check
  )
  where

import Core.Syntax (Type, Term)
import qualified Core.Syntax as Syntax

import Core.Bindings (Bindings)
import qualified Core.Bindings as Bindings

import qualified Core.Check as Check

import Control.Monad (when)
import Data.Foldable (foldlM)

data Entry =
  Declaration String Type |
  Definition String Term
  deriving (Show)

newtype Program =
  Program [Entry]
  deriving (Show)

declare :: String -> Type -> Bindings -> Either String Bindings
declare name tipe globals = do
  when (Bindings.declared name globals)
    (Left "declared name being declared again")

  Check.check globals Syntax.Type tipe
  Right (Bindings.declare name tipe globals)

define :: String -> Term -> Bindings -> Either String Bindings
define name term globals = do
  tipe <- case Bindings.declaration name globals of
    Nothing -> Left "undeclared name being defined"
    Just tipe -> Right tipe
  
  Check.check globals tipe term
  Right (Bindings.define name term globals)

process :: Bindings -> Entry -> Either String Bindings
process globals = \case
  Declaration name tipe -> declare name tipe globals
  Definition name term -> define name term globals

check :: Program -> Either String Bindings
check (Program entries) = foldlM process Bindings.empty entries
