module Intermediate.Debug
  ( debug
  )
  where

import Prelude hiding ((<>))

import Intermediate.Syntax (Expression (..), Sequence (..), Closure (..), Block (..), Program)
import Text.PrettyPrint.Boxes (Box, emptyBox, text, (<>), (//), hcat, top, vcat, left, render)
import Data.List (intercalate)
import Data.Int (Int32)
import Data.Generics.Product (the)
import Control.Lens ((^.))

debugBranch :: Int32 -> Expression -> Box
debugBranch label branch =
  text "|" <> text (show label) <> text "| " <> debugExpression branch <> text " "

debugExpression :: Expression -> Box
debugExpression = \case
  Int32Alloc value -> text "int32.alloc "
    <> text (show value)

  Int32Add one other -> text "int32.add ["
    <> text (show one)
    <> text ", "
    <> text (show other)
    <> text "]"

  Flt32Alloc value -> text "flt32.alloc "
    <> text (show value)

  Flt32Add one other -> text "flt32.add ["
    <> text (show one)
    <> text ", "
    <> text (show other)
    <> text "]"

  Pure atom -> text "pure ["
    <> text (show atom)
    <> text "]"

  BlockCall block atoms -> text "block.call "
    <> text block
    <> text " ["
    <> text (intercalate ", " $ map show atoms)
    <> text "]"

  Int32Match atom branches -> text "int32.match ["
    <> text (show atom)
    <> text "]: "
    <> hcat top [debugBranch label branch | (label, branch) <- branches]

  ClosureAlloc closure atoms -> text "closure.alloc "
    <> text closure
    <> text " {"
    <> text (intercalate ", " $ map show atoms)
    <> text "}"

  ClosureEnter atom atoms -> text "closure.enter ["
    <> text (show atom)
    <> text "] ["
    <> text (intercalate ", " $ map show atoms)
    <> text "]"

  StructAlloc atoms -> text "struct.alloc ["
    <> text (intercalate ", " $ map show atoms)
    <> text "]"

  StructSelect atom index -> text "struct.select ["
    <> text (show atom)
    <> text "] "
    <> text (show index)

debugSequence :: Sequence -> Box
debugSequence = \case
  Bind name body rest -> do
    let
      branchBox = text name <> text " <- " <> debugExpression body <> text ";"
      restBox = debugSequence rest

    branchBox // restBox

  Tail body ->
    debugExpression body

debugClosure :: String -> Closure -> Box
debugClosure name Closure { environment, parameters, body } = do
  let
    environmentBox = text " {" <> text (intercalate ", " environment) <> text "}"
    parametersBox = text " [" <> text (intercalate ", " parameters) <> text "]"
    headerBox = text "closure " <> text name <> environmentBox <> parametersBox <> text " do"
    bodyBox = emptyBox 1 2 <> debugSequence body
    footerBox = text "end" // emptyBox 1 1

  headerBox // bodyBox // footerBox

debugBlock :: String -> Block -> Box
debugBlock name Block { parameters, body } = do
  let
    parametersBox = text " [" <> text (intercalate ", " parameters) <> text "]"
    headerBox = text "block " <> text name <> parametersBox <> text " do"
    bodyBox = emptyBox 1 2 <> debugSequence body
    footerBox = text "end" // emptyBox 1 1

  headerBox // bodyBox // footerBox

debugProgram :: Program -> Box
debugProgram program = do
  let
    closures = [debugClosure name closure | (name, closure) <- program ^. the @"closures"]
    blocks = [debugBlock name block | (name, block) <- program ^. the @"blocks"]

  vcat left closures // vcat left blocks

debug :: Program -> String
debug program = render (debugProgram program)
