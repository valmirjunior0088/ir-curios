import Core.Parse (parse)
import Core.Program (check)
import Intermediate.Translate (translate)
import Intermediate.Compile (compile)
import Intermediate.Debug (debug)
import WebAssembly.Serialize (writeModule)
import System.Exit (die)
import System.Environment (getArgs, getProgName)

usage :: String -> String
usage name =
  "USAGE: " ++ name ++ " INPUT OUTPUT"

main :: IO ()
main = do
  arguments <- getArgs

  (input, output) <- case arguments of
    [input, output] -> return (input, output)
    _ -> getProgName >>= die . usage

  source <- readFile input

  program <- case parse source of
    Left sourceError -> die sourceError
    Right program -> return program
  
  globals <- case check program of
    Left checkError -> die checkError
    Right globals -> return globals
  
  putStrLn (debug $ translate globals)
  
  writeModule output (compile $ translate globals)

{-
import Intermediate.Syntax
import Intermediate.Compile
import WebAssembly.Serialize
import System.Exit
import System.Environment

main :: IO ()
main = do
  path <- getArgs >>= \case
    [path] -> return path
    _ -> die "wrong parameters"

  writeModule path $ compile $ Program
    { closures =
        [ ("const_1", Closure { environment = [], parameters = ["kept"], body =
            Tail (ClosureAlloc "const_2" [Local "kept"])
          })

        , ("const_2", Closure { environment = ["kept"], parameters = ["discarded"], body =
            Tail (Pure $ Environmental "kept")
          })
        ]


    , blocks =
        [ ("const", Block { parameters = [], body =
            Tail (ClosureAlloc "const_1" [])
          })

        , ("previous_start", Block { parameters = [], body =
            Bind "one" (Int32Alloc 2) $
            Bind "other" (Int32Alloc 3) $
            Bind "result" (Int32Add (Local "one") (Local "other")) $
            Bind "const_1" (Invoke "const" []) $
            Bind "const_2" (ClosureEnter (Local "const_1") [Local "result"]) $
            Bind "another" (Int32Alloc 4) $
            Tail (ClosureEnter (Local "const_2") [Local "another"])
          })

        , ("start", Block { parameters = [], body =
            Bind "scrutinee" (Int32Alloc 10) $
            Tail (Match (Local "scrutinee") [(5, Int32Alloc 10), (10, Int32Alloc 20), (15, Int32Alloc 30)])
          })
        ]
    }
-}