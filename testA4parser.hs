-- How to use: runghc testA4parser.hs

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Test.HUnit
import Text.Read (readMaybe)

import A4Def
import A4 (mainParser)
import ParserLib

parse = runParser mainParser

tests =
    [ testHandout
    , testJunk
    ]
-- more test cases when marking

testHandout =
    "handout" ~: parse inp
    ~?= (Just (Let [("x", Prim2 Mul (Num 4) (Prim2 Plus (Num 5) (Num 6)))]
                   (Lambda "y" (Cond (Prim2 Lt (Var "x") (Var "y"))
                                     (Var "x")
                                     (Var "y")))))
  where
    inp = "  let x \n     = 4 * ( \n  5+6) ; \n in \\ y ->  if  x< y then x else y\n"

testJunk = "junk after 0" ~: parse "0 (" ~?= Nothing

main = do
    args <- getArgs
    case args of
      a:_ | Just n <- readMaybe a, 0 <= n, n < length tests ->
            do c@Counts{errors=e, failures=f} <- runTestTT (tests !! n)
               if e == 0 && f == 0
                   then return c
                   else exitFailure
          | otherwise -> error "No such test number."
      _ -> runTestTT (TestList tests)
