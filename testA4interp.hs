-- How to use: runghc testA4interp.hs

import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           Test.HUnit
import           Text.Read (readMaybe)

import qualified Data.Map as Map

import           A4 (mainInterp)
import           A4Def

tests =
    [ testEqNum
    , testPlus, testDiv
    , testCond, testLambda, testLet, testApp
    , testError
    ]
-- more test cases when marking

testEqNum = TestList
    [ "7 == 7" ~: mainInterp (Prim2 Eq (Num 7) (Num 7)) ~?= Right (VB True)
    , "7 == 8" ~: mainInterp (Prim2 Eq (Num 7) (Num 8)) ~?= Right (VB False)
    ]

testPlus = "5 + 7" ~: mainInterp (Prim2 Plus (Num 5) (Num 7)) ~?= Right (VN 12)

testDiv = TestList
    [ "(-100) div 3" ~:
      mainInterp (Prim2 Div (Num (-100)) (Num 3)) ~?= Right (VN (-34))
    , "1 div 0" ~:
      mainInterp (Prim2 Div (Num 1) (Num 0)) ~?= Left DivByZero
    ]

testCond = TestList
    [ "if True then 1 else 2" ~:
      mainInterp (Cond (Bln True) (Num 1) (Num 2)) ~?= Right (VN 1)
    , "if False then 1 else 2" ~:
      mainInterp (Cond (Bln False) (Num 1) (Num 2)) ~?= Right (VN 2)
    ]

testLambda = "\\v -> v" ~:
    mainInterp (Lambda "v" (Var "v")) ~?= Right (VClosure Map.empty "v" (Var "v"))

testLet = TestList
    [ "let in 0" ~: mainInterp (Let [] (Num 0)) ~?= Right (VN 0)
    , "let x = 7; y = x; in y" ~:
      mainInterp (Let [("x", Num 7), ("y", Var "x")]
                   (Var "y"))
      ~?= Right (VN 7)
    ]

testApp = "(\\v -> v) 43" ~:
    mainInterp (App (Lambda "v" (Var "v")) (Num 43)) ~?= Right (VN 43)

testError = "let x=2/0; in 1+True" ~:
    mainInterp (Let [("x", Prim2 Div (Num 2) (Num 0))] (Prim2 Plus (Num 1) (Bln True)))
    ~?= Left DivByZero

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
