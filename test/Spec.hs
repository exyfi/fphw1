import Test.Hspec
import HW.Lang
import HW.Parse
import HW.Print
import HW.Interp
import Text.Megaparsec
import Data.Either

fibSrc = unlines [ "def fib(x):"
                 , "    if not (x > 1):"
                 , "        return 1"
                 , ""
                 , "    return fib(x - 1) + fib(x - 2)"
                 , ""
                 , "x = int(input())"
                 , "print(fib(x))"
                 ]

fibDsl = do
    let one = Lit (VInt 1)
    let two = Lit (VInt 2)

    def "fib" ["x"] $ do
        if' (Unop Not (Binop Gt (Ref "x") one)) $ do
            ret one
        ret (Binop Add (Call "fib" [Binop Sub (Ref "x") one]) (Call "fib" [Binop Sub (Ref "x") two]))
    assign "x" (Call "int" [Call "input" []])
    expr (Call "print" [Call "fib" [Ref "x"]])

fibDslStr = unlines [ "do"
                    , "    def \"fib\" [\"x\"] $ do"
                    , "        if' (Unop Not (Binop Gt (Ref \"x\") (Lit (VInt 1)))) $ do"
                    , "            ret $ Lit (VInt 1)"
                    , ""
                    , "        ret $ Binop Add (Call \"fib\" [Binop Sub (Ref \"x\") (Lit (VInt 1))]) (Call \"fib\" [Binop Sub (Ref \"x\") (Lit (VInt 2))])"
                    , ""
                    , "    assign \"x\" $ Call \"int\" [Call \"input\" []]"
                    , "    expr $ Call \"print\" [Call \"fib\" [Ref \"x\"]]"
                    ]

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)


main :: IO ()
main = hspec $ do
    describe "parser" $ do
        it "parses fibonacci" $
            (parse parser "" fibSrc) `shouldBe` (Right fibDsl)

        it "invalid indentation" $ 
            (isLeft $ parse parser "" $ unlines [ "if True:" , "x = 1" ]) `shouldBe` True

        it "invalid expr" $ 
            (isLeft $ parse parser "" $ unlines [ "if ((1 + 2 - 3) * 4 + 5 = 6):" , "    x = 1" ]) `shouldBe` True

    describe "pretty-printer" $ do
        it "prints fibonacci" $
            pretty fibDsl `shouldBe` fibSrc

    describe "edsl printer" $ do
        it "prints fibonacci" $
            edsl fibDsl `shouldBe` fibDslStr

    describe "interpreter" $ do
        sequence_ $ flip map [0..10] $ \n -> 
            it ("calculates fibonacci number #" <> (show n)) $
                (runInterpFake (interp fibDsl) [show n]) `shouldBe` Right [show $ fib n]

