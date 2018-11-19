import Test.Hspec
import Test.QuickCheck
import Parsers

isLeft :: Either a b -> Bool
isLeft (Left  _) = True
isLeft (Right _) = False

openingParenSpec :: Spec
openingParenSpec = describe "The open paren parser" $ do
    it "should parse the \"(\" string" $
        parse openingParen "(" `shouldBe` Right '('

    it "should parse the \"(hello)\" string" $
        parse openingParen "(hello)" `shouldBe` Right '('

    it "should fail on the \"hello\" string" $
        isLeft (parse openingParen "hello") `shouldBe` True

openingParenPropSpec :: Spec
openingParenPropSpec = describe "A single character parser" $ do
    it "should accept any string starting with (" $ property $
        \s -> parse openingParen ('(':s) `shouldBe` Right '('

    it "should not accept anything not starting with (" $ property $
        \s -> do
            let s' = case s of
                    ('(':rest) -> rest
                    x -> x
            isLeft (parse openingParen s') `shouldBe` True

main :: IO ()
main = hspec $ do
    openingParenSpec
    openingParenPropSpec