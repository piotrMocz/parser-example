import Test.Hspec
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

main :: IO ()
main = hspec $ do
    openingParenSpec
