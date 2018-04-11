import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck.Test
import MonadInstances
import Data.Monoid


-- Identity intance of Arbitrary and EqProp

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance Eq a => EqProp (Identity a) where
  (=-=) = eq


-- Contant instance of Arbitrary

instance (Arbitrary a, Monoid a) => Arbitrary (Constant a b) where
  arbitrary = do
    a <- arbitrary
    return $ Constant a

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq


-- List instance of Arbitrary

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [ (1, return Nil),
                (10, return (Cons x y)) ]

instance Eq a => EqProp (List a) where
  (=-=) = eq


tests :: IO ()
tests = hspec $ do
  describe "Identity Applicative" $ do
    it "satisfies property" $ do
      quickBatch (monad $ Identity ("Test", [
        ("hello" :: String) ], ("world" :: String)))

main :: IO ()
main = do tests
