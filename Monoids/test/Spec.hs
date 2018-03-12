import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function
import Control.Monad 
import Data.Monoid hiding (Sum)
import MonoidInstances


--  Associativity

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool 
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)


--  Identity

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool 
monoidRightIdentity a = (a <> mempty) == a


-- Sum instance of Arbitrary

instance (Arbitrary a) => Arbitrary (Sum a) where
  arbitrary = do
    a <- arbitrary
    return $ Sum a


-- List instance of Arbitrary

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [ (1, return Nil),
                (10, return (Cons x y)) ]


tests :: IO ()
tests = hspec $ do 
  describe "Sum Monoid" $ do
    it "satisfies associativity property" $ do
      quickCheck (monoidAssoc :: Sum Int -> Sum Int -> Sum Int -> Bool)
    
    it "satisfies left identity property" $ do
      property (monoidLeftIdentity :: Sum Int -> Bool)
    
    it "satisfies right identity property" $ do
      property (monoidRightIdentity :: Sum Int -> Bool)
  
  describe "List Monoid" $ do
    it "satisfies associativity property" $ do
      quickCheck (monoidAssoc :: List String -> List String -> List String -> Bool)
    
    it "satisfies left identity property" $ do
      property (monoidLeftIdentity :: List Int -> Bool)
    
    it "satisfies right identity property" $ do
      property (monoidRightIdentity :: List Int -> Bool)

main :: IO ()
main = do tests
