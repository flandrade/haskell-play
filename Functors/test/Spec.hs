import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function
import FunctorInstances


-- Identity 

identity :: (Functor f, Eq (f a)) => f a -> Bool
identity f = fmap id f == f


-- Composition

composition :: (Eq (f c), Functor f) => Fun a b -> Fun b c -> f a -> Bool
composition (Fun _ f) (Fun _ g) x  = (fmap  g . fmap f) x == (fmap (g . f) x)


-- Identity instance of Arbitrary

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

-- List instance of Arbitrary

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [ (1, return Nil),
                (10, return (Cons x y)) ]
  
  
-- Pair instance of Arbitrary
  
instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b


-- Two instance of Arbitrary
  
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b


tests :: IO ()
tests = hspec $ do 
  describe "Identity Functor" $ do
    it "satisfies identity property" $ do
      property (identity :: Identity Int -> Bool)
    
    it "satisfies composition property" $ do
      property (composition :: Fun Int String -> Fun String String -> Identity Int -> Bool)
  
  describe "List Functor" $ do
    it "satisfies identity property" $ do
      property (identity :: List String -> Bool)
    
    it "satisfies composition property" $ do
      property (composition :: Fun Int String -> Fun String String -> List Int -> Bool)

  describe "Pair Functor" $ do
    it "satisfies identity property" $ do
      property (identity :: Pair Int -> Bool)
    
    it "satisfies composition property" $ do
      property (composition :: Fun Int String -> Fun String String -> Pair Int -> Bool)

  describe "Two Functor" $ do
    it "satisfies identity property" $ do
      property (identity :: Two Int String -> Bool)
    
    it "satisfies composition property" $ do
      property (composition :: Fun Int String -> Fun String String -> Two Int Int -> Bool)
    

main :: IO ()
main = do tests
