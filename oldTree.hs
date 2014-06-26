import Test.QuickCheck
import Test.QuickCheck.All

data BinaryTree a = Node a (BinaryTree a) (BinaryTree a) | Null deriving (Show, Eq)

instance Arbitrary a => Arbitrary (BinaryTree a) where
  arbitrary = do
    x <- arbitrary
    left <- arbitrary
    right <- arbitrary
    elements [Null, Node x left right]

treeToList :: BinaryTree a -> [a]
treeToList Null = []
treeToList (Node x left right) = (treeToList left) ++ [x] ++ (treeToList right)

prop_equal_sizes tree = 
  length (treeToList tree) == treeLength tree
 where
  treeLength Null = 0
  treeLength (Node _ left right) = 1 + treeLength left + treeLength right
