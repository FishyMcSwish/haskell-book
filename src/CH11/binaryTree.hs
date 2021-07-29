module CH11.BinaryTree where

data BinaryTree a = 
  Leaf 
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b > a = Node left a (insert' b right)
  | b < a = Node (insert' b left) a right


starterTree = insert' 2 . insert' 4 . insert' 3 $ Leaf

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)


testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = (inorder left) ++ [a] ++ (inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = (postorder left) ++ (postorder right) ++ [a] 

testPreOrder = preorder testTree == [2, 1, 3]

testInOrder = inorder testTree == [1, 2, 3]

testPostOrder = postorder testTree == [1, 3, 2]


foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf = b
foldTree f  b (Node left a right) =
  foldTree f (foldTree f (f a b) left) right


foldTree' :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree' _ b Leaf = b
foldTree' f b (Node left a right) =
  foldTree' f (f a (foldTree' f b left)) right

foldTreer :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTreer _ b Leaf = b
foldTreer f b (Node left a right) =
  f a (foldTreer f (foldTreer f b left) right)





