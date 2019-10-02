{-# OPTIONS_GHC -Wall #-}
module Party where
import Employee
import Data.Tree
import Data.List
import Text.Read

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp _ efun) (GL el glfun) = GL (el ++ [e]) (glfun + efun)

instance Semigroup GuestList where
  GL el1 f1 <> GL el2 f2 = GL (el1 <> el2) (f1 + f2)

instance Monoid GuestList where
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node a forest) = f a . fmap (treeFold f) $ forest

nextLevel :: Employee 
  -> [ ( GuestList, GuestList ) ]
  -> ( GuestList, GuestList )
nextLevel boss xs = foldr
  (\(gl1, gl2) (gl1', gl2') -> (gl1' <> gl2, gl2' <> (moreFun gl1 gl2))) 
  (glCons boss mempty, mempty)
  xs

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

showEmployees :: Employee -> String -> String
showEmployees (Emp ename _) acc = acc ++ "\n" ++ ename

instance Ord Employee where
  compare (Emp ename1 _) (Emp ename2 _) = compare ename1 ename2

format :: GuestList -> String
format (GL xs fun) = "Total fun: " ++ show fun ++ (foldr showEmployees "" $ sort xs)

getGL :: Maybe (Tree Employee) -> String
getGL (Just t) = format . maxFun $ t
getGL Nothing = "Please check company.txt"

main :: IO ()
main = readFile "company.txt" >>= putStrLn . getGL . readMaybe
