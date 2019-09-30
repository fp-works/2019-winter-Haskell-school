{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Party
import Employee
import Data.Tree (Tree)
import Data.List (sort)

main :: IO ()
main = do
  companyStr <- readFile "company.txt"
  let company = read companyStr :: Tree Employee
  let gl = maxFun company
  putStrLn $ "Total fun: " ++ (show . getFun $ gl)
  putStrLn . unlines . sort . fmap empName . getInvitees $ gl
