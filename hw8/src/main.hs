module Main where

import Party
import Employee

main = do
  companyString <- readFile "company.txt"
  let company = read companyString
  putStrLn . show $ maxFun testCompany
  putStrLn . show $ maxFun testCompany2
  putStrLn . show $ maxFun company
