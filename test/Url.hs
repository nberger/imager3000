-- from https://searchcode.com/codesearch/view/16958267/
module Url where

import Test.QuickCheck
import Test.QuickCheck.Gen

import Data.List (intersperse, nub, nubBy)
import Network.URL

hostNameSource = (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ".-")
paramSource = hostNameSource ++ " ~;:@$_!*'(),"

webString :: Gen String
webString = listOf $ elements (paramSource ++ "?/=&^")

genPath :: Gen String
genPath = do
  x <- listOf $ webString
  x' <- return (map (encString True ok_path) x)
  return $ concat $ intersperse "/" x'

genParams :: Gen [(String, String)]
genParams = do
  n <- listOf $ webString
  v <- listOf $ webString
  return $ nubBy (\a b -> (fst a) == (fst b)) $ zip (enc n) (enc v)
  where enc x =  map (encString True ok_param) x

instance Arbitrary URL where
  arbitrary = do
    urlType <- arbitrary
    path <- genPath
    params <- genParams
    return (URL urlType path params)

instance Arbitrary URLType where
  arbitrary = do
    n <- choose (1,3) :: Gen Int
    case n of
      1 -> do x <- arbitrary
              return (Absolute x)
      2 -> return HostRelative
      3 -> return PathRelative

hostString :: Gen String
hostString = listOf $ elements hostNameSource

instance Arbitrary Host where
  arbitrary = do
    prot <- arbitrary
    host <- hostString
    port <- elements [Just 80, Just 8080, Nothing]
    return (Host prot host port)

instance Arbitrary Protocol where
  arbitrary = do
    n <- choose (1,3) :: Gen Int
    case n of
      1 -> do x <- arbitrary
              return (HTTP x)
      2 -> do x <- arbitrary
              return (FTP x)
      3 -> return (RawProt "some")
