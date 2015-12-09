{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Prelude hiding (div, id)

import Control.Monad (forM_)

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Utf8 as Utf8 (renderHtml)
import qualified Text.Blaze.Html.Renderer.String as String (renderHtml)

import Imager3000.Parse

makeImagesHtml :: Int -> H.Html
makeImagesHtml n = H.docTypeHtml $ do
                     H.body $ do
                       H.p "A list of images:"
                       H.img H.! src "foo.png"

instance Arbitrary H.Html where
  arbitrary = makeImagesHtml <$> choose (10, 200)

instance Show H.Html where
  show html = String.renderHtml html

tests = [
        testGroup "ParseTests" [
            testProperty "Parse html with 1 image" prop_parse_one_image
          ]
        ]

prop_parse_one_image page =
  ["foo.png"] == getImages ( Utf8.renderHtml page ) where
      types = (page::H.Html)

main :: IO ()
main = defaultMain tests
