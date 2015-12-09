{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (div, id)

import Control.Monad (forM_)

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Utf8 as Utf8 (renderHtml)

import Imager3000.Parse

makeImagesHtml :: Int -> Html
makeImagesHtml n = docTypeHtml $ do
                     body $ do
                       p "A list of images:"
                       img ! src "foo.png"

instance Arbitrary Html5 where
  arbitrary = makeImagesHtml <$> choose (10, 200)

tests = [
        testGroup "ParseTests" [
            testProperty "Parse html with 1 image" prop_parse_one_image
          ]
        ]

prop_parse_one_image page =
  ["foo.png"] == getImages $ Utf8.renderHtml page where
      types = (page::Html)

main :: IO ()
main = defaultMain tests
