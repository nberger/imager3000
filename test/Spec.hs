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
import qualified Text.Blaze.Html.Renderer.Utf8 as Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html.Renderer.String as Renderer.String (renderHtml)

import Imager3000.Parse

makeLiWithImage :: String -> H.Html
makeLiWithImage image = H.li $ H.img H.! src (H.stringValue image)

makeImagesHtml :: [String] -> H.Html
makeImagesHtml images = H.docTypeHtml $ do
                     H.body $ do
                       H.p "A list of images:"
                       H.ul $
                         forM_ images makeLiWithImage

data HtmlWithImages = HtmlWithImages { images :: [String]
                                     , html :: H.Html
                                     } deriving (Show)

genUrl :: Gen String
genUrl = listOf1 arbitrary

instance Arbitrary HtmlWithImages where
  arbitrary = do
    images <- listOf1 genUrl
    return (HtmlWithImages { images = images, html = makeImagesHtml images })

instance Show H.Html where
  show html = Renderer.String.renderHtml html

tests = [
        testGroup "ParseTests" [
            testProperty "Parse html with 1 image" prop_parse_one_image
          ]
        ]

prop_parse_one_image page =
-- (images page)
-- ["foo.png"]
   (images page) == getImages(Renderer.Utf8.renderHtml (html page)) where
      types = (page::HtmlWithImages)

main :: IO ()
main = defaultMain tests
