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

import qualified Network.URL as URL

import Imager3000.Parse
import Url

makeLiWithImage :: String -> H.Html
makeLiWithImage image = H.li $ H.img H.! src (H.stringValue image)

makeImagesHtml :: [URL.URL] -> H.Html
makeImagesHtml images = H.docTypeHtml $ do
                     H.body $ do
                       H.p "A list of images:"
                       H.ul $
                         forM_ images (makeLiWithImage . URL.exportURL)

data HtmlWithImages = HtmlWithImages { images :: [URL.URL]
                                     , html :: H.Html
                                     } deriving (Show)

instance Arbitrary HtmlWithImages where
  arbitrary = do
    images <- arbitrary
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
   map URL.exportURL (images page) == getImages(Renderer.Utf8.renderHtml (html page)) where
      types = (page::HtmlWithImages)

main :: IO ()
main = defaultMain tests
