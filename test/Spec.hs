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
            testProperty "Parse html with some images" prop_parse_images,
            testProperty "Parse html with some images and show labels" prop_classified_parse_images
          ]
        ]

prop_parse_images page =
-- ["foo.png"]
-- (take 4 (images page))
   map URL.exportURL (images page) == getImages(Renderer.Utf8.renderHtml (html page)) where
      types = (page::HtmlWithImages)

prop_classified_parse_images page =
  classify (length (images page) == 0) "no images" $
  classify (length (images page) == 1) "1 image" $
  classify (length (images page) > 1 && length (images page) <= 5) "some images" $
  classify (length (images page) > 5) "many images" $
  prop_parse_images page

prop_collect_parse_images page =
  collect (length (images page)) $
  prop_parse_images page

prop_cover_parse_images page =
  cover (length (images page) > 10) 80 "at least 10 images" $
  prop_parse_images page

main :: IO ()
--main = defaultMain tests
--main = quickCheck prop_classified_parse_images
--main = quickCheck prop_collect_parse_images
--main = quickCheck prop_cover_parse_images
--main = sample (arbitrary :: Gen URL.URL)
