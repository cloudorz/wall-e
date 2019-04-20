module FeedsCollector
    ( getFeedsItems
    , Item(..)
    , Tag
    ) where

import Data.List (find)
import Data.Maybe (maybeToList, mapMaybe)
import Control.Lens
import Network.Wreq
import Text.XML.Light
import Text.XML.Light.Lexer (XmlSource)
import Data.Time.Format (parseTimeM, rfc822DateFormat, defaultTimeLocale)
import Data.Time.Clock (UTCTime)

type Tag = String

data Item = Item { title :: String
                 , description :: String
                 , link :: String
                 , tags :: [Tag]
                 , pubDate :: UTCTime
                 } deriving (Show)

getFeedsItems :: String -> IO [Item]
getFeedsItems url = fmap (xmlToItems . (^. responseBody)) $ get url

xmlToItems :: XmlSource s => s -> [Item]
xmlToItems = toItems . parseXML
  where toItems = mapMaybe parseItem . allItemElements

allItemElements :: [Content] -> [Element]
allItemElements xmls = maybeToList rssRoot >>= channels >>= items
  where rssRoot = find ((== QName "rss" Nothing Nothing) . elName) $ onlyElems xmls
        channels = findChildren (QName "channel" Nothing Nothing)
        items = findChildren (QName "item" Nothing Nothing)

prop :: Element -> String -> Maybe String
prop node name = strContent <$> findChild (QName name Nothing Nothing) node

parseTags :: Element -> [Tag]
parseTags = (strContent <$>) . findChildren (QName "category" Nothing Nothing)

parsePubDate :: String -> Maybe UTCTime
parsePubDate = parseTimeM True defaultTimeLocale rfc822DateFormat 

parseItem :: Element -> Maybe Item
parseItem node = Item <$> title <*> description <*> link <*> tags <*> pubDate
  where title = prop node "title"
        link = prop node "link"
        description = prop node "description"
        tags = Just $ parseTags node
        pubDate = prop node "pubDate" >>= parsePubDate 

