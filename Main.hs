{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Main where


import qualified Data.List as L
import qualified Data.ByteString.Lazy as BS
import Data.Char
import Data.Maybe

import Options.Applicative hiding (header)
import Control.Lens
import qualified Options.Applicative as O
import Network.Wreq


makeUrl :: IpsumOpts -> String
makeUrl IpsumOpts{..} =
    L.intercalate "/" $ catMaybes [ Just "http://loripsum.net/api"
                                  , bool optAllCaps   "allcaps"
                                  , bool optDecorate  "decorate"
                                  , bool optCode      "code"
                                  , bool optDl        "dl"
                                  , bool optHeaders   "headers"
                                  , bool optLink      "link"
                                  , bool optBq        "bq"
                                  , bool optPlainText "plaintext"
                                  , bool optOl        "ol"
                                  , bool optUl        "ul"
                                  , Just $ show optParagraphs
                                  , Just . map toLower $ show optSize
                                  ]
    where bool True  x = Just x
          bool False _ = Nothing

download :: String -> IO BS.ByteString
download = fmap (view responseBody) . get


main :: IO ()
main = print =<< download . makeUrl =<< execParser ipsumOpts


data IpsumSize = Short | Medium | Long | VeryLong
               deriving (Show, Read, Eq)

data OutputDest = Print | Copy | Both
                deriving (Show, Read, Eq)

data IpsumOpts
    = IpsumOpts
    { optParagraphs :: Int
    , optSize       :: IpsumSize
    , optAllCaps    :: Bool
    , optDecorate   :: Bool
    , optCode       :: Bool
    , optDl         :: Bool
    , optHeaders    :: Bool
    , optLink       :: Bool
    , optBq         :: Bool
    , optPlainText  :: Bool
    , optOl         :: Bool
    , optUl         :: Bool
    , optOutput     :: OutputDest
    } deriving (Show)

ipsumOpts' :: Parser IpsumOpts
ipsumOpts' =   IpsumOpts
           <$> option (  short 'p' <> long "paragraphs" <> metavar "PARAGRAPHS"
                          <> value 4
                          <> help "The number of paragraphs to generate. Defaults to 4.")
           <*> option (  short 's' <> long "size" <> metavar "SIZE"
                          <> value Medium
                          <> help "The size of paragraphs. Options are Short, \
                                  \Medium, Long, VeryLong. Defaults to Medium.")
           <*> switch (  short 'a' <> long "allcaps" <> help "Use ALL CAPS.")
           <*> switch (  short 'b' <> long "decorate"
                      <> help "Add decorated text, including bold, italic and mark.")
           <*> switch (  short 'c' <> long "code" <> help "Add code samples.")
           <*> switch (  short 'd' <> long "dl" <> help "Add definition lists.")
           <*> switch (  short 'H' <> long "headers" <> help "Add headers.")
           <*> switch (  short 'l' <> long "link" <> help "Add links.")
           <*> switch (  short 'q' <> long "bq" <> help "Add blockquotes.")
           <*> switch (  short 't' <> long "plaintext" <> help "Return plain text, no HTML.")
           <*> switch (  short 'o' <> long "ol" <> help "Add ordered lists.")
           <*> switch (  short 'u' <> long "ul" <> help "Add unordered lists.")
           <*> option (  short 'O' <> long "output" <> value Copy <> help "Output option.")

ipsumOpts :: ParserInfo IpsumOpts
ipsumOpts = info (helper <*> ipsumOpts')
                 (  fullDesc
                 <> progDesc "Copies lorem ipsum text from the Loripsum.net API."
                 <> O.header "copyipsum - Copies lorem ipsum text from the Loripsum.net API.")

