{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


-- TODO: Documentation

module Main where


import           Control.Monad
import qualified Data.ByteString.Lazy       as BS
import qualified Data.ByteString.Lazy.Char8 as B8
import           Data.Char
import qualified Data.List                  as L
import           Data.Maybe
import           System.Exit
import           System.IO
import Control.Monad.Writer

import           Control.Lens
import           Network.Wreq
import           Options.Applicative        hiding (header)
import qualified Options.Applicative        as O
import           System.Process


-- TODO: Use [((IpsumOpts -> Bool, String)] to pull in the flags.
makeUrl :: IpsumOpts -> String
makeUrl IpsumOpts{..} = execWriter $ do
    tell "http://loripsum.net/api"
    api optAllCaps   "allcaps"
    api optDecorate  "decorate"
    api optCode      "code"
    api optDl        "dl"
    api optHeaders   "headers"
    api optLink      "link"
    api optBq        "bq"
    api optPlainText "plaintext"
    api optOl        "ol"
    api optUl        "ul"
    tell $ '/' : show optParagraphs
    tell . ('/':) . map toLower $ show optSize
    where api opt name = when opt $ tell "/" >> tell name

download :: String -> IO BS.ByteString
download = fmap (view responseBody) . get

doCopy :: OutputDest -> Bool
doCopy Print = False
doCopy _     = True

doPrint :: OutputDest -> Bool
doPrint Copy = False
doPrint _    = True

-- TODO: Use Shelly?
copy :: BS.ByteString -> IO ()
copy text = do
    (Just std_in, _, _, p) <- createProcess (proc "pbcopy" []) { std_in = CreatePipe }
    B8.hPut std_in text
    hClose std_in
    ec <- waitForProcess p
    case ec of
        ExitSuccess     -> return ()
        ExitFailure ec' -> hPutStrLn stderr $ "ERROR: " ++ show ec'


main :: IO ()
main = do
    opts@IpsumOpts{optOutput} <- execParser ipsumOpts
    let url = makeUrl opts
    when (optPrintUrl opts) $
        putStrLn url
    text <- download url
    when (doPrint optOutput) $
        B8.putStrLn text
    when (doCopy optOutput) $
        copy text


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
    , optPrintUrl   :: Bool
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
           <*> switch (  short 'P' <> long "print-url" <> help "Print the URL.")

ipsumOpts :: ParserInfo IpsumOpts
ipsumOpts = info (helper <*> ipsumOpts')
                 (  fullDesc
                 <> progDesc "Copies lorem ipsum text from the Loripsum.net API."
                 <> O.header "copyipsum - Copies lorem ipsum text from the Loripsum.net API.")

