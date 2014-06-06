{-# LANGUAGE OverloadedStrings #-}


module Main where


import Options.Applicative


main :: IO ()
main = print =<< execParser ipsumOpts


data IpsumSize = Short | Medium | Long | VeryLong
               deriving (Show, Read, Eq)

data OutputDest = Print | Copy | Both
                deriving (Show, Read, Eq)

data IpsumOpts
    = IpsumOpts
    { paragraphs :: Int
    , size       :: IpsumSize
    , allCaps    :: Bool
    , decorate   :: Bool
    , code       :: Bool
    , dl         :: Bool
    , headers    :: Bool
    , link       :: Bool
    , bq         :: Bool
    , plainText  :: Bool
    , ol         :: Bool
    , ul         :: Bool
    , output     :: OutputDest
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
                 <> header "copyipsum - Copies lorem ipsum text from the Loripsum.net API.")

