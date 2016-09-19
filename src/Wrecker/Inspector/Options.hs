{-# LANGUAGE RecordWildCards, CPP #-}
module Wrecker.Inspector.Options where
import qualified Wrecker.Options as W
import Options.Applicative.Builder
import Options.Applicative
import Control.Exception
#if __GLASGOW_HASKELL__ < 800
import Data.Monoid
#endif

data Options = Options 
  { harFilePath    :: FilePath
  , wreckerOptions :: W.Options
  } deriving (Show, Eq)
  
data PartialOptions = PartialOptions
  { mHarFilePath    :: Maybe FilePath
  , mWreckerOptions :: Maybe W.PartialOptions
  } deriving (Show, Eq)
  
instance Monoid PartialOptions where
  mempty = PartialOptions { mHarFilePath = Nothing, mWreckerOptions = mempty }
  mappend x y = PartialOptions 
    { mHarFilePath    = mHarFilePath    x <|> mHarFilePath    y
    , mWreckerOptions = mWreckerOptions x <|> mWreckerOptions y
    } 
  
optionalOption :: Read a => Mod OptionFields a -> Parser (Maybe a)
optionalOption = optional . option auto  
  
pPartialOptions :: Parser PartialOptions
pPartialOptions 
   =  PartialOptions 
  <$> optionalOption 
      (  long "file-path"
      <> help "HAR file path"
      )
  <*> optional W.pPartialOptions

completeOptions :: PartialOptions -> Maybe Options
completeOptions options = case mempty <> options of
  PartialOptions 
    { mHarFilePath    = Just harFilePath
    , mWreckerOptions = Just partialWreckerOptions
    } -> Options harFilePath 
     <$> W.completeOptions partialWreckerOptions
  _ -> Nothing
      
runParser :: IO Options
runParser = do 
  let opts = info (helper <*> pPartialOptions)
               ( fullDesc
               <> progDesc "Welcome to inspector-wrecker"
               <> header "inspector-wrecker - HTTP stress tester and benchmarker with HAR reader" 
               )
  
  partialOptions <- execParser opts 
  case completeOptions partialOptions of
    Nothing -> throwIO $ userError "Missing file-path argument!"
    Just  x -> return x
