{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Evaluator
import Parser
import System.IO
import GHC.IO.Encoding
import Web.Scotty hiding (headers)
import Types
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import Data.Text
import Data.Maybe (fromJust)
import Text.Megaparsec.Error (errorBundlePretty)

data ArgumentJSON = ArgumentJSON { argument :: String } deriving (Show, Generic)



data HaskellServerResponse = Table
                     { err :: String
                     , headers :: [String]
                     , assignments :: [(String, [Bool])]
                     , premiseEval :: [[(String, Bool)]]
                     , conclusionEval :: [(String, Bool)]
                     , validityy :: String
                     }
                     deriving (Show, Generic)


{-
data HaskellServerResponse = Table
                           { err :: String
                           , headers :: String
                           , assignments :: String
                           , premiseEval :: String
                           , conclusionEval :: String
                           , validityy :: String
                           } deriving (Show, Generic)
-}
instance FromJSON ArgumentJSON
instance ToJSON ArgumentJSON
instance FromJSON HaskellServerResponse
instance ToJSON HaskellServerResponse


--------------
-- This stuff is all just for one function called truthTableToResponse

generateHeaders :: (Either Argument Premises, String) -> [String]
generateHeaders (e , vars)  =
    case e of
     (Right premises) ->
                         g premises vars
                         where g p vars = case vars of
                                           [] -> case p of
                                                  [] -> []
                                                  (x : xs)  -> (show x) : (g xs [])
                                           (v : vs) -> [v] : (g p vs)

     (Left (premises,conclusion)) -> f premises conclusion vars
                                     where f p c vars =    case vars of
                                                            [] -> case p of
                                                                   [] -> [show c]
                                                                   (x : xs)  -> (show x) : (f xs c [])
                                                            (v : vs) -> [v] :  (f p c vs)


fromValidity :: Maybe Validity -> String
fromValidity Nothing = "Nothing"
fromValidity v = show (Data.Maybe.fromJust v)

------------------------------------------

-- function that takes in natural premises, a truth table, and create a repsonse

truthTableToResponse :: TruthTable -> HaskellServerResponse
truthTableToResponse t = Table
                            { err = []
                            , headers =  (generateHeaders (Types.eap t , Types.variables t))
                            , Main.assignments = Types.assignments t
                            , premiseEval = (Prelude.map (Prelude.map (\(p,e) ->  (show p, Data.Maybe.fromJust e) )) (Types.everyPremiseEval t))
                            , conclusionEval =  (case Types.everyConclusionEval t of
                                                       Nothing -> []
                                                       (Just l) -> Prelude.map (\(p,e) -> (show p, Data.Maybe.fromJust e) ) (l))
                            , validityy =  (fromValidity $ Types.validity t)
                            }
{-

data TruthTable = TruthTable
                { eap :: Either Argument Premises
                , variables :: String
                , assignments :: [(String, [Bool])]
                , everyPremiseEval :: [[(Proposition, Maybe Bool)]]
                , everyConclusionEval :: Maybe [(Conclusion, Maybe Bool)]
                , validity :: Maybe Validity
                }
                deriving (Eq, Show)
-}


main :: IO ()
main = scotty 3000 $ do
       get "/" $ file "static/elmStatic/src/index.html"
       post "/api/submit" $ do
           body' <- jsonData :: ActionM ArgumentJSON
           let argString = argument body' :: String
           let result = runArgument argString
           case result of
            (Left err', _ ) -> do
                                setHeader "Content-Type" "application/json"
                                json $ Table { err = errorBundlePretty err'
                                             , headers = []
                                             , Main.assignments = []
                                             , premiseEval = []
                                             , conclusionEval = []
                                             , validityy = ""
                                             }
            (Right e, _) -> do
                        let truthtable = generateTruthTable e :: TruthTable
                        json $ truthTableToResponse truthtable
