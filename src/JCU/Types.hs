{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module JCU.Types where

import            Control.Applicative
import            Data.Aeson as AE
import qualified  Data.Attoparsec.Lazy as AP
import qualified  Data.ByteString.Lazy.Char8 as LBS
import qualified  Data.ByteString.Char8 as BS
import            Data.ListLike (CharStringLazy(..))
import            Data.String
import            Data.Tree (Tree(..))
import            Language.Prolog.NanoProlog.NanoProlog
import            Language.Prolog.NanoProlog.Parser
import            Text.ParserCombinators.UU.BasicInstances (Parser(), Error, LineColPos)


data DBRule = DBRule {
     ruleId     :: Int
  ,  ruleOrder  :: Int
  ,  rule       :: Rule
}

data DropReq   =  DropReq Proof [Int] Rule
               deriving Show

data DropRes   =  DropRes Bool Proof
               deriving Show

data Status    =  Correct
               |  Incomplete
               |  Invalid
               deriving Show

type Proof     =  Tree Term
type PCheck    =  Tree Status
type ErrorMsg  =  BS.ByteString


instance ToJSON DBRule where
  toJSON (DBRule rid ro rl) = object  [ "id"    .= rid
                                      , "ro"    .= ro
                                      , "rule"  .= show rl]

instance FromJSON DropReq where
  parseJSON (Object o)  = mkJSONDropReq  <$>  o .: "proof"
                                         <*>  o .: "treeLvl"
                                         <*>  o .: "rule"

  parseJSON val         = fail $ "No case for (FromJSON DropReq) with value: " ++ show val

mkJSONDropReq :: Value -> [Int] -> LBS.ByteString -> DropReq
mkJSONDropReq prf lvl rl = DropReq mkProofTree lvl (mkJSONRule rl)
  where mkProofTree = case fromJSON prf :: AE.Result Proof of
                        (AE.Success a)  -> a
                        (AE.Error err)  -> error ("Error parsing drop request: " ++ err)

instance ToJSON DropRes where
  toJSON (DropRes ufd prf) = object ["unified" .= ufd, "nproof" .= prf]

instance ToJSON PCheck where
  toJSON (Node st cs) = object  [  "proofCheckResult"    .= show st
                                ,  "proofCheckChildren"  .= toJSON cs ]

instance ToJSON Rule where
  toJSON t = object [ "rule" .= show t ]

instance FromJSON Rule where
  parseJSON (Object o)  = mkJSONRule <$> o .: "rule"
  parseJSON val         = fail $ "No case for (FromJSON Rule) with value: " ++ show val

-- TODO: Errors
mkJSONRule :: LBS.ByteString -> Rule
mkJSONRule = fst . startParse pRule . CSL

instance FromJSON Proof where
  parseJSON (Object o)  = mkJSONProofTree <$> o .: "term" <*> o .: "childTerms"
  parseJSON val         = fail $ "No case for (FromJSON Proof) with value: " ++ show val

instance ToJSON Proof where
  toJSON (Node t ps) = object  [  "term"        .= show t
                               ,  "childTerms"  .= toJSON ps ]

mkJSONProofTree :: LBS.ByteString -> Value -> Proof
mkJSONProofTree tm rts = Node (mkJSONTerm tm) mkProofTrees
  where mkProofTrees = case fromJSON rts :: AE.Result [Proof] of
                         (AE.Success a)  -> a
                         (AE.Error err)  -> error ("Error parsing proof tree: " ++ err)


-- TODO: Something with errors
mkJSONTerm :: LBS.ByteString -> Term
mkJSONTerm = fst . startParse pTerm . CSL

mkRule :: LBS.ByteString -> Either ErrorMsg Rule
mkRule = processJSON fromJSON

mkDropReq :: LBS.ByteString -> Either ErrorMsg DropReq
mkDropReq = processJSON fromJSON

mkProof :: LBS.ByteString -> Either ErrorMsg Proof
mkProof = processJSON fromJSON


processJSON :: (Value -> AE.Result a) -> LBS.ByteString -> Either ErrorMsg a
processJSON f raw =
  case AP.parse json raw of
    (AP.Done _ r)  ->
      case f r of
        (AE.Success a)  -> Right a
        (AE.Error err)  -> Left . fromString $ "Error converting ByteString to data type: " ++ err
    (AP.Fail _ _ err) -> Left . fromString $ "Error parsing raw JSON: " ++ err

-- TODO: Try to get rid of the explicit annotations...
parseCheck :: Maybe BS.ByteString -> LBS.ByteString -> (Bool, [ErrorMsg])
parseCheck Nothing   _     = checkErr [fromString "Unknown error."]
parseCheck (Just x)  body
  | x == "rule"  = parseMsg pRule body
  | x == "term"  = parseMsg pTerm body
  | otherwise    = checkErr [fromString "Invalid type specified"]
  where  parseMsg :: Parser t -> LBS.ByteString -> (Bool, [ErrorMsg])
         parseMsg p txt = writeRes $ startParse p (CSL txt)
         writeRes :: (t, [Error LineColPos]) -> (Bool, [ErrorMsg])
         writeRes (_, [])  = (True, [])
         writeRes (_, rs)  = checkErr (map (fromString . show) rs)

checkErr :: [ErrorMsg] -> (Bool, [ErrorMsg])
checkErr msg = (False, msg)
