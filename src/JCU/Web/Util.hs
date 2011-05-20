{-# LANGUAGE OverloadedStrings #-}

module JCU.Web.Util where

import            Data.ByteString as B (ByteString, length)
import            Data.ByteString.Char8 as B (unpack, pack)
import            Data.List as DL (length)
import            Data.Tree (Tree(..))
import            JCU.Prolog.Prolog
import            JCU.Prolog.Types
import            JCU.Web.Types
import            Text.Email.Validate as E (isValid)

getRhss :: Term -> Rule -> DropRes
getRhss t (c :<-: cs) =
  case unify (t, c) (Just []) of
    Nothing  -> (False, 0)
    Just _   -> (True, DL.length cs)


type FormValidator = [(ByteString, FormField)]
data FormField = FormField  {  isRequired    :: Bool
                            ,  fldValidator  :: ByteString -> Bool }

-- TODO: Add support for multiple parameters with the same name
-- TODO: Add support for returning validation errors.
-- TODO: See what the Digestive Functors can do for form validation... it is
-- much better suited for validation than this...
formValidator :: FormValidator
formValidator =  [  ("email",     FormField True (E.isValid . unpack))
                 ,  ("password",  FormField True (\xs -> B.length xs >= 6)) ]

