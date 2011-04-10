{-# LANGUAGE OverloadedStrings  #-}

module JCU.Web.Prolog where

import JCU.Prolog.Prolog
import JCU.Web.Types


-- To be able to do anything, we need:
-- - A set of rules
-- - One or more queries
--
-- Rules are stored on a per-user basis. One user has a set of rules.
-- A user can add to those rules. The goal is to stack those rules, rewrite
-- them and where needed make up new rules to find the answer to a query.
--
-- Adding is done by adding a row to the rule-list on the front-end.
-- An asynchronous call adds it to the user's rule set on the server.
--
-- A user can do several things on the front-end.
-- Initially the user's rule-list is empty, so the user needs to add rules.
-- Once rules have been entered, the user can proceed to rewrite a query.
--
-- A textfield is presented to enter the original query. No hints or solutions
-- can be given until this first query is entered.
--
-- After the first rule is entered, the user can do several things.
--
-- A hint can be given as to which term is next. This requires the current set
-- of rules in the rewrite-stack to be submitted to the server. On the server,
-- the current user and his rule-list are already known.
--
-- The process of checking whether the last entry is correct is a subset of the
-- process which gives a hint as to which term is next.
--
-- A naive approach: we take the query and solve it using the existing API.
-- We then compare the next term to the one in the list we got from the client-
-- side. If they don't match, we return an empty set of next rules. If they do
-- match, we look at the set of rules with which the query can unify. We then
-- compare this set with the next term on the client listh
