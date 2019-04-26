{-|
Module      : Kore.Step.SMT.Lemma
Description : Declares all rules marked smt-lemma to the SMT solver.
Copyright   : (c) Runtime Verification, 2019
License     : NCSA
Maintainer  : phillip.harris@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.SMT.Lemma
    ( declareSMTLemmas
    ) where

import           Control.Applicative
                 ( Alternative (..) )
import qualified Control.Comonad.Trans.Cofree as Cofree
import           Control.Error
                 ( runMaybeT )
import qualified Control.Monad.Counter as Counter
import           Control.Monad.Except
import qualified Control.Monad.State as State
import qualified Data.Functor.Foldable as Recursive
import qualified Data.Map.Strict as Map
import           Data.Reflection
import qualified Data.Text as Text

import           Kore.AST.Pure
import           Kore.AST.Sentence
import qualified Kore.Attribute.Axiom as Attribute
import           Kore.Attribute.SmtLemma
import           Kore.Attribute.Symbol
import           Kore.IndexedModule.IndexedModule
import           Kore.IndexedModule.MetadataTools
import           Kore.Predicate.Predicate
import           Kore.Step.Pattern
import qualified Kore.Step.SMT.Declaration.All as SMT.All
                 ( declare )
import           Kore.Step.SMT.Translate
import           Kore.Unparser
import           SMT
                 ( MonadSMT, SExpr (..), SMT )
import qualified SMT

-- | Given an indexed module, `declareSMTLemmas` translates all
-- rewrite rules marked with the smt-lemma attribute into the
-- smt2 standard, and sends them to the current SMT solver.
-- It assumes that all symbols in all smt-lemma rules either have been
-- declared in the smt prelude or they have an smtlib attribute.
declareSMTLemmas
    :: forall m
    .   ( MonadSMT m
        , Given (SmtMetadataTools StepperAttributes)
        )
    => VerifiedModule StepperAttributes Attribute.Axiom
    -> m ()
declareSMTLemmas m = SMT.liftSMT $ do
    SMT.All.declare (smtData tools)
    mapM_ declareRule (indexedModuleAxioms m)
  where
    tools :: SmtMetadataTools StepperAttributes
    tools = given

    declareRule
        :: forall sortParam . (Given (SmtMetadataTools StepperAttributes))
        => ( Attribute.Axiom
           , SentenceAxiom sortParam (StepPattern Object Variable)
           )
        -> SMT (Maybe ())
    declareRule (atts, axiomDeclaration) = runMaybeT $ do
        guard (isSmtLemma $ Attribute.smtLemma atts)
        (lemma, vars) <-
            runTranslator
            $ translatePredicate translateUninterpreted
            $ wrapPredicate $ sentenceAxiomPattern axiomDeclaration
        SMT.assert (addQuantifiers vars lemma)

    addQuantifiers vars lemma | null vars = lemma
    addQuantifiers vars lemma = SMT.List
        [ SMT.Atom "forall"
        , SMT.List
            [ SMT.List [sexpr, t] | (sexpr, t) <- Map.elems vars ]
        , lemma
        ]

translateUninterpreted
    :: ( Ord p
       , p ~ StepPattern Object variable
       , Unparse (variable Object)
       )
    => SExpr  -- ^ type name
    -> p  -- ^ uninterpreted pattern
    -> Translator p SExpr
translateUninterpreted t pat | isVariable pat =
    lookupPattern <|> freeVariable
  where
    isVariable p =
        case Cofree.tailF $ Recursive.project p of
            VariablePattern _ -> True
            _ -> False
    lookupPattern = do
        result <- State.gets $ Map.lookup pat
        maybe empty (return . fst) result
    freeVariable = do
        n <- Counter.increment
        let var = SMT.Atom ("<" <> Text.pack (show n) <> ">")
        State.modify' (Map.insert pat (var, t))
        return var
translateUninterpreted _ _ = empty
