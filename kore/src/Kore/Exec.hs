{- |
Module      : Kore.Exec
Description : Expose concrete execution as a library
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause
Stability   : experimental
Portability : portable

Expose concrete execution as a library
-}
module Kore.Exec (
    exec,
    search,
    prove,
    proveWithRepl,
    boundedModelCheck,
    matchDisjunction,
    checkFunctions,
    simplify,
    Rewrite,
    Equality,
    Initialized,
    makeSerializedModule,
    SerializedModule (..),
) where

import Control.Concurrent.MVar
import Control.DeepSeq (
    deepseq,
 )
import Control.Error (
    hoistMaybe,
 )
import Control.Lens qualified as Lens
import Control.Monad (
    filterM,
    (>=>),
 )
import Control.Monad.Catch (
    MonadMask,
 )
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Coerce (
    coerce,
 )
import Data.Limit (
    Limit (..),
 )
import Data.List (
    tails,
 )
import Data.Map.Strict (
    Map,
 )
import Data.Map.Strict qualified as Map
import GHC.Generics qualified as GHC
import Kore.Attribute.Axiom qualified as Attribute
import Kore.Attribute.Definition
import Kore.Attribute.Symbol (
    StepperAttributes,
 )
import Kore.Attribute.Symbol qualified as Attribute
import Kore.Builtin qualified as Builtin
import Kore.Equation (
    Equation,
    extractEquations,
    isSimplificationRule,
    right,
 )
import Kore.Equation qualified as Equation (
    Equation (antiLeft),
    argument,
    requires,
 )
import Kore.IndexedModule.IndexedModule (
    IndexedModule (..),
    VerifiedModule,
    VerifiedModuleSyntax,
 )
import Kore.IndexedModule.IndexedModule qualified as IndexedModule
import Kore.IndexedModule.MetadataTools (
    SmtMetadataTools,
 )
import Kore.IndexedModule.MetadataToolsBuilder qualified as MetadataTools (
    build,
 )
import Kore.IndexedModule.OverloadGraph
import Kore.IndexedModule.OverloadGraph qualified as OverloadGraph
import Kore.IndexedModule.Resolvers (
    resolveInternalSymbol,
 )
import Kore.IndexedModule.SortGraph
import Kore.IndexedModule.SortGraph qualified as SortGraph
import Kore.Internal.Condition qualified as Condition
import Kore.Internal.InternalInt
import Kore.Internal.MultiOr qualified as MultiOr
import Kore.Internal.OrPattern qualified as OrPattern
import Kore.Internal.Pattern (
    Pattern,
 )
import Kore.Internal.Pattern qualified as Pattern
import Kore.Internal.Predicate (
    fromPredicate,
    makeMultipleOrPredicate,
 )
import Kore.Internal.Predicate qualified as Predicate
import Kore.Internal.SideCondition qualified as SideCondition
import Kore.Internal.TermLike
import Kore.Log.ErrorEquationRightFunction (
    errorEquationRightFunction,
 )
import Kore.Log.ErrorEquationsSameMatch (
    errorEquationsSameMatch,
 )
import Kore.Log.ErrorRewriteLoop (
    errorRewriteLoop,
 )
import Kore.Log.InfoExecDepth
import Kore.Log.KoreLogOptions (
    KoreLogOptions (..),
 )
import Kore.Log.WarnDepthLimitExceeded (
    warnDepthLimitExceeded,
 )
import Kore.Log.WarnTrivialClaim
import Kore.ModelChecker.Bounded qualified as Bounded
import Kore.Reachability (
    AllClaims (AllClaims),
    AlreadyProven (AlreadyProven),
    Axioms (Axioms),
    MinDepth,
    ProveClaimsResult (..),
    Rule (ReachabilityRewriteRule),
    SomeClaim (..),
    StuckCheck (..),
    ToProve (ToProve),
    extractClaims,
    isTrusted,
    lensClaimPattern,
    proveClaims,
 )
import Kore.Repl qualified as Repl
import Kore.Repl.Data qualified as Repl.Data
import Kore.Rewrite
import Kore.Rewrite.Axiom.Identifier (
    AxiomIdentifier,
 )
import Kore.Rewrite.RewritingVariable
import Kore.Rewrite.Rule (
    extractImplicationClaims,
    extractRewriteAxioms,
 )
import Kore.Rewrite.Rule.Expand (
    ExpandSingleConstructors (..),
 )
import Kore.Rewrite.Rule.Simplify (
    SimplifyRuleLHS (..),
 )
import Kore.Rewrite.Rule.Simplify qualified as Rule
import Kore.Rewrite.RulePattern (
    ImplicationRule (..),
    RewriteRule (..),
    getRewriteRule,
    lhsEqualsRhs,
    mapRuleVariables,
 )
import Kore.Rewrite.RulePattern as RulePattern (
    RulePattern (..),
 )
import Kore.Rewrite.Search (
    searchGraph,
 )
import Kore.Rewrite.Search qualified as Search
import Kore.Rewrite.Strategy (FinalNodeType (Leaf))
import Kore.Rewrite.Strategy qualified as Strategy
import Kore.Rewrite.Transition (
    runTransitionT,
    scatter,
 )
import Kore.Simplify.Data (
    evalSimplifier,
    evalSimplifierProofs,
 )
import Kore.Simplify.Data qualified as Simplifier
import Kore.Simplify.Pattern qualified as Pattern
import Kore.Simplify.Simplify (
    MonadSimplify,
    SimplifierXSwitch,
 )
import Kore.Syntax.Module (
    ModuleName,
 )
import Kore.TopBottom (
    isBottom,
 )
import Kore.Unparser (
    unparseToText,
    unparseToText2,
 )
import Log (
    MonadLog,
 )
import Log qualified
import Logic (
    LogicT,
    observeAllT,
 )
import Logic qualified
import Prelude.Kore
import Prof
import SMT (
    MonadSMT,
    SMT,
 )
import System.Exit (
    ExitCode (..),
 )

-- | Semantic rule used during execution.
type Rewrite = RewriteRule RewritingVariableName

-- | Function rule used during execution.
type Equality = Equation VariableName

-- | A collection of rules and simplifiers used during execution.
newtype Initialized = Initialized {rewriteRules :: [Rewrite]}
    deriving stock (GHC.Generic)
    deriving anyclass (NFData)

data SerializedModule = SerializedModule
    { sortGraph :: SortGraph
    , overloadGraph :: OverloadGraph
    , metadataTools :: SmtMetadataTools StepperAttributes
    , verifiedModule :: VerifiedModuleSyntax StepperAttributes
    , rewrites :: Initialized
    , equations :: Map AxiomIdentifier [Equation VariableName]
    }
    deriving stock (GHC.Generic)
    deriving anyclass (NFData)

makeSerializedModule ::
    forall smt.
    ( MonadIO smt
    , MonadLog smt
    , MonadSMT smt
    , MonadMask smt
    , MonadProf smt
    ) =>
    SimplifierXSwitch ->
    VerifiedModule StepperAttributes ->
    smt SerializedModule
makeSerializedModule simplifierx verifiedModule =
    evalSimplifier simplifierx (indexedModuleSyntax verifiedModule') sortGraph overloadGraph metadataTools equations $ do
        rewrites <- initializeAndSimplify verifiedModule
        return
            SerializedModule
                { sortGraph
                , overloadGraph
                , metadataTools
                , verifiedModule = indexedModuleSyntax verifiedModule
                , rewrites
                , equations
                }
  where
    sortGraph = SortGraph.fromIndexedModule verifiedModule
    overloadGraph = OverloadGraph.fromIndexedModule verifiedModule
    earlyMetadataTools = MetadataTools.build verifiedModule
    verifiedModule' =
        IndexedModule.mapPatterns
            (Builtin.internalize earlyMetadataTools)
            verifiedModule
    metadataTools = MetadataTools.build verifiedModule'
    equations = extractEquations verifiedModule'

-- | Symbolic execution
exec ::
    forall smt.
    ( MonadIO smt
    , MonadLog smt
    , MonadSMT smt
    , MonadMask smt
    , MonadProf smt
    ) =>
    SimplifierXSwitch ->
    Limit Natural ->
    Limit Natural ->
    -- | The main module
    SerializedModule ->
    ExecutionMode ->
    -- | The input pattern
    TermLike VariableName ->
    smt (ExitCode, TermLike VariableName)
exec
    simplifierx
    depthLimit
    breadthLimit
    SerializedModule
        { sortGraph
        , overloadGraph
        , metadataTools
        , verifiedModule
        , rewrites
        , equations
        }
    strategy
    (mkRewritingTerm -> initialTerm) =
        evalSimplifier simplifierx verifiedModule' sortGraph overloadGraph metadataTools equations $ do
            let Initialized{rewriteRules} = rewrites
            finals <-
                getFinalConfigsOf $ do
                    initialConfig <-
                        Pattern.simplify
                            (Pattern.fromTermLike initialTerm)
                            >>= Logic.scatter
                    let updateQueue = \as ->
                            Strategy.unfoldDepthFirst as
                                >=> lift
                                    . Strategy.applyBreadthLimit
                                        breadthLimit
                                        dropStrategy
                        rewriteGroups = groupRewritesByPriority rewriteRules
                        transit instr config =
                            Strategy.transitionRule
                                ( transitionRule rewriteGroups strategy
                                    & profTransitionRule
                                    & trackExecDepth
                                )
                                instr
                                config
                                & runTransitionT
                                & fmap (map fst)
                                & lift
                    Strategy.leavesM
                        Leaf
                        updateQueue
                        (unfoldTransition transit)
                        ( limitedExecutionStrategy depthLimit
                        , (ExecDepth 0, Start initialConfig)
                        )
            let (depths, finalConfigs) = unzip finals
            infoExecDepth (maximum (ExecDepth 0 : depths))
            let finalConfigs' =
                    MultiOr.make $
                        mapMaybe extractProgramState finalConfigs
            exitCode <- getExitCode verifiedModule finalConfigs'
            let finalTerm =
                    MultiOr.map getRewritingPattern finalConfigs'
                        & OrPattern.toTermLike initialSort
                        & sameTermLikeSort initialSort
            return (exitCode, finalTerm)
      where
        dropStrategy = snd
        getFinalConfigsOf act = observeAllT $ fmap snd act
        verifiedModule' =
            IndexedModule.mapAliasPatterns
                -- TODO (thomas.tuegel): Move this into Kore.Builtin
                (Builtin.internalize metadataTools)
                verifiedModule
        initialSort = termLikeSort initialTerm
        unfoldTransition transit (instrs, config) = do
            when (null instrs) $ forM_ depthLimit warnDepthLimitExceeded
            Strategy.unfoldTransition transit (instrs, config)

-- | Modify a 'TransitionRule' to track the depth of the execution graph.
trackExecDepth ::
    TransitionRule monad rule state ->
    TransitionRule monad rule (ExecDepth, state)
trackExecDepth transit prim (execDepth, execState) = do
    execState' <- transit prim execState
    let execDepth' = (if didRewrite execState' then succ else id) execDepth
    pure (execDepth', execState')
  where
    didRewrite _ = isRewrite prim

    isRewrite Rewrite = True
    isRewrite _ = False

-- | Add profiling markers to a 'TransitionRule'.
profTransitionRule ::
    forall monad rule state.
    MonadProf monad =>
    TransitionRule monad rule state ->
    TransitionRule monad rule state
profTransitionRule rule prim proofState =
    case prim of
        Rewrite -> Just ":rewrite:"
        Simplify -> Just ":simplify:"
        Begin -> Nothing
        & \case
            Just marker -> lift (traceProf marker (runTransitionT go)) >>= scatter
            Nothing -> go
  where
    go = rule prim proofState

-- | Project the value of the exit cell, if it is present.
getExitCode ::
    forall simplifier.
    (MonadIO simplifier, MonadSimplify simplifier) =>
    -- | The main module
    VerifiedModuleSyntax StepperAttributes ->
    -- | The final configuration(s) of execution
    OrPattern.OrPattern RewritingVariableName ->
    simplifier ExitCode
getExitCode
    indexedModule
    configs =
        takeExitCode $ \mkExitCodeSymbol -> do
            let mkGetExitCode t = mkApplySymbol (mkExitCodeSymbol []) [t]
            exitCodePatterns <-
                do
                    config <- Logic.scatter configs
                    Pattern.simplifyTopConfiguration (mkGetExitCode <$> config)
                        >>= Logic.scatter
                    & MultiOr.observeAllT
            let exitCode =
                    case toList (MultiOr.map Pattern.term exitCodePatterns) of
                        [exitTerm] -> extractExit exitTerm
                        _ -> ExitFailure 111
            return exitCode
      where
        extractExit = \case
            InternalInt_ InternalInt{internalIntValue = exit}
                | exit == 0 -> ExitSuccess
                | otherwise -> ExitFailure (fromInteger exit)
            _ -> ExitFailure 111

        resolve =
            resolveInternalSymbol indexedModule
                . noLocationId

        takeExitCode ::
            (([Sort] -> Symbol) -> simplifier ExitCode) ->
            simplifier ExitCode
        takeExitCode act =
            resolve "LblgetExitCode"
                & maybe (pure ExitSuccess) act

-- | Symbolic search
search ::
    ( MonadIO smt
    , MonadLog smt
    , MonadSMT smt
    , MonadMask smt
    , MonadProf smt
    ) =>
    SimplifierXSwitch ->
    Limit Natural ->
    Limit Natural ->
    -- | The main module
    SerializedModule ->
    -- | The input pattern
    TermLike VariableName ->
    -- | The pattern to match during execution
    Pattern VariableName ->
    -- | The bound on the number of search matches and the search type
    Search.Config ->
    smt (TermLike VariableName)
search
    simplifierx
    depthLimit
    breadthLimit
    SerializedModule
        { sortGraph
        , overloadGraph
        , metadataTools
        , verifiedModule
        , rewrites
        , equations
        }
    (mkRewritingTerm -> termLike)
    searchPattern
    searchConfig =
        evalSimplifier simplifierx verifiedModule sortGraph overloadGraph metadataTools equations $ do
            let Initialized{rewriteRules} = rewrites
            simplifiedPatterns <-
                Pattern.simplify $
                    Pattern.fromTermLike termLike
            let initialPattern =
                    case toList simplifiedPatterns of
                        [] -> Pattern.bottomOf (termLikeSort termLike)
                        (config : _) -> config
                rewriteGroups =
                    groupRewritesByPriority rewriteRules
                runStrategy' =
                    runStrategy
                        breadthLimit
                        -- search relies on exploring
                        -- the entire space of states.
                        ( transitionRule rewriteGroups All
                            & profTransitionRule
                        )
                        (limitedExecutionStrategy depthLimit)
            executionGraph <-
                runStrategy' (Start initialPattern)
            let match target config1 config2 = do
                    extracted <- hoistMaybe $ extractProgramState config2
                    Search.matchWith target config1 extracted
            solutionsLists <-
                searchGraph
                    searchConfig
                    ( match
                        SideCondition.top
                        (mkRewritingPattern searchPattern)
                    )
                    executionGraph
            let solutions = concatMap toList solutionsLists
                orPredicate =
                    makeMultipleOrPredicate (Condition.toPredicate <$> solutions)
            return
                . sameTermLikeSort patternSort
                . getRewritingTerm
                . fromPredicate patternSort
                $ orPredicate
      where
        patternSort = termLikeSort termLike

-- | Proving a spec given as a module containing rules to be proven
prove ::
    forall smt.
    ( MonadLog smt
    , MonadMask smt
    , MonadIO smt
    , MonadSMT smt
    , MonadProf smt
    ) =>
    Maybe MinDepth ->
    StuckCheck ->
    SimplifierXSwitch ->
    Strategy.GraphSearchOrder ->
    Limit Natural ->
    Limit Natural ->
    Natural ->
    FinalNodeType ->
    -- | The main module
    VerifiedModule StepperAttributes ->
    -- | The spec module
    VerifiedModule StepperAttributes ->
    -- | The module containing the claims that were proven in a previous run.
    Maybe (VerifiedModule StepperAttributes) ->
    smt ProveClaimsResult
prove
    maybeMinDepth
    stuckCheck
    simplifierx
    searchOrder
    breadthLimit
    depthLimit
    maxCounterexamples
    finalNodeType
    definitionModule
    specModule
    trustedModule =
        evalSimplifierProofs simplifierx definitionModule $ do
            initialized <-
                initializeProver
                    definitionModule
                    specModule
                    trustedModule
            let InitializedProver{axioms, claims, alreadyProven} = initialized
            proveClaims
                maybeMinDepth
                stuckCheck
                breadthLimit
                searchOrder
                maxCounterexamples
                finalNodeType
                (AllClaims claims)
                (Axioms axioms)
                (AlreadyProven (map unparseToText2 alreadyProven))
                ( ToProve
                    ( map
                        (\x -> (x, depthLimit))
                        (extractUntrustedClaims' claims)
                    )
                )
      where
        extractUntrustedClaims' :: [SomeClaim] -> [SomeClaim]
        extractUntrustedClaims' = filter (not . isTrusted)

{- | Initialize and run the repl with the main and spec modules. This will loop
 the repl until the user exits.
-}
proveWithRepl ::
    Maybe MinDepth ->
    StuckCheck ->
    SimplifierXSwitch ->
    -- | The main module
    VerifiedModule StepperAttributes ->
    -- | The spec module
    VerifiedModule StepperAttributes ->
    -- | The module containing the claims that were proven in a previous run.
    Maybe (VerifiedModule StepperAttributes) ->
    MVar (Log.LogAction IO Log.ActualEntry) ->
    -- | Optional script
    Repl.Data.ReplScript ->
    -- | Run in a specific repl mode
    Repl.Data.ReplMode ->
    -- | Optional flag for output in run-mode
    Repl.Data.ScriptModeOutput ->
    -- | Optional Output file
    Repl.Data.OutputFile ->
    ModuleName ->
    KoreLogOptions ->
    KFileLocations ->
    SMT ()
proveWithRepl
    minDepth
    stuckCheck
    simplifierx
    definitionModule
    specModule
    trustedModule
    mvar
    replScript
    replMode
    scriptModeOutput
    outputFile
    mainModuleName
    logOptions
    kFileLocations =
        evalSimplifierProofs simplifierx definitionModule $ do
            initialized <-
                initializeProver
                    definitionModule
                    specModule
                    trustedModule
            let InitializedProver{axioms, claims} = initialized
            Repl.runRepl
                minDepth
                stuckCheck
                axioms
                claims
                mvar
                replScript
                replMode
                scriptModeOutput
                outputFile
                mainModuleName
                logOptions
                kFileLocations

-- | Bounded model check a spec given as a module containing rules to be checked
boundedModelCheck ::
    ( MonadLog smt
    , MonadSMT smt
    , MonadIO smt
    , MonadMask smt
    , MonadProf smt
    ) =>
    SimplifierXSwitch ->
    Limit Natural ->
    Limit Natural ->
    -- | The main module
    VerifiedModule StepperAttributes ->
    -- | The spec module
    VerifiedModule StepperAttributes ->
    Strategy.GraphSearchOrder ->
    smt
        ( Bounded.CheckResult
            (TermLike VariableName)
            (ImplicationRule VariableName)
        )
boundedModelCheck
    simplifierx
    breadthLimit
    depthLimit
    definitionModule
    specModule
    searchOrder =
        evalSimplifierProofs simplifierx definitionModule $ do
            initialized <- initializeAndSimplify definitionModule
            let Initialized{rewriteRules} = initialized
                specClaims = extractImplicationClaims specModule
            assertSomeClaims specClaims
            assertSingleClaim specClaims
            let axioms = fmap Bounded.Axiom rewriteRules
                claims =
                    mapRuleVariables (pure mkRuleVariable) . makeImplicationRule
                        <$> specClaims

            Bounded.checkClaim
                breadthLimit
                (Bounded.bmcStrategy axioms)
                searchOrder
                (head claims, depthLimit)

matchDisjunction ::
    ( MonadLog smt
    , MonadSMT smt
    , MonadIO smt
    , MonadMask smt
    , MonadProf smt
    ) =>
    SimplifierXSwitch ->
    VerifiedModule Attribute.Symbol ->
    Pattern RewritingVariableName ->
    [Pattern RewritingVariableName] ->
    smt (TermLike VariableName)
matchDisjunction simplifierx mainModule matchPattern disjunctionPattern =
    evalSimplifierProofs simplifierx mainModule $ do
        results <-
            traverse (runMaybeT . match matchPattern) disjunctionPattern
                <&> catMaybes
                <&> concatMap toList
        results
            <&> Condition.toPredicate
            & Predicate.makeMultipleOrPredicate
            & Predicate.fromPredicate sort
            & getRewritingTerm
            & return
  where
    sort = Pattern.patternSort matchPattern
    match = Search.matchWith SideCondition.top

{- | Ensure that for every equation in a function definition, the right-hand
side of the equation is a function pattern. Additionally, check if any function
definition in the module carries two equations that both match the same term.
'checkFunctions' first extracts equations from a verified module to a list of
lists of equations, ignoring any equations that are simplification rules. Then
check that the 'right' side of each equation is a function pattern. Any
equations whose 'right' side is __not__ a function pattern are thrown as an
'ErrorEquationRightFunction'. Finally, check if two equations both match the
same term. Start by constructing all in-order pairs of equations in each
sub-list. Then check that for each pair they both do __not__ match the same
term. Any pairs that do match the same term throw 'ErrorEquationsSameMatch'.
See 'checkEquation',
'Kore.Equation.Registry.extractEquations',
'Kore.Internal.TermLike.isFunctionPattern',
'Kore.Log.ErrorEquationRightFunction.errorEquationRightFunction',
'Kore.Log.ErrorEquationsSameMatch.errorEquationsSameMatch'.
-}
checkFunctions ::
    ( MonadLog smt
    , MonadSMT smt
    , MonadIO smt
    , MonadMask smt
    , MonadProf smt
    ) =>
    SimplifierXSwitch ->
    -- | The main module
    VerifiedModule StepperAttributes ->
    smt ()
checkFunctions simplifierx verifiedModule =
    evalSimplifierProofs simplifierx verifiedModule $ do
        -- check if RHS is function pattern
        equations >>= filter (not . isFunctionPattern . right)
            & mapM_ errorEquationRightFunction
        -- check if two equations both match the same term
        equations >>= inOrderPairs
            & filterM (uncurry bothMatch)
            >>= mapM_ (uncurry errorEquationsSameMatch)
  where
    equations :: [[Equation VariableName]]
    equations =
        extractEquations verifiedModule
            & Map.elems
            & map (filter (not . Kore.Equation.isSimplificationRule))
    -- https://stackoverflow.com/q/34044366/4051020
    inOrderPairs xs = [(x, y) | (x : ys) <- tails xs, y <- ys]

{- | Returns true when both equations match the same term.  See:
https://github.com/runtimeverification/haskell-backend/issues/2472#issue-833143685
-}
bothMatch ::
    MonadSimplify m =>
    Equation VariableName ->
    Equation VariableName ->
    m Bool
bothMatch eq1 eq2 =
    let pre1 = Equation.requires eq1
        pre2 = Equation.requires eq2
        arg1 = fromMaybe Predicate.makeTruePredicate $ Equation.argument eq1
        arg2 = fromMaybe Predicate.makeTruePredicate $ Equation.argument eq2
        prio1 = fromMaybe Predicate.makeTruePredicate $ Equation.antiLeft eq1
        prio2 = fromMaybe Predicate.makeTruePredicate $ Equation.antiLeft eq2
        check =
            Predicate.makeAndPredicate prio1 prio2
                & Predicate.makeAndPredicate arg2
                & Predicate.makeAndPredicate arg1
                & Predicate.makeAndPredicate pre2
                & Predicate.makeAndPredicate pre1
                & Predicate.mapVariables (pure mkConfigVariable)
        sort = termLikeSort $ right eq1
        patt = Pattern.fromPredicateSorted sort check
     in (not . isBottom) <$> Pattern.simplify patt

simplify ::
    (Monad m) =>
    Pattern RewritingVariableName ->
    m (Pattern RewritingVariableName)
simplify = return

assertSingleClaim :: Monad m => [claim] -> m ()
assertSingleClaim claims =
    when (length claims > 1) . error $
        "More than one claim is found in the module."

assertSomeClaims :: Monad m => [claim] -> m ()
assertSomeClaims claims =
    when (null claims) . error $
        "Unexpected empty set of claims.\n"
            ++ "Possible explanation: the frontend and the backend don't agree "
            ++ "on the representation of claims."

makeImplicationRule ::
    (Attribute.Axiom Symbol VariableName, ImplicationRule VariableName) ->
    ImplicationRule VariableName
makeImplicationRule (attributes, ImplicationRule rulePattern) =
    ImplicationRule rulePattern{attributes}

simplifySomeClaim ::
    MonadSimplify simplifier =>
    SomeClaim ->
    simplifier SomeClaim
simplifySomeClaim rule = do
    let claim = Lens.view lensClaimPattern rule
    claim' <- Rule.simplifyClaimPattern claim
    return $ Lens.set lensClaimPattern claim' rule

initializeAndSimplify ::
    MonadSimplify simplifier =>
    VerifiedModule StepperAttributes ->
    simplifier Initialized
initializeAndSimplify verifiedModule =
    initialize (simplifyRuleLhs >=> Logic.scatter) verifiedModule

-- | Collect various rules and simplifiers in preparation to execute.
initialize ::
    forall simplifier.
    MonadSimplify simplifier =>
    (RewriteRule RewritingVariableName -> LogicT simplifier (RewriteRule RewritingVariableName)) ->
    VerifiedModule StepperAttributes ->
    simplifier Initialized
initialize simplificationProcedure verifiedModule = do
    rewriteRules <-
        Logic.observeAllT $ do
            rule <- Logic.scatter (extractRewriteAxioms verifiedModule)
            initializeRule (mapRuleVariables (pure mkRuleVariable) rule)
    pure Initialized{rewriteRules}
  where
    initializeRule ::
        RewriteRule RewritingVariableName ->
        LogicT simplifier (RewriteRule RewritingVariableName)
    initializeRule rule = do
        simplRule <- simplificationProcedure rule
        when
            (lhsEqualsRhs $ getRewriteRule simplRule)
            (errorRewriteLoop simplRule)
        deepseq simplRule pure simplRule

data InitializedProver = InitializedProver
    { axioms :: ![Rule SomeClaim]
    , claims :: ![SomeClaim]
    , alreadyProven :: ![SomeClaim]
    }

data MaybeChanged a = Changed !a | Unchanged !a

fromMaybeChanged :: MaybeChanged a -> a
fromMaybeChanged (Changed a) = a
fromMaybeChanged (Unchanged a) = a

-- | Collect various rules and simplifiers in preparation to execute.
initializeProver ::
    forall simplifier.
    MonadSimplify simplifier =>
    VerifiedModule StepperAttributes ->
    VerifiedModule StepperAttributes ->
    Maybe (VerifiedModule StepperAttributes) ->
    simplifier InitializedProver
initializeProver definitionModule specModule maybeTrustedModule = do
    initialized <- initializeAndSimplify definitionModule
    tools <- Simplifier.askMetadataTools
    let Initialized{rewriteRules} = initialized
        changedSpecClaims :: [MaybeChanged SomeClaim]
        changedSpecClaims = expandClaim tools <$> extractClaims specModule
        simplifyToList :: SomeClaim -> simplifier [SomeClaim]
        simplifyToList rule = do
            simplified <- simplifyRuleLhs rule
            let result = toList simplified
            when (null result) $ warnTrivialClaimRemoved rule
            return result

        trustedClaims :: [SomeClaim]
        trustedClaims = maybe [] extractClaims maybeTrustedModule

    mapM_ logChangedClaim changedSpecClaims

    let specClaims :: [SomeClaim]
        specClaims = map fromMaybeChanged changedSpecClaims
    -- This assertion should come before simplifying the claims,
    -- since simplification should remove all trivial claims.
    assertSomeClaims specClaims
    simplifiedSpecClaims <- mapM simplifyToList specClaims
    claims <- traverse simplifySomeClaim (concat simplifiedSpecClaims)
    let axioms = coerce <$> rewriteRules
        alreadyProven = trustedClaims
    pure InitializedProver{axioms, claims, alreadyProven}
  where
    expandClaim ::
        SmtMetadataTools attributes ->
        SomeClaim ->
        MaybeChanged SomeClaim
    expandClaim tools claim =
        if claim /= expanded
            then Changed expanded
            else Unchanged claim
      where
        expanded = expandSingleConstructors tools claim

    logChangedClaim ::
        MaybeChanged SomeClaim ->
        simplifier ()
    logChangedClaim (Changed claim) =
        Log.logInfo ("Claim variables were expanded:\n" <> unparseToText claim)
    logChangedClaim (Unchanged _) = return ()
