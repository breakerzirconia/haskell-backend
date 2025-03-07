{- |
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause
-}
module Kore.Simplify.Mu (
    simplify,
    makeEvaluate,
) where

import Kore.Internal.OrPattern (
    OrPattern,
 )
import Kore.Internal.OrPattern qualified as OrPattern
import Kore.Internal.Pattern (
    Pattern,
 )
import Kore.Internal.Pattern qualified as Pattern (
    fromTermLike,
    simplifiedAttribute,
    toTermLike,
 )
import Kore.Internal.TermLike (
    Mu (Mu),
    SetVariable,
    mkMu,
 )
import Kore.Internal.TermLike qualified as TermLike (
    setSimplified,
 )
import Kore.Internal.TermLike qualified as TermLike.DoNotUse
import Kore.Rewrite.RewritingVariable (
    RewritingVariableName,
 )
import Prelude.Kore

{- |'simplify' simplifies a 'Mu' pattern with an 'OrPattern'
child.
-}
simplify ::
    Mu RewritingVariableName (OrPattern RewritingVariableName) ->
    OrPattern RewritingVariableName
simplify Mu{muVariable, muChild} =
    OrPattern.map (makeEvaluate muVariable) muChild

{- | evaluates a 'Mu' given its two 'Pattern' children.

See 'simplify' for detailed documentation.
-}
makeEvaluate ::
    SetVariable RewritingVariableName ->
    Pattern RewritingVariableName ->
    Pattern RewritingVariableName
makeEvaluate variable patt =
    Pattern.fromTermLike $
        TermLike.setSimplified (Pattern.simplifiedAttribute patt) $
            mkMu variable $
                Pattern.toTermLike patt
