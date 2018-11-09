module Test.Kore.Attribute.Injective where

import Test.Tasty
import Test.Tasty.HUnit

import Kore.AST.Common
import Kore.AST.Kore
import Kore.Attribute.Injective

import Test.Kore.Attribute.Parser

parseInjective :: Attributes -> Parser Injective
parseInjective = parseAttributes

test_injective :: TestTree
test_injective =
    testCase "[injective{}()] :: Injective"
        $ expectSuccess Injective { isDeclaredInjective = True }
        $ parseInjective $ Attributes [ injectiveAttribute ]

test_Attributes :: TestTree
test_Attributes =
    testCase "[injective{}()] :: Attributes"
        $ expectSuccess attrs $ parseAttributes attrs
  where
    attrs = Attributes [ injectiveAttribute ]

test_duplicate :: TestTree
test_duplicate =
    testCase "[injective{}(), injective{}()]"
        $ expectFailure
        $ parseInjective $ Attributes [ injectiveAttribute, injectiveAttribute ]

test_arguments :: TestTree
test_arguments =
    testCase "[injective{}(\"illegal\")]"
        $ expectFailure
        $ parseInjective $ Attributes [ illegalAttribute ]
  where
    illegalAttribute =
        (KoreObjectPattern . ApplicationPattern)
            Application
                { applicationSymbolOrAlias = injectiveSymbol
                , applicationChildren =
                    [ (KoreMetaPattern . StringLiteralPattern)
                        (StringLiteral "illegal")
                    ]
                }

test_parameters :: TestTree
test_parameters =
    testCase "[injective{illegal}()]"
        $ expectFailure
        $ parseInjective $ Attributes [ illegalAttribute ]
  where
    illegalAttribute =
        (KoreObjectPattern . ApplicationPattern)
            Application
                { applicationSymbolOrAlias =
                    SymbolOrAlias
                        { symbolOrAliasConstructor = injectiveId
                        , symbolOrAliasParams =
                            [ SortVariableSort (SortVariable "illegal") ]
                        }
                , applicationChildren = []
                }
