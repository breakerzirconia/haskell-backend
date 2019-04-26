module Test.Kore.Attribute.Label where

import Test.Tasty
import Test.Tasty.HUnit

import Kore.AST.Pure
import Kore.Attribute.Label

import Test.Kore.Attribute.Parser

parseLabel :: Attributes -> Parser Label
parseLabel = parseAttributes

attribute :: AttributePattern
attribute = labelAttribute "text"

test_Label :: TestTree
test_Label =
    testCase "[label{}(\"text\")] :: Label"
    $ expectSuccess Label { unLabel = Just "text" }
    $ parseLabel $ Attributes [ attribute ]

test_Attributes :: TestTree
test_Attributes =
    testCase "[label{}(\"text\")] :: Attributes"
    $ expectSuccess attrs $ parseAttributes attrs
  where
    attrs = Attributes [ attribute ]

test_duplicate :: TestTree
test_duplicate =
    testCase "[label{}(\"text\"), label{}(\"text\")]"
    $ expectFailure
    $ parseLabel $ Attributes [ attribute, attribute ]

test_arguments :: TestTree
test_arguments =
    testCase "[label{}()]"
    $ expectFailure
    $ parseLabel $ Attributes [ illegalAttribute ]
  where
    illegalAttribute =
        (asAttributePattern . ApplicationPattern)
            Application
                { applicationSymbolOrAlias = labelSymbol
                , applicationChildren = []
                }

test_parameters :: TestTree
test_parameters =
    testCase "[label{illegal}(\"text\")]"
    $ expectFailure
    $ parseLabel $ Attributes [ illegalAttribute ]
  where
    illegalAttribute =
        (asAttributePattern . ApplicationPattern)
            Application
                { applicationSymbolOrAlias =
                    SymbolOrAlias
                        { symbolOrAliasConstructor = labelId
                        , symbolOrAliasParams =
                            [ SortVariableSort (SortVariable "illegal") ]
                        }
                , applicationChildren =
                    [ (asAttributePattern . StringLiteralPattern)
                        (StringLiteral "text")
                    ]
                }
