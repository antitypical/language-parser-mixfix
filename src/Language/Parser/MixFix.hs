module Language.Parser.MixFix where

data Associativity = LeftA | RightA | NonA
  deriving (Eq, Show)

data Fixity = Prefix | Postfix | Infix Associativity | Closed
  deriving (Eq, Show)
