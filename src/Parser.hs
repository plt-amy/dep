module Parser (parseStmt, parseProg) where

import qualified Text.Parsec.Language as Lang
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String
import Text.Parsec hiding (parse)

import Data.Functor.Identity

import Term

parseStmt :: SourceName -> String -> Either String Stmt
parseStmt n x =
  case runParser stmt () n x of
    Left x -> Left (show x)
    Right x -> Right x

parseProg :: SourceName -> String -> Either String [Stmt]
parseProg n x =
  case runParser (Tok.semiSep1 lexer stmt) () n x of
    Left x -> Left (show x)
    Right x -> Right x

lexer :: Tok.GenTokenParser String u Identity
lexer = Tok.makeTokenParser (Lang.haskellDef { Tok.reservedNames = "assume":Tok.reservedNames Lang.haskellDef  })

stmt :: Parser Stmt
stmt = (do Tok.reserved lexer "let"
           v <- Tok.identifier lexer
           ty <- optionMaybe $ do
             ":" <- Tok.colon lexer
             term
           Tok.reservedOp lexer "="
           Define v ty <$> term
   <|> (do Tok.reserved lexer "assume"
           v <- Tok.identifier lexer
           ":" <- Tok.colon lexer
           Postulate v <$> term
        <?> "definition") <?> "definition")
   <|> (Infer <$> term <* eof
         <?> "term")

term :: Parser Term
term = Ex.buildExpressionParser
  [ [ Ex.Infix (Tok.reservedOp lexer "->" *> pure (Pi "_")) Ex.AssocRight ] ]
  texp

texp :: Parser Term
texp = do
  a <- atom
  (foldl (:$) a <$> many atom) <|> pure a

atom :: Parser Term
atom = Tok.parens lexer
         (do t <- term
             fmap (maybe t (t :::)) . optionMaybe $ do
               _ <- Tok.colon lexer
               term)
     <|> (do Tok.reservedOp lexer "\\"
             vs <- many . Tok.parens lexer $ do
               x <- Tok.identifier lexer
               ":" <- Tok.colon lexer
               (,) x <$> term
             "." <- Tok.dot lexer
             flip (foldr (\(x, y) k -> Lam x y k)) vs <$> term
         <?> "lambda abstraction")
     <|> (do Tok.reserved lexer "forall"
             vs <- many . Tok.parens lexer $ do
               x <- Tok.identifier lexer
               ":" <- Tok.colon lexer
               (,) x <$> term
             "." <- Tok.dot lexer
             flip (foldr (\(x, y) k -> Pi x y k)) vs <$> term
          <?> "forall-quantified type")
     <|> (do Tok.reserved lexer "type"
             Type . fromInteger <$> Tok.natural lexer
          <?> "type")
     <|> (Var <$> Tok.identifier lexer
          <?> "variable")
