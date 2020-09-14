{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Eucalypt.Core.ParseEmbed
Description : Parse quoted core directly into core without desugaring
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Core.ParseEmbed
  (translateEmbedded)
where

import Eucalypt.Core.Syn as Syn
import Eucalypt.Syntax.Ast as Ast
import Eucalypt.Core.SourceMap
import Eucalypt.Reporting.Location
import Debug.Trace

-- | Desugar quote-embedded core so we can reconstruct core that is
-- described directly in AST
translateEmbedded :: MonadSupplySMID m => Expression -> m CoreExpr
translateEmbedded Located{..} =
  translateExpr locatee
  where
    translateExpr (EList (Located{locatee=(ELiteral(VSym s))}:xs)) = dispatch s xs
    translateExpr (EOpSoup _ (x:_)) = translateEmbedded x
    translateExpr e = traceShow e $ error "bad parse-embed core content"
    dispatch "c-var" [x] = cvar x
    dispatch "c-let" [decls, body] = clet decls body
    dispatch "c-bif" [n] = cbif n
    dispatch "c-lit" [v] = clit v
    dispatch "c-lookup" [t, n, fb] = clookup t n $ Just fb
    dispatch "c-lookup" [t, n] = clookup t n Nothing
    dispatch "c-name" [n] = cname n
    dispatch "c-list" xs = clist xs
    dispatch "c-block" [b] = cblock b
    dispatch "c-meta" [e, m] = cmeta e m
    dispatch "c-args" xs = cargs xs
    dispatch "c-lam" [xs, body] = clam xs body
    dispatch "c-app" [f, xs] = capp f xs
    dispatch "c-soup" xs = csoup xs
    dispatch "c-op" [f, p, e] = cop f p e
    dispatch "c-bk-ana" [i] = cbkana i
    dispatch "c-ex-ana" [a] = cexana a
    dispatch "e-unresolved" [n] = eunresolved n
    dispatch "e-redeclared" [n] = eredeclared n
    dispatch "e-eliminated" [] = eeliminated
    dispatch "e-pseudodot" [] = epseudodot
    dispatch "e-pseudocall" [] = epseudocall
    dispatch "e-pseudocat" [] = epseudocat
    dispatch _ _ = undefined

-- | Translate embedded [:c-var n]
cvar :: MonadSupplySMID m => Expression -> m CoreExpr
cvar Located{locatee=(ELiteral (VStr varname)), location=loc} =
  mint Syn.var loc varname
cvar _ = error "bad c-var"

-- | Translate embedded [:c-let {} body]
clet :: MonadSupplySMID m => Expression -> Expression -> m CoreExpr
clet Located{locatee = (EBlock declblock), location=loc} body = do
  bindings <- traverse binding $ declarations declblock
  value <- translateEmbedded body
  mint2 Syn.letexp loc bindings value
  where
    declarations Located{locatee=Block elts} =
      map (\ Located{locatee=(Declaration Annotated{content=c})} -> c) elts
    binding Located{locatee=PropertyDecl k v} = do
      expr <- translateEmbedded v
      return (atomicName k, expr)
clet _ _ = error "bad c-let"


-- | Translate embedded [:c-bif n]
cbif :: MonadSupplySMID m => Expression -> m CoreExpr
cbif Located{locatee=(ELiteral (VSym bifname)), location=loc} =
  mint Syn.bif loc bifname
cbif _ = error "bad c-bif"


-- | Translate embedded [:c-lit _]
clit :: MonadSupplySMID m => Expression -> m CoreExpr
clit Located{locatee=(ELiteral prim), location=loc} =
  mint CorePrim loc $
  case prim of
    VInt i -> CoreInt i
    VFloat f -> CoreFloat f
    VStr s -> CoreString s
    VSym s -> CoreSymbol s
clit _ = error "bad c-lit"


-- | Translate embedded [:c-lookup _ n _?]
clookup :: MonadSupplySMID m => Expression -> Expression -> Maybe Expression -> m CoreExpr
clookup target Located{ locatee = (ELiteral (VStr n)) } (Just fallback) = do
  tgt <- translateEmbedded target
  fb <- translateEmbedded fallback
  return $ anon Syn.dynlookup tgt n fb
clookup target Located{locatee=(ELiteral (VStr n))} Nothing = do
  tgt <- translateEmbedded target
  return $ anon Syn.corelookup tgt n
clookup _ _ _ = error "bad c-lookup"


-- | Translate embedded [:c-name n]
cname :: MonadSupplySMID m => Expression -> m CoreExpr
cname Located{locatee=(ELiteral (VStr namename)), location=loc} =
  mint Syn.corename loc namename
cname _ = error "bad c-name"


-- | Translate embedded [:c-list _ _ _ _ ..._]
clist :: MonadSupplySMID m => [Expression] -> m CoreExpr
clist exprs = anon corelist <$> traverse translateEmbedded exprs


-- | Translate embedded [:c-block {}]
cblock :: MonadSupplySMID m => Expression -> m CoreExpr
cblock Located{locatee=(EBlock Located{locatee=(Block elements)})} =
  anon Syn.block <$> traverse toElement elements
  where
    propDecl Annotated{content=Located{locatee=(PropertyDecl n expr)}} = (n, expr)
    toElement Located{locatee=((Declaration dform)), location=loc} = do
      let (n, expr) = propDecl dform
      val <- translateEmbedded expr
      mint corelist loc [anon Syn.sym $ atomicName n, val]
cblock _ = error "bad c-block"


-- | Translate embedded [:c-meta e m]
cmeta :: MonadSupplySMID m => Expression -> Expression -> m CoreExpr
cmeta expr meta = do
  e <- translateEmbedded expr
  m <- translateEmbedded meta
  return $ anon withMeta m e


-- | Translate embedded [:c-args _ _ _ _ ..._]
cargs :: MonadSupplySMID m => [Expression] -> m CoreExpr
cargs exprs = anon args <$> traverse translateEmbedded exprs


-- | Translate embedded [:c-lam [x, y, z] body]
clam :: MonadSupplySMID m => Expression -> Expression -> m CoreExpr
clam bound impl = do
  let boundVars = extractBoundVars bound
  body <- translateEmbedded impl
  return $ anon lam boundVars body
  where
    extractBoundVars Located{locatee=EList vars} = map varName vars
    varName Located{locatee=ELiteral (VStr n)} = n


-- | Translate embedded [:c-app f xs]
capp :: MonadSupplySMID m => Expression -> Expression -> m CoreExpr
capp f xs = do
  applicable <- translateEmbedded f
  arguments <- sequenceA $ extractArgs xs
  return $ anon app applicable arguments
  where
    extractArgs Located{locatee=EList vs} = map translateEmbedded vs


-- | Translate embedded [:c-soup _ _ _ _ ..._]
csoup :: MonadSupplySMID m => [Expression] -> m CoreExpr
csoup exprs = anon soup <$> traverse translateEmbedded exprs


-- | Translate embedded [:c-op f p expr]
cop :: MonadSupplySMID m => Expression -> Expression -> Expression -> m CoreExpr
cop f p e = do
  let fixity = extractFixity f
  let precedence = extractPrecedence p
  expr <- translateEmbedded e
  return $ anon CoreOperator fixity precedence expr
  where
    extractFixity Located{locatee=(ELiteral (VSym "unary-prefix"))} = UnaryPrefix
    extractFixity Located{locatee=(ELiteral (VSym "unary-postfix"))} = UnaryPostfix
    extractFixity Located{locatee=(ELiteral (VSym "infix-left"))} = InfixLeft
    extractFixity Located{locatee=(ELiteral (VSym "infix-right"))} = InfixRight
    extractPrecedence Located{locatee=(ELiteral (VInt prec))} = fromIntegral prec


cbkana :: Expression -> m CoreExpr
cbkana = undefined

cexana :: Expression -> m CoreExpr
cexana = undefined

eunresolved :: Expression -> m CoreExpr
eunresolved _ = undefined


eredeclared :: Expression -> m CoreExpr
eredeclared _ = undefined


eeliminated :: m CoreExpr
eeliminated = undefined


epseudocall :: m CoreExpr
epseudocall = undefined


epseudocat :: m CoreExpr
epseudocat = undefined


epseudodot :: m CoreExpr
epseudodot = undefined
