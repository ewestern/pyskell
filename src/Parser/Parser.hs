module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative ((<$>))
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Parser.Lexer
import Parser.Syntax


--binary s f assoc = Ex.Infix (reservedOp s >> return (BinOp f)) assoc

--table = [[binary "*" Times Ex.AssocLeft, binary "/" Divide Ex.AssocLeft],
--          [binary "+" Plus Ex.AssocLeft, binary "-" Minus Ex.AssocLeft]]

--pNumber :: Parser Number
--pNumber = do
--     n <- Tok.naturalOrFloat
--     case n of
        --Right d -> return $ Number_Float d
        --Left i -> return $ Number_Int i

pNumber :: Parser Number
pNumber = do
  n <- Tok.naturalOrFloat lexer
  case n of
    Right d -> return $ Number_Float d
    Left i -> return $ Number_Int i

pNewline :: Parser Newline
pNewline = do
  n <- newline
  return $ Newline

pSingleInput :: Parser SingleInput
pSingleInput =  (SingleInput_NewLine <$> pNewline) <|> (SingleInput_Simple <$> pSimpleStatement) <|> (SingleInput_Compound <$> pCompAndNew)
  where
    pCompAndNew = do
      c <- pCompoundStatement
      n <- pNewline
      return (c, n)


pFileInput :: Parser FileInput
pFileInput = do
  f <- many pNew
  eof
  return $ FileInput f
    where
      pNew = (NewLineOrStatement_NL <$> pNewline) <|> (NewLineOrStatement_ST <$> pStatement)

pEvalInput :: Parser EvalInput
pEvalInput = do
  tl <- pTestList
  ns <- many pNewline
  eof
  return $ EvalInput tl ns

pDecorator :: Parser Decorator
pDecorator = do
  char '@'
  dn <- pDottedName
  ma <- pArg
  nl <- pNewline
  return $ Decorator dn ma nl
    where
      pArg = do
        al <- option Nothing $ (Just <$> parens pArgList)
        return al

pDecorators :: Parser Decorators
pDecorators = many1 pDecorator

pDecorated :: Parser Decorated
pDecorated = do
  ds <- pDecorators
  cfd <- (FuncOrClass_Class <$> pClassDef) <|> (FuncOrClass_Func <$> pFuncDef)
  return $ Decorated ds cfd

pFuncDef :: Parser FuncDef
pFuncDef = do
  reserved "def"
  n <- identifier
  ps <- pParameters
  char ':'
  s <- pSuite
  return $ FuncDef n ps s

pParameters :: Parser Parameters
pParameters = do
  va <- parens (option Nothing (Just <$> pVarArgsList))
  return $ Parameters va

pVarArgsList :: Parser VarArgsList
pVarArgsList = do
  fp <- commaSep pFP
  pa <- pProtoArgs
  return $ VarArgsList fp pa
  where 
    pFP = do
      fp <- pFPDef
      char '='
      t <- option Nothing (Just <$> pTest)
      return (fp, t)
    pDStarName = count 2 $ char '*' >> (identifier)
    pNames = do
      char '*'
      n1 <- identifier
      n2 <- option Nothing (Just <$> (Tok.comma >> pDStarName))
      return $ ProtoVarArgsList_Names n1 n2
    pFPTest = commaSep1 pFP
    pProtoArgs = pNames <|> (ProtoVarArgsList_Name <$> pDStarName) <|> (ProtoVarArgsList_FP <$> pFPTest)


pFPDef :: Parser FPDef
pFPDef = (FPDef_Name <$> identifier) <|> (FPDef_FP <$> (parens pFPList))

pFPList :: Parser FPList
pFPList = commaSep1 pFPDef

pStatement :: Parser Statement
pStatement = (Statement_Simple <$> pSimpleStatement) <|> (Statement_Compound <$> pCompoundStatement)

pSimpleStatement :: Parser SimpleStatement
pSimpleStatement = do
  s <- semiSep1 pSmallStatement
  n <- pNewline
  return $ SimpleStatement s n

pSmallStatement :: Parser SmallStatement
pSmallStatement = (SmallStatement_Expr <$> pExpressionStatement) <|> (SmallStatement_Print <$> pSmallStatement) <|> (SmallStatement_Delete <$> pDeleteStatement) <|> (SmallStatement_Pass <$> pPassStatement) <|> (SmallStatement_Flow <$> pFlowStatement) <|> (SmallStatement_Import <$> pImportStatement) <|> (SmallStatement_Global <$> pGlobalStatement) <|> (SmallStatement_Exec <$> pExecStatement) <|> (SmallStatement_Assert <$> pAssertStatement)

pExpressionStatement :: Parser ExpressionStatement
pExpressionStatement = do
  tl <- pTestList
  as <- pAssign <|> pAug
  return $ ExpressionStatement tl as
  where
    pYieldOrTest = (YieldOrTest_Yield <$> pYieldExpression) <|> (YieldOrTest_Test <$> pTestList)
    pAssign = do 
      char '=' 
      yt <- many pYieldOrTest
      return $ Assignment yt
    pAug = do
      a <- pAugAssign
      yt <- pYieldOrTest
      return $ Assignment_Aug a yt

pAugAssign :: Parser AugAssign
pAugAssign =  (PlusEquals <$> string "+=") <|> (MinusEquals <$> string "-=") <|> (TimesEquals <$> string "*=") <|> (DivideEquals <$> string "/=") <|> (ModEquals <$> string "%=") <|> (AndEquals <$> string "&=") <|> (OrEquals <$> string "|=") <|> (HatEquals <$> string "^=") <|> (LShiftEquals <$> string "^=") <|> (RShiftEquals <$> string ">>=") <|> (ExpEquals <$> string "**=") <|> (FDivideEquals <$> "//=")

pPrintStatement :: Parser PrintStatement
pPrintStatement = do
  reserved "print"
  tl <- (commaSep pTest) <|> (string ">>" >> commaSep1 pTest)
  return $ PrintStatement tl

pDeleteStatement :: Parser DeleteStatement
pDeleteStatement = do
  reserved "del"
  el <- pExpressionList
  return $ DeleteStatement el

pPassStatement :: Parser PassStatement
pPassStatement = reserved "pass" >> Pass


pFlowStatement :: Parser FlowStatement
pFlowStatement = (FlowStatement_Break <$> pBreakStatement) <|> (FlowStatement_Cont <$> pContinueStatement) <|> (FlowStatement_Ret <$> pReturnStatement) <|> (FlowStatement_Raise <$> pRaiseStatement) <|> (FlowStatement_Yield <$> pYieldStatement)

pBreakStatement :: Parser BreakStatement
pBreakStatement = reserved "break" >> BreakStatement

pContinueStatement :: Parser ContinueStatement
pContinueStatement = reserved "continue" >> ContinueStatement

pReturnStatement :: Parser ReturnStatement
pReturnStatement = do
  reserved "return"
  tl <- option Nothing (Just <$> pTestList)
  return $ ReturnStatement tl

pYieldStatement :: Parser YieldStatement
pYieldStatement = do
  ye <- pYieldExpression
  return $ YieldStatement ye

pRaiseStatement :: Parser RaiseStatement
pRaiseStatement = reserved "raise" >> (RaiseStatement <$>  pTestList)

pImportStatement :: Parser ImportStatement
pImportStatement = (ImportStatement_Name <$> pImportName) <|> (ImportStament_From <$> pImportFrom)

pImportName :: Parser ImportName
pImportName = reserved "import" >> (ImportName <$> pDottedAsNames)

pImportFrom :: Parser ImportFrom
pImportFrom = do
  reserved "from"
  m <- try (Just <$> pDot) <|> mDot
  reserved "import"
  n <- pImp  
  return ImportFrom
  where
    mDot = (many1 Tok.dot) >> Nothing
    pImp = (char '*' >> Nothing) <|> (Just <$> (parens pImportAsNames)) <|> (Just <$> pImportAsNames)
    pDot = do
      many Tok.dot
      dn <- pDottedName
      return $ dn

pAsName :: Parser Name
pAsName = do
  reserved "as"
  i <- identifier
  return i

pImportAsName :: Parser ImportAsName
pImportAsName = do
  i1 <- identifier
  i2 <- option Nothing (Just <$> pAsName)
  return $ ImportAsName i1 i2

pDottedAsName :: Parser DottedAsName
pDottedAsName = do
  dn <- pDottedName
  m <- option Nothing (Just <$> pAsName)
  return $ DottedAsName dn m

pImportAsNames :: Parser ImportAsNames
pImportAsNames = commaSep1 pImportAsName

pDottedAsNames :: Parser DottedAsNames
pDottedAsNames = commaSep1 pDottedAsName

pDottedName :: Parser DottedName
pDottedName = identified `sepBy` Tok.dot


pGlobalStatement :: Parser GlobalStatement
pGlobalStatement = do
  reserved "global"
  ns <- commaSep1 identifier
  return GlobalStatement ns


--pCompoundStatement :: Parser CompoundStatement
--pCompoundStatement = do
--  c <- char '0'
--  return $ CompoundStatement_If c
