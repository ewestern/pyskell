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
  symbol "@"
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
  colon
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
      symbol "="
      t <- option Nothing (Just <$> pTest)
      return (fp, t)
    pDStarName = (count 2 $ symbol "*") >> (identifier)
    pNames = do
      symbol "*"
      n1 <- identifier
      n2 <- parseMaybe (comma >> pDStarName)
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
pSmallStatement = (SmallStatement_Expr <$> pExpressionStatement) <|> (SmallStatement_Print <$> pPrintStatement) <|> (SmallStatement_Delete <$> pDeleteStatement) <|> (SmallStatement_Pass <$> pPassStatement) <|> (SmallStatement_Flow <$> pFlowStatement) <|> (SmallStatement_Import <$> pImportStatement) <|> (SmallStatement_Global <$> pGlobalStatement) <|> (SmallStatement_Exec <$> pExecStatement) <|> (SmallStatement_Assert <$> pAssertStatement)

pExpressionStatement :: Parser ExpressionStatement
pExpressionStatement = do
  tl <- pTestList
  as <- pAssign <|> pAug
  return $ ExpressionStatement tl as
  where
    pYieldOrTest = (YieldOrTest_Yield <$> pYieldExpression) <|> (YieldOrTest_Test <$> pTestList)
    pAssign = do 
      symbol "="
      yt <- many pYieldOrTest
      return $ Assignment yt
    pAug = do
      a <- pAugAssign
      yt <- pYieldOrTest
      return $ Assignment_Aug a yt

pAugAssign :: Parser AugAssign
pAugAssign =  (symbol "+=" >> return PlusEquals) <|> (symbol "-=" >> return MinusEquals) <|> (symbol "*=" >> return TimesEquals) <|> (symbol "/=" >> return DivideEquals) <|> (symbol "%=" >> return ModEquals) <|> (symbol "&=" >> return AndEquals) <|> (symbol "|=" >> return OrEquals) <|> (symbol "^=" >> return HatEquals) <|> (symbol "^=" >> return LShiftEquals) <|> (symbol ">>=" >> return RShiftEquals) <|> (symbol "**=" >> return ExpEquals) <|> ( symbol "//=" >> return FDivideEquals)

pPrintStatement :: Parser PrintStatement
pPrintStatement = do
  reserved "print"
  tl <- (commaSep pTest) <|> (symbol ">>" >> commaSep1 pTest)
  return $ PrintStatement tl

pDeleteStatement :: Parser DeleteStatement
pDeleteStatement = do
  reserved "del"
  el <- pExpressionList
  return $ DeleteStatement el

pPassStatement :: Parser PassStatement
pPassStatement = reserved "pass" >> return PassStatement


pFlowStatement :: Parser FlowStatement
pFlowStatement = (FlowStatement_Break <$> pBreakStatement) <|> (FlowStatement_Cont <$> pContinueStatement) <|> (FlowStatement_Ret <$> pReturnStatement) <|> (FlowStatement_Raise <$> pRaiseStatement) <|> (FlowStatement_Yield <$> pYieldStatement)

pBreakStatement :: Parser BreakStatement
pBreakStatement = reserved "break" >> return BreakStatement

pContinueStatement :: Parser ContinueStatement
pContinueStatement = reserved "continue" >> return ContinueStatement

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
  return $ ImportFrom m n
  where
    mDot = (many1 dot) >> return Nothing
    pImp = (symbol "*" >> return Nothing) <|> (Just <$> (parens pImportAsNames)) <|> (Just <$> pImportAsNames)
    pDot = do
      many dot
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
pDottedName = identifier `sepBy` dot


pGlobalStatement :: Parser GlobalStatement
pGlobalStatement = do
  reserved "global"
  ns <- commaSep1 identifier
  return $ GlobalStatement ns

pExecStatement :: Parser ExecStatement
pExecStatement = do
  reserved "exec"
  ex <- pExpression
  mt <- option Nothing (Just <$> pInTest)
  return $ ExecStatement ex mt
  where 
    pInTest = do
      reserved "in"
      t <- commaSep1 pTest
      return t

pAssertStatement :: Parser AssertStatement
pAssertStatement = do
  reserved "assert"
  t <- commaSep1 pTest
  return $ AssertStatement t

pCompoundStatement :: Parser CompoundStatement
pCompoundStatement = (CompoundStatement_If <$> pIfStatement) <|> (CompoundStatement_While <$> pWhileStatement) <|> (CompoundStatement_For <$> pForStatement) <|> (CompoundStatement_Try <$> pTryStatement) <|> (CompoundStatement_With <$> pWithStatement) <|> (CompoundStatement_Func <$> pFuncDef) <|> (CompoundStatement_Class <$> pClassDef) <|> (CompoundStatement_Dec <$> pDecorated)

pIfStatement :: Parser IfStatement
pIfStatement = do
  reserved "if"
  t <- pTest
  s <- pSuite
  elifs <- many pElif
  el <- option Nothing (Just <$> pElse)
  return $ IfStatement t s elifs el
  where
    pElif = do
      reserved "elif"
      t <- pTest
      colon
      s <- pSuite
      return (t, s)

pElse :: Parser Suite
pElse = do
  reserved "else"
  colon
  s <- pSuite
  return s


pWhileStatement :: Parser WhileStatement
pWhileStatement = do
  reserved "while"
  t <- pTest
  s <- pSuite
  el <- option Nothing (Just <$> pElse)
  return $ WhileStatement t s el

pForStatement :: Parser ForStatement
pForStatement = do
  reserved "for"
  ex <- pExpressionList
  reserved "in"
  tl <- pTestList
  colon
  s <- pSuite
  el <- option Nothing (Just <$> pElse)
  return $ ForStatement ex tl s el

pTryStatement :: Parser TryStatement
pTryStatement = do
  reserved "try"
  s <- pSuite
  tp <- pTryExcept <|> (TryPredicate_Finally <$> pFinally)
  return $ TryStatement s tp
  where
    pExceptSuite = do
      pc <- pExceptClause
      colon
      s <- pSuite
      return (pc, s)
    pFinally = do
      reserved "finally"
      colon
      s <- pSuite
      return s
    pTryExcept = do
      es <- many1 pExceptSuite
      el <- option Nothing (Just <$> pElse)
      f <- option Nothing (Just <$> pFinally)
      return $ TryPredicate_Except es el f

pWithStatement :: Parser WithStatement
pWithStatement = do
  reserved "with"
  ws <- commaSep1 pWithItem
  colon
  s <- pSuite
  return $ WithStatement ws s

pWithItem :: Parser WithItem
pWithItem = do
  t <- pTest
  m <- option Nothing (Just <$> pAs)
  return $ WithItem t m
  where
    pAs = do
      reserved "as"
      ex <- pExpression
      return ex

parseMaybe :: Parser a -> Parser (Maybe a)
parseMaybe p = option Nothing (Just <$> p)

pExceptClause :: Parser ExceptClause
pExceptClause = do
  reserved "except"
  (t1, t2) <- pCompound
  return $ ExceptClause t1 t2
  where
    pCompound = do
      t1 <- parseMaybe pTest
      t2 <- parseMaybe pTrailer
      return $ (t1, t2)
    pTrailer = do
      c <- (reserved "as" >> return Except_As) <|> (comma >> return Except_Comma)
      t <- pTest
      return $ (c, t)

--TODO: Critical to come back to this to insure indentation works
pSuite :: Parser Suite
pSuite = (Suite_Simple <$> pSimpleStatement) <|> (Suite_Compound <$> pIndent)
  where pIndent = do
            pNewline
            --indent
            ss <- many pStatement
            --dedent
            return ss

pOldLambda :: Parser OldLambda
pOldLambda = do
  reserved "lambda"
  va <- parseMaybe pVarArgsList
  colon
  ot <- pOldTest
  return $ OldLambda va ot

pOldTest :: Parser OldTest
pOldTest = (OldTest_Or <$> pOrTest) <|> (OldTest_Lam <$> pOldLambda)

pTestListSafe :: Parser TestListSafe
pTestListSafe = many1 pOldTest

pTest :: Parser Test
pTest = pTestOr <|> (Test_Lam <$> pLambdaDef)
  where
    pIfElse = do
      reserved "if"
      ot <- pOrTest
      reserved "else"
      t <- pTest
      return (ot, t)
    pTestOr = do
      ot <- pOrTest
      m <- parseMaybe pIfElse
      return $ Test_Or ot m
pOrTest :: Parser OrTest
pOrTest = sepBy1 pAndTest (reserved "or")

pAndTest :: Parser AndTest
pAndTest = sepBy1 pNotTest (reserved "and")

pNotTest :: Parser NotTest
pNotTest =  (NotTest_Not <$> pNNot) <|> (NotTest_Comp <$> pComparison)
  where pNNot = do
        reserved "not"
        nt <- pNotTest
        return nt

pComparison :: Parser Comparison
pComparison = do
  ex <- pExpression
  c <- many pCE
  return $ Comparison ex c
  where pCE = do
        co <- pCompOperator
        ex <- pExpression
        return (co, ex)

pCompOperator :: Parser CompOperator
pCompOperator = (CompOperator <$> pComp)
  where pComp = (symbol "<" >> return LT') <|> (symbol ">" >> return GT') <|> (symbol "==" >> return EQ') <|> (symbol ">=" >> return GTE) <|> (symbol "<=" >> return LTE) <|> (symbol "<>" >> return OldNE) <|> (symbol "!=" >> return NE) <|> (reserved "in" >> return In) <|> (reserved "not" >> return Not) <|> (reserved "is" >> return Is)
pExpression :: Parser Expression
pExpression = sepBy1 pXORExpression (symbol "|")

pXORExpression :: Parser XORExpression
pXORExpression = sepBy1 pAndExpression (symbol "^")

pAndExpression :: Parser AndExpression
pAndExpression = sepBy1 pShiftExpression (symbol "&")

pShiftExpression :: Parser ShiftExpression
pShiftExpression = do
  a <- pArithExpression
  as <- many pShift
  return $ ShiftExpression a as
  where
    pShift = do
      s <- (symbol "<<" >> return LeftS) <|> (symbol ">>" >> return RightS)
      a <- pArithExpression
      return (s, a)

pArithExpression :: Parser ArithExpression
pArithExpression = do
  t <- pTerm
  ts <- many pArith
  return $ ArithExpression t ts
  where
    pArith = do
      s <- (symbol "+" >> return Plus) <|> (symbol "-" >> return Minus)
      t <- pTerm
      return (s, t)

pTerm :: Parser Term
pTerm = do
  f <- pFactor
  t <- many pTermE
  return $ Term f t
    where
        pTermE = do
          ts <- ((symbol "*") >> return Star) <|> ((symbol "/") >> return Div) <|> ((symbol "%") >> return Mod) <|> ((symbol "//") >> return DDiv)
          f <- pFactor
          return (ts, f)

pFactor :: Parser Factor
pFactor = pFact <|> (Factor_Power <$> pPower)
  where 
    pFact = do
      s <- (symbol "+" >> return Factor_Plus) <|> (symbol "-" >> return Factor_Minus) <|> (symbol "~" >> return Factor_Tilde)
      f <- pFactor
      return $ Factor_Op s f

pPower :: Parser Power
pPower = do
  a <- pAtom
  t <- many pTrailer
  f <- parseMaybe pFact
  return $ Power a t f
  where
    pFact = do
      symbol "**"
      f <- pFactor
      return f

pString :: Parser String'
pString = quotes $ many anyChar

pAtom :: Parser Atom
pAtom = pYieldOrTest <|> (Atom_List <$> (squares pListMaker)) <|> (Atom_Dict <$> (braces pDictOrSetMaker)) <|> (Atom_TestList <$> (ticks pTestList1)) <|> (Atom_Name <$> identifier) <|> (Atom_Number <$> pNumber) <|>  (Atom_String <$> pString)
  where 
    pYieldOrTest = parens ((Atom_Yield <$> pYieldExpression) <|> (Atom_TestComp <$> pTestListComp))

pListMaker :: Parser ListMaker
pListMaker = (ListMaker_Test <$> (pTest `sepBy1` comma)) <|> pLF
  where pLF = do
        t <- pTest
        c <- pListFor
        return $ ListMaker_List t c

pTestListComp :: Parser TestListComp
pTestListComp = try (TestListComp_Test <$> (pTest `sepBy1` comma)) <|> pComp
    where
        pComp = do
          t <- pTest
          cf <- pCompFor
          return $ TestListComp_Comp t cf


pLambdaDef :: Parser LambdaDef
pLambdaDef = do
  reserved "lambda"
  va <- parseMaybe pVarArgsList
  colon
  t <- pTest
  return $ LambdaDef va t

pTrailer :: Parser Trailer
pTrailer =  (Trailer_Arglist <$> (parens (parseMaybe pArgList))) <|> (Trailer_SubscriptList <$> (squares pSubscriptList)) <|> (Trailer_Name <$> (dot >> identifier))

pSubscriptList :: Parser SubscriptList
pSubscriptList = pSubscript `sepBy1` comma

pSubscript :: Parser Subscript
pSubscript = (Subscript_Ellipsis <$> (count 3 (char '.'))) <|> pT
  where
      pC = do
        t <- parseMaybe pTest
        colon
        t2 <- parseMaybe pTest
        s <- parseMaybe pSliceOp
        return $ SubScript_Slice t t2 s
      pT = try pC <|> (Subscript_Test <$> pTest)

pSliceOp :: Parser SliceOp
pSliceOp = do
  colon
  t <- parseMaybe pTest
  return $ SliceOp t

pExpressionList :: Parser ExpressionList
pExpressionList = pExpression `sepBy1` comma

pTestList :: Parser TestList
pTestList = pTest `sepBy1` comma

pDictOrSetMaker :: Parser DictOrSetMaker
pDictOrSetMaker = (DictOrSetMaker_Set <$> pSetMaker) <|> (DictOrSetMaker_Dict <$> pDictMaker)
  where
    pCT = (CompOrTest_Comp <$> pCompFor) <|> (CompOrTest_Tests <$> (pTest `sepBy1` comma))
    pTs = do
      t1 <- pTest
      colon
      t2 <- pTest
      return (t1, t2) 
    pCTs = (CompOrTests_Comp <$> pCompFor) <|> (CompOrTests_Tests <$> (pTs `sepBy1` comma))
    pSetMaker = do
      t <- pTest
      ct <- pCT
      return $ SetMaker t ct
    pDictMaker = do
      t1 <- pTest
      t2 <- pTest
      ct <- pCTs
      return $ DictMaker t1 t2 ct

pClassDef :: Parser ClassDef
pClassDef = do
  reserved "class"
  n <- identifier
  tl <- parens (parseMaybe pTestList)
  colon
  s <- pSuite
  return $ ClassDef n tl s

--arglist: (argument ',')* (argument [',']
--                         |'*' test (',' argument)* [',' '**' test] 
--                         |'**' test)

pArgList :: Parser ArgList
pArgList = do
  as <- pArgument `sepBy` comma
  r <- (ArgList_Arg <$> pArgument) <|> pSing <|> (ArgList_Double <$> pDStarTest)
  return $ ArgList as r
  where
    pCA = do
      comma
      a <- pArgument
      return a
    pDStarTest = do
      symbol "**"
      t <- pTest
      return t
    pOptDStar = do
      comma
      t <- pDStarTest
      return t
    pSing = do
      symbol "*"
      t <- pTest
      as <- many pCA
      mt <- parseMaybe pOptDStar
      return $ ArgList_Single t as mt

pArgument :: Parser Argument
pArgument = pC <|> pT
    where
        pC = do
          t <- pTest
          c <- parseMaybe pCompFor
          return $ Argument_Comp t c
        pT = do
          t1 <- pTest
          symbol "="
          t2 <- pTest
          return $ Argument_Test t1 t2

pListIter :: Parser ListIter
pListIter = ListIter_For <$> pListFor

pListFor :: Parser ListFor
pListFor = do
  reserved "for"
  ex <- pExpressionList
  reserved "in"
  tl <- pTestListSafe
  li <- parseMaybe pListIter
  return $ ListFor ex tl li

pCompIter :: Parser CompIter
pCompIter = CompIter_For <$> pCompFor

pCompFor :: Parser CompFor
pCompFor = do
  reserved "for"
  el <- pExpressionList
  reserved "in"
  ot <- pOrTest
  ci <- parseMaybe pCompIter
  return $ CompFor el ot ci

pTestList1 :: Parser TestList1
pTestList1 = pTest `sepBy1` comma

pYieldExpression :: Parser YieldExpression
pYieldExpression = do
  reserved "yield"
  tl <- parseMaybe pTestList
  return tl


    --pCompoundStatement :: Parser CompoundStatement
--pCompoundStatement = do
--  c <- char '0'
--  return $ CompoundStatement_If c
