{-#LANGUAGE GADTs #-}

module Parser.Syntax where

data Newline = Newline
data Endmarker = Endmarker
type Name = String
data Number = 
  Number_Int Integer
  | Number_Float Double

type String' = String
--type New
--single_input: NEWLINE | simple_stmt | compound_stmt NEWLINE

data SingleInput = 
  SingleInput_NewLine Newline 
  | SingleInput_Simple SimpleStatement 
  | SingleInput_Compound (CompoundStatement, Newline)

--file_input: (NEWLINE | stmt)* ENDMARKER
data NewLineOrStatement = 
  NewLineOrStatement_NL Newline
  | NewLineOrStatement_ST Statement

data FileInput = FileInput [NewLineOrStatement]

--eval_input: testlist NEWLINE* ENDMARKER
data EvalInput = EvalInput TestList [Newline]
--decorator: '@' dotted_name [ '(' [arglist] ')' ] NEWLINE
data Decorator = Decorator DottedName (Maybe ArgList) Newline

--decorators: decorator+
type Decorators = [Decorator]

--decorated: decorators (classdef | funcdef)
data FuncOrClass = 
  FuncOrClass_Class ClassDef
  | FuncOrClass_Func FuncDef

data Decorated = Decorated Decorators FuncOrClass

--funcdef: 'def' NAME parameters ':' suite

data FuncDef = FuncDef Name Parameters Suite

--parameters: '(' [varargslist] ')'
data Parameters = Parameters (Maybe VarArgsList)

--varargslist: ((fpdef ['=' test] ',')*
--              ('*' NAME [',' '**' NAME] | '**' NAME) |
--              fpdef ['=' test] (',' fpdef ['=' test])*   [','])
data ProtoVarArgsList = 
  ProtoVarArgsList_Name Name
  | ProtoVarArgsList_Names Name Name
  | ProtoVarArgsList_FP FPDef (Maybe Test) [(FPDef, (Maybe Test))]


data VarArgsList = VarArgsList [(FPDef, (Maybe Test))] ProtoVarArgsList


--fpdef: NAME | '(' fplist ')'

data FPDef = 
  FPDef_Name Name
  | FPDef_FP FPList

--fplist: fpdef (',' fpdef)* [',']
type FPList = [FPDef]

--stmt: simple_stmt | compound_stmt

data Statement = 
  Statement_Simple SimpleStatement 
  | Statement_Compound CompoundStatement

--simple_stmt: small_stmt (';' small_stmt)* [';'] NEWLINE
data SimpleStatement = SimpleStatement [SmallStatement] Newline

--small_stmt: (expr_stmt | print_stmt  | del_stmt | pass_stmt | flow_stmt |
--             import_stmt | global_stmt | exec_stmt | assert_stmt)

data SmallStatement = 
  SmallStatement_Expr ExpressionStatement
  | SmallStatement_Print PrintStatement
  | SmallStatement_Delete DeleteStatement
  | SmallStatement_Pass PassStatement
  | SmallStatement_Flow FlowStatement
  | SmallStatement_Import ImportStatement
  | SmallStatement_Global GlobalStatement
  | SmallStatement_Exec ExecStatement
  | SmallStatement_Assert AssertStatement




data YieldOrTest = 
  YieldOrTest_Yield YieldExpression
  | YieldOrTest_Test TestList


data Assignment = 
  Assignment_Aug AugAssign YieldOrTest
  | Assignment [YieldOrTest]


--expr_stmt: testlist (augassign (yield_expr|testlist) |
--                     ('=' (yield_expr|testlist))*)

data ExpressionStatement = ExpressionStatement TestList Assignment

---augassign: ('+=' | '-=' | '*=' | '/=' | '%=' | '&=' | '|=' | '^=' |
--            '<<=' | '>>=' | '**=' | '//=')
data Assign = Equals

data AugAssign = 
   PlusEquals
  | MinusEquals
  | TimesEquals
  | DivideEquals
  | ModEquals
  | AndEquals
  | OrEquals
  | HatEquals
  | LShiftEquals
  | RShiftEquals
  | ExpEquals
  | FDivideEquals
--   # For normal assignments, additional restrictions enforced by the interpreter
--print_stmt: 'print' ( [ test (',' test)* [','] ] |
--                      '>>' test [ (',' test)+ [','] ] )


data PrintStatement = PrintStatement TestList


--del_stmt: 'del' exprlist

data DeleteStatement = DeleteStatement ExpressionList

--pass_stmt: 'pass'
data PassStatement = PassStatement

--flow_stmt: break_stmt | continue_stmt | return_stmt | raise_stmt | yield_stmt

data FlowStatement = 
  FlowStatement_Break BreakStatement
  | FlowStatement_Cont ContinueStatement
  | FlowStatement_Ret ReturnStatement
  | FlowStatement_Raise RaiseStatement
  | FlowStatement_Yield YieldStatement

--break_stmt: 'break'
data BreakStatement = BreakStatement 
--continue_stmt: 'continue'
data ContinueStatement = ContinueStatement
--return_stmt: 'return' [testlist]
data ReturnStatement = ReturnStatement (Maybe TestList)
--yield_stmt: yield_expr
data YieldStatement = YieldStatement YieldExpression

--raise_stmt: 'raise' [test [',' test [',' test]]]
data RaiseStatement = RaiseStatement TestList

--import_stmt: import_name | import_from
data ImportStatement = 
  ImportStatement_Name ImportName
  | ImportStament_From ImportFrom

--import_name: 'import' dotted_as_names
data ImportName = ImportName DottedAsNames

--import_from: ('from' ('.'* dotted_name | '.'+)
--              'import' ('*' | '(' import_as_names ')' | import_as_names))

--TODO: May need to reconsider dots
--data FromImport = FromImport (Maybe ImportAsNames)(Maybe DottedName)

--data Import = 
--  Import_Star
--  | Import_Paren ImportAsNames
--  | Import_Names ImportAsNames

data ImportFrom = ImportFrom (Maybe DottedName) (Maybe ImportAsNames)


--import_as_name: NAME ['as' NAME]

data ImportAsName = ImportAsName Name (Maybe Name)

--dotted_as_name: dotted_name ['as' NAME]
data DottedAsName = DottedAsName DottedName (Maybe Name)

--import_as_names: import_as_name (',' import_as_name)* [',']

type ImportAsNames = [ImportAsName]

--dotted_as_names: dotted_as_name (',' dotted_as_name)*
data DottedAsNames = DottedAsNames [DottedAsNames]

--dotted_name: NAME ('.' NAME)*

type DottedName = [Name]

--global_stmt: 'global' NAME (',' NAME)*
data GlobalStatement = GlobalStatement [Name]

--exec_stmt: 'exec' expr ['in' test [',' test]]
data ExecStatement = ExecStatement Expression (Maybe [Test])

--assert_stmt: 'assert' test [',' test]
data AssertStatement = AssertStatement [Test]

--compound_stmt: if_stmt | while_stmt | for_stmt | try_stmt | with_stmt | funcdef | classdef | decorated
data CompoundStatement = 
  CompoundStatement_If IfStatement
  | CompoundStatement_While WhileStatement
  | CompoundStatement_For ForStatement
  | CompoundStatement_Try TryStatement
  | CompoundStatement_With WithStatement
  | CompoundStatement_Func FuncDef
  | CompoundStatement_Class ClassDef
  | CompoundStatement_Dec Decorated

--if_stmt: 'if' test ':' suite ('elif' test ':' suite)* ['else' ':' suite]
data IfStatement = IfStatement Test Suite [(Test, Suite)] (Maybe Suite)

--while_stmt: 'while' test ':' suite ['else' ':' suite]

data WhileStatement = WhileStatement Test Suite (Maybe Suite)

--for_stmt: 'for' exprlist 'in' testlist ':' suite ['else' ':' suite]

data ForStatement = ForStatement ExpressionList TestList Suite (Maybe Suite)

--try_stmt: ('try' ':' suite
--           (
        --    (except_clause ':' suite)+
--            ['else' ':' suite]
--            ['finally' ':' suite] |
--           'finally' ':' suite
    --       )
--         )

data TryPredicate = 
  TryPredicate_Except ExceptClause (Maybe Suite) (Maybe Suite)
  | TryPredicate_Finally Suite

data TryStatement = TryStatement Suite TryPredicate

--with_stmt: 'with' with_item (',' with_item)*  ':' suite

data WithStatement = WithStatement [WithItem] Suite

--with_item: test ['as' expr]

data WithItem = WithItem Test (Maybe Expression)

--   # NB compile.c makes sure that the default except clause is last
--except_clause: 'except' [test [('as' | ',') test] ]

data CompoundExcept = Except_As Test | Except_Comma Test

data ExceptClause = ExceptClause (Maybe Test) (Maybe CompoundExcept)
--suite: simple_stmt | NEWLINE INDENT stmt+ DEDENT

data Suite = 
  Suite_Simple SimpleStatement
  | Suite_Compound [Statement]

--    # Backward compatibility cruft to support:
--    # [ x for x in lambda: True, lambda: False if x() ]
--    # even while also allowing:
--    # lambda x: 5 if x else 2
--    # (But not a mix of the two)
--    testlist_safe: old_test [(',' old_test)+ [',']]
--old_test: or_test | old_lambdef

--old_lambdef: 'lambda' [varargslist] ':' old_test
data OldLambda = OldLambda (Maybe VarArgsList) OldTest

--test: or_test ['if' or_test 'else' test] | lambdef

data OldTest = 
  OldTest_Or OrTest
  | OldTest_Lam OldLambda

data TestListSafe = TestListSafe OldTest [Maybe OldTest]

data Test = 
  Test_Or (OrTest, (Maybe (OrTest, Test)))
  | Test_Lam LambdaDef


--or_test: and_test ('or' and_test)*

data OrTest = OrTest AndTest [AndTest]
--and_test: not_test ('and' not_test)*

data AndTest = AndTest NotTest [NotTest]
--not_test: 'not' not_test | comparison
data NotTest = 
  NotTest_Not NotTest
  | NotTest_Comp Comparison

--comparison: expr (comp_op expr)*

data Comparison = Comparison Expression [(CompOperator, Expression)]

data Comp = 
  LT
  | GT
  | EQ
  | GTE
  | LTE
  | OldNE
  | NE
  | In
  | Not
  | Is

--comp_op: '<'|'>'|'=='|'>='|'<='|'<>'|'!='|'in'|'not' 'in'|'is'|'is' 'not'
--data FirstComp = 
--  LT
--  | GT
--  | EQ
--  | GTE
--  | LTE
--  | OldNE
--  | NE
--  | In
--  | Not
--data SecondComp = 

data CompOperator = CompOperator Comp Comp Comp

--data Command where
--  Arithmetic' :: Arithmetic -> Command 
--  MemAccess' :: MemAccess -> Command
--  Flow' :: Flow -> Command
--  FCall' :: FCall -> Command
--  deriving (Show)




--expr: xor_expr ('|' xor_expr)*
data Expression = Expression XORExpression [XORExpression] 

--xor_expr: and_expr ('^' and_expr)*
data XORExpression = XORExpression AndExpression [AndExpression]
--and_expr: shift_expr ('&' shift_expr)*
data AndExpression = AndExpression ShiftExpression [ShiftExpression]

--shift_expr: arith_expr (('<<'|'>>') arith_expr)*
data Shift =
  LeftS
  | RightS

data ShiftExpression = ShiftExpression ArithExpression [(Shift, ArithExpression)]

--arith_expr: term (('+'|'-') term)*
data Arith =
  Plus
  | Minus

data ArithExpression = ArithExpression Term [(Arith, Term)]

--term: factor (('*'|'/'|'%'|'//') factor)*

data TermExp =
  Star
  | Div
  | Mod
  | DDiv

data Term = Term Factor [(TermExp, Factor)]

--factor: ('+'|'-'|'~') factor | power
data FactorExp =
   Factor_Plus
  | Factor_Minus
  | Factor_Tilde

data Factor = 
  Factor_Op FactorExp Factor
  | Factor_Power Power

--power: atom trailer* ['**' factor]

data Power = Power Atom [Trailer] (Maybe Factor)
--atom: ('(' [yield_expr|testlist_comp] ')' |
--       '[' [listmaker] ']' |
--       '{' [dictorsetmaker] '}' |
--       '`' testlist1 '`' |
--       NAME | NUMBER | STRING+)

data Atom = 
  Atom_Yield YieldExpression
  | Atom_TestComp TestListComp
  | Atom_List ListMaker
  | Atom_Dict DictOrSetMaker
  | Atom_TestList TestList1
  | Atom_Name Name
  | Atom_Number Number
  | Atom_String String'
--listmaker: test ( list_for | (',' test)* [','] )

data ListMaker = 
  ListMaker_List Test ListFor
  | ListMaker_Test Test [Test]

--testlist_comp: test ( comp_for | (',' test)* [','] )
data TestListComp = 
  TestListComp_Comp Test CompFor
  |TestListComp_Test Test [Test]

--lambdef: 'lambda' [varargslist] ':' test

data LambdaDef = LambdaDef (Maybe VarArgsList) Test
--trailer: '(' [arglist] ')' | '[' subscriptlist ']' | '.' NAME

data Trailer = 
  Trailer_Arglist (Maybe ArgList)
  | Trailer_SubscriptList SubscriptList
  | Trailer_Name Name
--subscriptlist: subscript (',' subscript)* [',']

data SubscriptList = SubscriptList [Subscript]

type Ellipsis = String
--subscript: '.' '.' '.' | test | [test] ':' [test] [sliceop]

data Subscript = 
  Subscript_Ellipsis Ellipsis
  | Subscript_Test Test
  | SubScript_Slice (Maybe Test) (Maybe Test) SliceOp
--sliceop: ':' [test]

data SliceOp = SliceOp (Maybe Test)

--exprlist: expr (',' expr)* [','] 
data ExpressionList = ExpressionList [Expression]

--testlist: test (',' test)* [',']
data TestList = TestList [Test]

--dictorsetmaker: ( (test ':' test (comp_for | (',' test ':' test)* [','])) |
--                  (test (comp_for | (',' test)* [','])) )

--data CompOrTests = 
--  CompOrTests_For CompFor
--  CompOrTests_Test 
--  | [(Test, Test)]

data DictMaker = 
  DictMaker_For Test Test CompFor
  | DIctMaker_Test Test Test [(Test, Test)]

data SetMaker = 
  SetMaker_For Test CompFor
  | SetMaker_Test Test [Test]

data DictOrSetMaker = 
  DictOrSetMaker_Dict DictMaker 
  | DictOrSetMaker_Set SetMaker

--classdef: 'class' NAME ['(' [testlist] ')'] ':' suite

data ClassDef = ClassDef Name (Maybe TestList) Suite
--arglist: (argument ',')* (argument [',']
--                         |'*' test (',' argument)* [',' '**' test] 
--                         |'**' test)
--    # The reason that keywords are test nodes instead of NAME is that using NAME
--    # results in an ambiguity. ast.c makes sure it's a NAME.

data ArgList = 
  ArgList_Arg Argument
  | ArgList_ArgTest Test [Argument] (Maybe Test)
  | Test
--argument: test [comp_for] | test '=' test

data Argument = 
  Argument_Comp Test (Maybe CompFor)
  | Argument_Test Test Test

--list_iter: list_for | list_if
-- dont' use list if
data ListIter = ListIter_For ListFor
  
--list_for: 'for' exprlist 'in' testlist_safe [list_iter]
data ListFor = ListFor ExpressionList TestListSafe (Maybe ListIter)
--list_if: 'if' old_test [list_iter]


--comp_iter: comp_for | comp_if
data CompIter = CompIter CompFor

--comp_for: 'for' exprlist 'in' or_test [comp_iter]
data CompFor = CompFor ExpressionList OrTest (Maybe CompIter)

--comp_if: 'if' old_test [comp_iter]

--testlist1: test (',' test)*

data TestList1 = TestList1 Test [Test]
--  # not used in grammar, but may appear in "node" passed from Parser to Compiler
--encoding_decl: NAME

--yield_expr: 'yield' [testlist]
data YieldExpression = YieldExpression (Maybe TestList)
