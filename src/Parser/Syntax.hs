{-#LANGUAGE GADTs #-}

module Syntax where

data Newline = Newline deriving (Show)
data Endmarker = Endmarker
type Name = String
data Number = 
  Number_Int Integer
  | Number_Float Double deriving (Show)

type String' = String
--type New
--single_input: NEWLINE | simple_stmt | compound_stmt NEWLINE

data SingleInput = 
  SingleInput_NewLine Newline 
  | SingleInput_Simple SimpleStatement 
  | SingleInput_Compound (CompoundStatement, Newline) deriving (Show)

--file_input: (NEWLINE | stmt)* ENDMARKER
data NewLineOrStatement = 
  NewLineOrStatement_NL Newline
  | NewLineOrStatement_ST Statement deriving (Show)

data FileInput = FileInput [NewLineOrStatement] deriving (Show)

--eval_input: testlist NEWLINE* ENDMARKER
data EvalInput = EvalInput TestList [Newline] deriving (Show)
--decorator: '@' dotted_name [ '(' [arglist] ')' ] NEWLINE
data Decorator = Decorator DottedName (Maybe ArgList) Newline deriving (Show)

--decorators: decorator+
type Decorators = [Decorator]

--decorated: decorators (classdef | funcdef)
data FuncOrClass = 
  FuncOrClass_Class ClassDef
  | FuncOrClass_Func FuncDef deriving (Show)

data Decorated = Decorated Decorators FuncOrClass deriving (Show)

--funcdef: 'def' NAME parameters ':' suite

data FuncDef = FuncDef Name Parameters Suite deriving (Show)

--parameters: '(' [varargslist] ')'
data Parameters = Parameters (Maybe VarArgsList) deriving (Show)

--varargslist: ((fpdef ['=' test] ',')*
--              ('*' NAME [',' '**' NAME] | '**' NAME) |
--              fpdef ['=' test] (',' fpdef ['=' test])*   [','])
data ProtoVarArgsList = 
  ProtoVarArgsList_Name Name
  | ProtoVarArgsList_Names Name (Maybe Name)
  | ProtoVarArgsList_FP [(FPDef, (Maybe Test))] deriving (Show)


data VarArgsList = VarArgsList [(FPDef, (Maybe Test))] ProtoVarArgsList deriving (Show)


--fpdef: NAME | '(' fplist ')'

data FPDef = 
  FPDef_Name Name
  | FPDef_FP FPList deriving (Show)

--fplist: fpdef (',' fpdef)* [',']
type FPList = [FPDef]

--stmt: simple_stmt | compound_stmt

data Statement = 
  Statement_Simple SimpleStatement 
  | Statement_Compound CompoundStatement deriving (Show)

--simple_stmt: small_stmt (';' small_stmt)* [';'] NEWLINE
data SimpleStatement = SimpleStatement [SmallStatement] Newline deriving (Show)

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
  | SmallStatement_Assert AssertStatement deriving (Show)




data YieldOrTest = 
  YieldOrTest_Yield YieldExpression
  | YieldOrTest_Test TestList deriving (Show)


data Assignment = 
  Assignment_Aug AugAssign YieldOrTest
  | Assignment [YieldOrTest] deriving (Show)


--expr_stmt: testlist (augassign (yield_expr|testlist) |
--                     ('=' (yield_expr|testlist))*)

data ExpressionStatement = ExpressionStatement TestList Assignment deriving (Show)

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
  | FDivideEquals deriving (Show)
--   # For normal assignments, additional restrictions enforced by the interpreter
--print_stmt: 'print' ( [ test (',' test)* [','] ] |
--                      '>>' test [ (',' test)+ [','] ] )


data PrintStatement = PrintStatement TestList deriving (Show)


--del_stmt: 'del' exprlist

data DeleteStatement = DeleteStatement ExpressionList deriving (Show)

--pass_stmt: 'pass'
data PassStatement = PassStatement deriving (Show)

--flow_stmt: break_stmt | continue_stmt | return_stmt | raise_stmt | yield_stmt

data FlowStatement = 
  FlowStatement_Break BreakStatement
  | FlowStatement_Cont ContinueStatement
  | FlowStatement_Ret ReturnStatement
  | FlowStatement_Raise RaiseStatement
  | FlowStatement_Yield YieldStatement deriving (Show)

--break_stmt: 'break'
data BreakStatement = BreakStatement deriving (Show)
--continue_stmt: 'continue'
data ContinueStatement = ContinueStatement deriving (Show)
--return_stmt: 'return' [testlist]
data ReturnStatement = ReturnStatement (Maybe TestList) deriving (Show)
--yield_stmt: yield_expr
data YieldStatement = YieldStatement YieldExpression deriving (Show)

--raise_stmt: 'raise' [test [',' test [',' test]]]
data RaiseStatement = RaiseStatement TestList deriving (Show)

--import_stmt: import_name | import_from
data ImportStatement = 
  ImportStatement_Name ImportName
  | ImportStament_From ImportFrom deriving (Show)

--import_name: 'import' dotted_as_names
data ImportName = ImportName DottedAsNames deriving (Show)

--import_from: ('from' ('.'* dotted_name | '.'+)
--              'import' ('*' | '(' import_as_names ')' | import_as_names))

--TODO: May need to reconsider dots
--data FromImport = FromImport (Maybe ImportAsNames)(Maybe DottedName)

--data Import = 
--  Import_Star
--  | Import_Paren ImportAsNames
--  | Import_Names ImportAsNames

data ImportFrom = ImportFrom (Maybe DottedName) (Maybe ImportAsNames) deriving (Show)


--import_as_name: NAME ['as' NAME]

data ImportAsName = ImportAsName Name (Maybe Name) deriving (Show)

--dotted_as_name: dotted_name ['as' NAME]
data DottedAsName = DottedAsName DottedName (Maybe Name) deriving (Show)

--import_as_names: import_as_name (',' import_as_name)* [',']

type ImportAsNames = [ImportAsName]

--dotted_as_names: dotted_as_name (',' dotted_as_name)*
type DottedAsNames = [DottedAsName]

--dotted_name: NAME ('.' NAME)*

type DottedName = [Name]

--global_stmt: 'global' NAME (',' NAME)*
data GlobalStatement = GlobalStatement [Name] deriving (Show)

--exec_stmt: 'exec' expr ['in' test [',' test]]
data ExecStatement = ExecStatement Expression (Maybe [Test]) deriving (Show)

--assert_stmt: 'assert' test [',' test]
data AssertStatement = AssertStatement [Test] deriving (Show)

--compound_stmt: if_stmt | while_stmt | for_stmt | try_stmt | with_stmt | funcdef | classdef | decorated
data CompoundStatement = 
  CompoundStatement_If IfStatement
  | CompoundStatement_While WhileStatement
  | CompoundStatement_For ForStatement
  | CompoundStatement_Try TryStatement
  | CompoundStatement_With WithStatement
  | CompoundStatement_Func FuncDef
  | CompoundStatement_Class ClassDef
  | CompoundStatement_Dec Decorated deriving (Show)

--if_stmt: 'if' test ':' suite ('elif' test ':' suite)* ['else' ':' suite]
data IfStatement = IfStatement Test Suite [(Test, Suite)] (Maybe Suite) deriving (Show)

--while_stmt: 'while' test ':' suite ['else' ':' suite]

data WhileStatement = WhileStatement Test Suite (Maybe Suite) deriving (Show)

--for_stmt: 'for' exprlist 'in' testlist ':' suite ['else' ':' suite]

data ForStatement = ForStatement ExpressionList TestList Suite (Maybe Suite) deriving (Show)

--try_stmt: ('try' ':' suite
--           (
        --    (except_clause ':' suite)+
--            ['else' ':' suite]
--            ['finally' ':' suite] |
--           'finally' ':' suite
    --       )
--         )

data TryPredicate = 
  TryPredicate_Except [(ExceptClause, Suite)] (Maybe Suite) (Maybe Suite)
  | TryPredicate_Finally Suite deriving (Show)

data TryStatement = TryStatement Suite TryPredicate deriving (Show)

--with_stmt: 'with' with_item (',' with_item)*  ':' suite

data WithStatement = WithStatement [WithItem] Suite deriving (Show)

--with_item: test ['as' expr]

data WithItem = WithItem Test (Maybe Expression) deriving (Show)

--   # NB compile.c makes sure that the default except clause is last
--except_clause: 'except' [test [('as' | ',') test] ]

data CompoundExcept = 
  Except_As
  | Except_Comma deriving (Show)

data ExceptClause = ExceptClause (Maybe Test) (Maybe (CompoundExcept, Test)) deriving (Show)

--suite: simple_stmt | NEWLINE INDENT stmt+ DEDENT
data Suite = 
  Suite_Simple SimpleStatement
  | Suite_Compound [Statement] deriving (Show)

--    # Backward compatibility cruft to support:
--    # [ x for x in lambda: True, lambda: False if x() ]
--    # even while also allowing:
--    # lambda x: 5 if x else 2
--    # (But not a mix of the two)

--old_lambdef: 'lambda' [varargslist] ':' old_test
data OldLambda = OldLambda (Maybe VarArgsList) OldTest deriving (Show)

--old_test: or_test | old_lambdef

data OldTest = 
  OldTest_Or OrTest
  | OldTest_Lam OldLambda deriving (Show)

--    testlist_safe: old_test [(',' old_test)+ [',']]
type TestListSafe = [OldTest] 

--test: or_test ['if' or_test 'else' test] | lambdef
data Test = 
  Test_Or OrTest (Maybe (OrTest, Test))
  | Test_Lam LambdaDef deriving (Show)


--or_test: and_test ('or' and_test)*
type OrTest = [AndTest]

--and_test: not_test ('and' not_test)*
type AndTest = [NotTest]

--not_test: 'not' not_test | comparison
data NotTest = 
  NotTest_Not NotTest
  | NotTest_Comp Comparison deriving (Show)

--comparison: expr (comp_op expr)*

data Comparison = Comparison Expression [(CompOperator, Expression)] deriving (Show)

--comp_op: '<'|'>'|'=='|'>='|'<='|'<>'|'!='|'in'|'not' 'in'|'is'|'is' 'not'
data Comp = 
  LT'
  | GT'
  | EQ'
  | GTE
  | LTE
  | OldNE
  | NE
  | In
  | Not
  | Is deriving (Show)

data CompOperator = CompOperator Comp deriving (Show)


--expr: xor_expr ('|' xor_expr)*
type Expression = [XORExpression] 

--xor_expr: and_expr ('^' and_expr)*
type XORExpression = [AndExpression]

--and_expr: shift_expr ('&' shift_expr)*
type AndExpression = [ShiftExpression]

--shift_expr: arith_expr (('<<'|'>>') arith_expr)*
data Shift =
  LeftS
  | RightS deriving (Show)

data ShiftExpression = ShiftExpression ArithExpression [(Shift, ArithExpression)] deriving (Show)

--arith_expr: term (('+'|'-') term)*
data Arith =
  Plus
  | Minus deriving (Show)

data ArithExpression = ArithExpression Term [(Arith, Term)] deriving (Show)

--term: factor (('*'|'/'|'%'|'//') factor)*

data TermExp =
  Star
  | Div
  | Mod
  | DDiv deriving (Show)

data Term = Term Factor [(TermExp, Factor)] deriving (Show)

--factor: ('+'|'-'|'~') factor | power
data FactorExp =
   Factor_Plus
  | Factor_Minus
  | Factor_Tilde deriving (Show)

data Factor = 
  Factor_Op FactorExp Factor
  | Factor_Power Power deriving (Show)

--power: atom trailer* ['**' factor]

data Power = Power Atom [Trailer] (Maybe Factor) deriving (Show)
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
  | Atom_String String' deriving (Show)

--listmaker: test ( list_for | (',' test)* [','] )
data ListMaker = 
  ListMaker_List Test ListFor
  | ListMaker_Test [Test] deriving (Show)

--testlist_comp: test ( comp_for | (',' test)* [','] )
data TestListComp = 
  TestListComp_Comp Test CompFor
  |TestListComp_Test [Test] deriving (Show)

--lambdef: 'lambda' [varargslist] ':' test
data LambdaDef = LambdaDef (Maybe VarArgsList) Test deriving (Show)


--trailer: '(' [arglist] ')' | '[' subscriptlist ']' | '.' NAME
data Trailer = 
  Trailer_Arglist (Maybe ArgList)
  | Trailer_SubscriptList SubscriptList
  | Trailer_Name Name deriving (Show)

--subscriptlist: subscript (',' subscript)* [',']
type SubscriptList = [Subscript]


type Ellipsis = String

--subscript: '.' '.' '.' | test | [test] ':' [test] [sliceop]
data Subscript = 
  Subscript_Ellipsis Ellipsis
  | Subscript_Test Test
  | SubScript_Slice (Maybe Test) (Maybe Test) (Maybe SliceOp) deriving (Show)

--sliceop: ':' [test]
data SliceOp = SliceOp (Maybe Test) deriving (Show)

--exprlist: expr (',' expr)* [','] 
type ExpressionList = [Expression]

--testlist: test (',' test)* [',']
type TestList = [Test]

--dictorsetmaker: ( (test ':' test (comp_for | (',' test ':' test)* [','])) |
--                  (test (comp_for | (',' test)* [','])) )


data CompOrTests = 
  CompOrTests_Comp CompFor
  | CompOrTests_Tests [(Test, Test)] deriving (Show)

data CompOrTest = 
  CompOrTest_Comp CompFor
  | CompOrTest_Tests [Test] deriving (Show)

data DictMaker = DictMaker Test Test CompOrTests deriving (Show)
  --  | DictMaker_Test Test Test [(Test, Test)]

data SetMaker = SetMaker Test CompOrTest deriving (Show)

data DictOrSetMaker = 
  DictOrSetMaker_Dict DictMaker 
  | DictOrSetMaker_Set SetMaker deriving (Show)

--classdef: 'class' NAME ['(' [testlist] ')'] ':' suite
data ClassDef = ClassDef Name (Maybe TestList) Suite deriving (Show)


--arglist: (argument ',')* (argument [',']
--                         |'*' test (',' argument)* [',' '**' test] 
--                         |'**' test)
--    # The reason that keywords are test nodes instead of NAME is that using NAME
--    # results in an ambiguity. ast.c makes sure it's a NAME.

data ArgListPred = 
  ArgList_Arg Argument
  | ArgList_Single Test [Argument] (Maybe Test)
  | ArgList_Double Test  deriving (Show)

data ArgList = ArgList [Argument] ArgListPred deriving (Show)


 --argument: test [comp_for] | test '=' test
data Argument = 
  Argument_Comp Test (Maybe CompFor)
  | Argument_Test Test Test deriving (Show)

--list_iter: list_for | list_if
-- dont' use list if
data ListIter = ListIter_For ListFor deriving (Show) 
--list_for: 'for' exprlist 'in' testlist_safe [list_iter]
data ListFor = ListFor ExpressionList TestListSafe (Maybe ListIter) deriving (Show)

--list_if: 'if' old_test [list_iter]


--comp_iter: comp_for | comp_if
data CompIter = CompIter_For CompFor deriving (Show)

--comp_for: 'for' exprlist 'in' or_test [comp_iter]
data CompFor = CompFor ExpressionList OrTest (Maybe CompIter) deriving (Show)

--comp_if: 'if' old_test [comp_iter]

--testlist1: test (',' test)*

type TestList1 = [Test]
--  # not used in grammar, but may appear in "node" passed from Parser to Compiler
--encoding_decl: NAME

--yield_expr: 'yield' [testlist]
type YieldExpression = (Maybe TestList)
