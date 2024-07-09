from Emitter import *
from functools import reduce

from Frame import Frame
from abc import ABC
from Visitor import *
from AST import *


class MType:
    def __init__(self, partype, rettype):
        self.partype = partype
        self.rettype = rettype


class CodeGenerator:
    def __init__(self):
        self.libName = "io"

    def init(self):
        return [Symbol("readNumber", MType(list(), NumberType()), CName(self.libName)),
                Symbol("writeNumber", MType([NumberType()], VoidType()), CName(self.libName)),
                Symbol("readBool", MType(list(), BoolType()), CName(self.libName)),
                Symbol("writeBool", MType([BoolType()], VoidType()), CName(self.libName)),
                Symbol("readString", MType(list(), StringType()), CName(self.libName)),
                Symbol("writeString", MType([StringType()], VoidType()), CName(self.libName))
                ]

    def gen(self, ast, path):
        # ast: AST
        # dir_: String
        gl = self.init()
        gc = CodeGenVisitor(ast, gl, path)
        gc.visit(ast, None)


class SubBody():
    def __init__(self, frame, sym):
        self.frame = frame
        self.sym = sym
class Access():
    def __init__(self, frame, sym, isLeft, isFirst = False):
        self.frame = frame
        self.sym = sym
        self.isLeft = isLeft
        self.isFirst = isFirst
class Symbol:
    def __init__(self, name, mtype, value = None):
        self.name = name
        self.mtype = mtype
        self.value = value
    def __str__(self):
        return "Symbol("+self.name+","+str(self.mtype)+")"

class Val(ABC):
    pass
class Index(Val):
    def __init__(self, value):
        self.value = value
class CName(Val):
    def __init__(self, value):
        self.value = value


class CodeGenVisitor(BaseVisitor):
    def __init__(self, astTree, env, path):
        self.astTree = astTree
        self.env = env
        self.path = path
        self.emit = Emitter(path)
        self.libName = "io"

    def visitClassDecl(self, ast, o):
        self.className = ast.classname.name
        self.emit = Emitter(self.path+"/" + self.className + ".j")
        self.emit.printout(self.emit.emitPROLOG(
            self.className, "java.lang.Object"))
        [self.visit(ele, SubBody(None, self.env))
         for ele in ast.memlist if type(ele) == MethodDecl]
        # generate default constructor
        self.genMETHOD(MethodDecl(Instance(), Id("<init>"), list(
        ), None, Block([], [])), c, Frame("<init>", VoidType()))
        self.emit.emitEPILOG()
        return o

    def genMETHOD(self, consdecl, o, frame):
        isInit = consdecl.returnType is None
        isMain = consdecl.name.name == "main" and len(
            consdecl.param) == 0 and type(consdecl.returnType) is VoidType
        returnType = VoidType() if isInit else consdecl.returnType
        methodName = "<init>" if isInit else consdecl.name.name
        intype = [ArrayType(0, StringType())] if isMain else list(
            map(lambda x: x.typ, consdecl.param))
        mtype = MType(intype, returnType)

        self.emit.printout(self.emit.emitMETHOD(
            methodName, mtype, not isInit, frame))

        frame.enterScope(True)

        glenv = o

        # Generate code for parameter declarations
        if isInit:
            self.emit.printout(self.emit.emitVAR(frame.getNewIndex(), "this", ClassType(
                Id(self.className)), frame.getStartLabel(), frame.getEndLabel(), frame))
        elif isMain:
            self.emit.printout(self.emit.emitVAR(frame.getNewIndex(), "args", ArrayType(
                0, StringType()), frame.getStartLabel(), frame.getEndLabel(), frame))
        else:
            local = reduce(lambda env, ele: SubBody(
                frame, [self.visit(ele, env)]+env.sym), consdecl.param, SubBody(frame, []))
            glenv = local.sym+glenv

        body = consdecl.body
        self.emit.printout(self.emit.emitLABEL(frame.getStartLabel(), frame))

        # Generate code for statements
        if isInit:
            self.emit.printout(self.emit.emitREADVAR(
                "this", ClassType(Id(self.className)), 0, frame))
            self.emit.printout(self.emit.emitINVOKESPECIAL(frame))
        list(map(lambda x: self.visit(x, SubBody(frame, glenv)), body.stmt))

        self.emit.printout(self.emit.emitLABEL(frame.getEndLabel(), frame))
        if type(returnType) is VoidType:
            self.emit.printout(self.emit.emitRETURN(VoidType(), frame))
        self.emit.printout(self.emit.emitENDMETHOD(frame))
        frame.exitScope()

    def visitMethodDecl(self, ast, o):
        frame = Frame(ast.name, ast.returnType)
        self.genMETHOD(ast, o.sym, frame)
        return Symbol(ast.name, MType([x.typ for x in ast.param], ast.returnType), CName(self.className))

    # # # # # PROGRAM, DECLARATIONS, AND SUPPORT FUNCTIONS # # # # #
    def visitProgram(self, ast, o):
        self.emit.printout(self.emit.emitPROLOG('ZCode', 'java/lang/Object'))
        self.emit.printout("""
.method public <init>()V
  .limit stack 1
  .limit locals 1
  .line 1
  0: aload_0
  1: invokespecial java/lang/Object/<init>()V
  4: return
.end method

.method public main([Ljava/lang/String;)V
  .limit stack 3
  .limit locals 2
  .line 5
  0: getstatic java/lang/System/out Ljava/io/PrintStream;
  3: dconst_1
  4: invokevirtual java/io/PrintStream/println(D)V
  .line 6
  7: return
.end method
                           
""")
    
    def visitVarDecl(self, ast, o):
        name = ast.name
        typ = ast.varType
        # Global variable
        if o.frame is None:
            str = self.emit.emitATTRIBUTE(name, typ, False)
            # Print and return
            self.emit.printout(str)
            return Symbol(name, typ, CName(self.className))
        # Local variable
        else:
            index = o.frame.getNewIndex()
            startLabel = o.frame.getStartLabel()
            endLabel = o.frame.getEndLabel()
            code = self.emit.emitVAR(index, name, typ, startLabel, endLabel)
            # Print and return
            self.emit.printout(code)
            return Symbol(name, typ, Index(index))

    def visitFuncDecl(self, ast, o):
        pass

    # # # # # EXPRESSIONS # # # # #
    def visitBinaryOp(self, ast, o):
        leftCode, leftTyp = self.visit(ast.left, o)
        rightCode, rightTyp = self.visit(ast.right, o)
        returnTyp = None
        op = None
        # Get op and returnTyp
        # Arithmetic: +, -, *, /, %
        if ast.op in ['+', '-']:
            returnTyp = NumberType()
            op = self.emit.emitADDOP(ast.op, returnTyp, o.frame)
        elif ast.op in ['*', '/']:
            returnTyp = NumberType()
            op = self.emit.emitMULOP(ast.op, returnTyp, o.frame)
        elif ast.op in ['%']:
            returnTyp = NumberType()
            op = self.emit.emitMOD(o.frame)
        # Logic: and, or
        elif ast.op in ['and']:
            returnTyp = BoolType()
            op = self.emit.emitANDOP(o.frame)
        elif ast.op in ['or']:
            returnTyp = BoolType()
            op = self.emit.emitOROP(o.frame)
        # Relational: =, !=, <, >, <=, >=, == 
        elif ast.op in ['=', '!=', '<', '>', '<=', '>=', '==']:
            returnTyp = BoolType()
            op = self.emit.emitREOP(ast.op, returnTyp, o.frame)
        # Return
        return leftCode + rightCode + op, returnTyp

    def visitUnaryOp(self, ast, o):
        exprCode, exprTyp = self.visit(ast.operand, Access(o.frame, o.sym, False))
        returnTyp = None
        op = None
        # not, - (sign), index operator
        if ast.op in ['not']:
            returnTyp = BoolType()
            op = self.emit.emitNOT(returnTyp, o.frame)
        elif ast.op in ['-']:
            returnTyp = NumberType()
            op = self.emit.emitNEGOP(returnTyp, o.frame)
        # Return
        return exprCode + op, returnTyp

    def visitCallExpr(self, ast, o):
        symbol = list(filter(lambda x: x.name == ast.name, self.env[::-1]))
        symbol = symbol[0]
        cname = symbol.value.value
        ctype = symbol.mtype

        if symbol.name == "readNumber":
            return self.emit.emitINVOKESTATIC(f"io/{ast.name.name}", Symbol(ast.name.name, NumberType(), []), o.frame), NumberType()
        elif symbol.name == "readString":
            return self.emit.emitINVOKESTATIC(f"io/{ast.name.name}", Symbol(ast.name.name, StringType(), []), o.frame), StringType()
        elif symbol.name == "readBool":
            return self.emit.emitINVOKESTATIC(f"io/{ast.name.name}", Symbol(ast.name.name, BoolType(), []), o.frame), BoolType()

        code = ("", list())
        for arg in ast.args:
            exprCode, exprTyp = self.visit(arg, Access(o.frame, o.sym, False))
            code = (code[0] + exprCode, code[1].append(exprTyp))

        return code[0] + self.emit.emitINVOKESTATIC(cname + "/" + ast.name, ctype, o.frame), ctype

    def visitId(self, ast, o):
        code = None
        typ = None
        for symbol in o.sym:
            if symbol.name == ast.name:
                typ = symbol.mtype
                index = symbol.value.value
                if o.isLeft == True:
                    if type(symbol.value) is Index:
                        code = self.emit.emitWRITEVAR(symbol.name, typ, index, o.frame)
                    else:
                        lexeme = symbol.value.value + "." + symbol.name
                        code = self.emit.emitPUTSTATIC(lexeme, typ, o.frame)
                else:
                    if type(symbol.value) is Index:
                        
                        code = self.emit.emitisitIVAR(symbol.name, typ, index, o.frame)
                    else:
                        lexeme = index + "." + symbol.name
                        code = self.emit.emitGETSTATIC(lexeme, typ, o.frame)
                return code, typ
    
    def visitArrayCell(self, ast, param):
        pass
    
    # # # # # STATEMENTS # # # # #
    def visitCallStmt(self, ast, o):
        symbol = next(filter(lambda x: ast.method.name == x.name, o.sym), None)
        cname = symbol.value.value
        ctype = symbol.mtype

        if ast.name.name == "writeNumber": 
            self.LHS_RHS(NumberType(), ast.args[0], o)
        elif ast.name.name == "writeBool": 
            self.LHS_RHS(BoolType(), ast.args[0], o)
        elif ast.name.name == "writeString": 
            self.LHS_RHS(StringType(), ast.args[0], o)
        
        argsCode, argsTyp = self.visit(ast.args[0], o)
        self.emit.printout(argsCode)
        self.emit.printout(self.emit.emitINVOKESTATIC(f"io/{ast.name.name}", Symbol(ast.name.name, VoidType(), [argsTyp]), o.frame))

        code = ("", list())
        for param in ast.param:
            str1, typ1 = self.visit(param, Access(o.frame, o.sym, False, True))
            code = (code[0] + str1, code[1].append(typ1))

        self.emit.printout(code[0])
        self.emit.printout(self.emit.emitINVOKESTATIC(cname + "/" + ast.method.name, ctype, o.frame))

    def visitBlock(self, ast, o):
        newSym = [[]] + o.sym
        o.frame.enterScope(False)
        # getStartLabel
        putLabel = o.frame.getStartLabel()
        self.emit.printout(self.emit.emitLABEL(putLabel, o.frame))
        for stmt in ast.stmt:
            self.visit(stmt, o)
        # getEndLabel
        putLabel = o.frame.getEndLabel()
        self.emit.printout(self.emit.emitLABEL(putLabel, o.frame))
        o.frame.exitScope()

    def visitIf(self, ast, o):
        """
        Generate code for expr => getNewLabel flabel => Jump to flabel if False
        => Generate code for tstmt
        """
        exprCode, exprTyp = self.visit(ast.expr, Access(o.frame, o.sym, False))
        self.emit.printout(exprCode)
        flabel = o.frame.getNewLabel()
        jumpToflabel = self.emit.emitIFFALSE(flabel, o.frame)
        self.emit.printout(jumpToflabel)
        self.visit(ast.tstmt, o)
        if ast.estmt is None:
            """ Put flabel """
            putLabel = self.emit.emitLABEL(flabel, o.frame)
            self.emit.printout(putLabel)
        else:
            """
            getNewLabel elabel => Jump to elabel => Put flabel
            => Generate code for estmt => Put elabel
            """
            elabel = o.frame.getNewLabel()
            jumpToelabel = self.emit.emitGOTO(elabel, o.frame)
            self.emit.printout(jumpToelabel)
            putLabel = self.emit.emitLABEL(flabel, o.frame)
            self.emit.printout(putLabel)
            self.visit(ast.estmt, o)
            putLabel = self.emit.emitLABEL(elabel, o.frame)
            self.emit.printout(putLabel)

    def visitFor(self, ast, param):
        pass

    def visitContinue(self, ast, o):
        self.emit.printout(self.emit.emitGOTO(o.frame.getContinueLabel(), o.frame))

    def visitBreak(self, ast, o):
        self.emit.printout(self.emit.emitGOTO(o.frame.getBreakLabel(), o.frame))

    def visitReturn(self, ast, o):
        if ast.expr:
            exprCode, exprTyp = self.visit(ast.expr, Access(o.frame, o.sym, False))
            self.emit.printout(exprCode)
        else:
            self.emit.printout(self.emit.emitRETURN(VoidType(), o.frame))

    def visitAssign(self, ast, o):
        rightCode, rightTyp = self.visit(ast.rhs, Access(o.frame, o.sym, False))
        leftCode, leftTyp = self.visit(ast.lhs, Access(o.frame, o.sym, True))
        if type(leftTyp) is ArrayType:
            return self.emit.emitASTORE(self.arrayCell, frame)
        else:
            return self.emit.printout(rightCode + leftCode)

    def visitNumberLiteral(self, ast, o):
        return self.emit.emitPUSHCONST(ast.value, NumberType(), o.frame), NumberType()

    def visitStringLiteral(self, ast, o):
        return self.emit.emitPUSHCONST(ast.value, StringType(), o.frame), StringType()

    def visitBooleanLiteral(self, ast, o):
        return self.emit.emitPUSHICONST(str(ast.val).lower(), o.frame), BoolType()

    def visitArrayLiteral(self, ast, o):
        pass

    def visitNumberType(self, ast, o):
        return None, NumberType()

    def visitBoolType(self, ast, o):
        return None, BoolType()

    def visitStringType(self, ast, o):
        return None, StringType()

    def visitArrayType(self, ast, o):
        return None, ast