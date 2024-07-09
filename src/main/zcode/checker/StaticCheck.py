
from AST import *
from Visitor import *
from Utils import Utils
from StaticError import *
from functools import reduce

class Zcode(Type):
    pass

class FuncZcode(Zcode):
    def __init__(self, param = [], typ = None, body = False):
        self.param = param
        self.typ = typ
        self.body = body
    def __str__(self):
        return f"FuncZcode(param=[{', '.join(str(i) for i in self.param)}],typ={str(self.typ)},body={self.body})"

class VarZcode(Zcode):
    def __init__(self, typ = None):
        self.typ = typ    
    def __str__(self):
        return f"VarZcode(type={self.typ})"

class ArrayZcode(Type):
    def __init__(self, eleType, ast):
        self.eleType = eleType
        self.ast = ast
    def __str__(self):
        return f"ArrayZcode(eleType=[{', '.join(str(i) for i in self.eleType)}])"
    
class CannotBeInferredZcode(Type):
    def __str__(self):
        return "CannotBeInferredZcode()"

class StaticChecker(BaseVisitor, Utils):
    def __init__(self,ast):
        self.ast = ast 
        self.BlockFor = 0
        self.function = None
        self.Return = False
        self.listFunction = {
            "readNumber" : FuncZcode([], NumberType(), True),
            "readBool" : FuncZcode([], BoolType(), True),
            "readString" : FuncZcode([], StringType(), True),
            "writeNumber" : FuncZcode([NumberType()], VoidType(), True),
            "writeBool" : FuncZcode([BoolType()], VoidType(), True),
            "writeString" : FuncZcode([StringType()], VoidType(), True)
        }
    
    def print(self):
        print(f"BlockFor {self.BlockFor}")
        print(f"function {str(self.function)}")
        print(f"Return {self.Return}")
        print(f"listFunction :")
        for key, value in self.listFunction.items():
            print(f"    {key}  {str(value)}")       
    

    def check(self):
        self.visit(self.ast, [{}])
        return None

    def LHS_RHS_stmt(self,LHS : Type, RHS : Type, ast, param):
        # print(f"LHS_RHS_stmt {LHS} {RHS}")
        if isinstance(LHS,CannotBeInferredZcode) or isinstance(RHS,CannotBeInferredZcode):
            raise TypeCannotBeInferred(ast)
        elif isinstance(LHS, Zcode) and isinstance(RHS, Zcode):
            raise TypeCannotBeInferred(ast)
        elif isinstance(LHS, Zcode) and isinstance(RHS, ArrayZcode): 
            raise TypeCannotBeInferred(ast)
        if isinstance(LHS,ArrayType) and isinstance(RHS,ArrayZcode):
            RHS = self.visitArrayLiteral(RHS.ast, param, LHS)
            self.LHS_RHS_stmt(LHS, RHS, ast, param)
        elif isinstance(RHS, ArrayZcode):
            raise TypeCannotBeInferred(ast)
        elif isinstance(LHS, Zcode):
            LHS.typ = RHS
        elif isinstance(RHS, Zcode):
            RHS.typ = LHS 
        elif not self.comparType(LHS, RHS):
            raise TypeMismatchInStatement(ast)
        
    def LHS_RHS_expr(self, LHS : Type, RHS : Type,ast, param) -> bool:
        if isinstance(LHS,CannotBeInferredZcode) or isinstance(RHS,CannotBeInferredZcode):
            return True
        elif isinstance(LHS, Zcode) and isinstance(RHS, Zcode):
            return True
        elif isinstance(LHS, Zcode) and isinstance(RHS, ArrayZcode): 
            return True
        if isinstance(LHS,ArrayType) and isinstance(RHS,ArrayZcode):
            RHS = self.visitArrayLiteral(RHS.ast, param, LHS)
            return self.LHS_RHS_expr(LHS, RHS, ast, param)
        elif isinstance(RHS, ArrayZcode):
            return True
        elif isinstance(LHS, Zcode):
            LHS.typ = RHS
        elif isinstance(RHS, Zcode):
            RHS.typ = LHS 
        elif not self.comparType(LHS, RHS):
            raise TypeMismatchInExpression(ast)
        return False

    def comparType(self, LHS : Type, RHS : Type) -> bool:
        if type(LHS) is not type(RHS): return False
        elif type(LHS) is ArrayType:
            if len(LHS.size) != len(RHS.size): return False
            for i in range(len(LHS.size)):
                if LHS.size[i] != RHS.size[i]: return False
            return type(LHS.eleType) is type(RHS.eleType)
        return True
    
    def visitProgram(self, ast, param):
        for i in ast.decl: self.visit(i, param)
        
        for key, value in self.listFunction.items():
            if not value.body: raise NoDefinition(key)
            
        main = self.listFunction.get("main")
        if main is None or main.param or type(main.typ) is not VoidType :
            raise NoEntryPoint()
        
    def visitVarDecl(self, ast, param):
        if param[0].get(ast.name.name): raise Redeclared(Variable(), ast.name.name)
        param[0][ast.name.name] = VarZcode(ast.varType)
        
        if ast.varInit:
            LHS = param[0][ast.name.name].typ if param[0][ast.name.name].typ else param[0][ast.name.name]
            RHS = self.visit(ast.varInit, param)
            self.LHS_RHS_stmt(LHS, RHS, ast, param)

    def visitFuncDecl(self, ast, param):
        method = self.listFunction.get(ast.name.name)
        if method and (method.body or not ast.body): raise Redeclared(Function(), ast.name.name)
        
        if ast.body is None:
            typeParam = []
            for i in ast.param:
                typeParam.append(i.varType)   
            self.listFunction[ast.name.name] = FuncZcode(typeParam, None, False)
            return

        listParam, typeParam = {}, [] 
        for i in ast.param:
            if listParam.get(i.name.name): raise Redeclared(Parameter(),i.name.name)
            listParam[i.name.name] = VarZcode(i.varType)
            typeParam.append(i.varType)

        
        if method:
            ListLHS = self.listFunction[ast.name.name].param
            ListRHS = typeParam
            if len(ListLHS) != len(ListRHS): raise Redeclared(Function(), ast.name.name)
            for i in range(len(ListLHS)):
                if not self.comparType(ListLHS[i], ListRHS[i]): 
                    raise Redeclared(Function(), ast.name.name) 
            self.listFunction[ast.name.name].body = True
        else:
            self.listFunction[ast.name.name] = FuncZcode(typeParam, None, True)
        
        self.Return = False
        self.function = self.listFunction[ast.name.name] 
        self.visit(ast.body, [listParam] + param)
        if not self.Return:
            if self.listFunction[ast.name.name].typ is None: 
                self.listFunction[ast.name.name].typ = VoidType()
            elif not isinstance(self.listFunction[ast.name.name].typ, VoidType):
                raise TypeMismatchInStatement(Return(None))
                
 
    def visitId(self, ast, param):
        for item in param:
            Id = item.get(ast.name)
            if Id: return Id.typ if Id.typ else Id
        raise Undeclared(Identifier(), ast.name)

    def visitCallExpr(self, ast, param):
        method = self.listFunction.get(ast.name.name)
        if method is None: raise Undeclared(Function(),ast.name.name)
        
        if len(method.param) != len(ast.args): raise TypeMismatchInExpression(ast)
        for i in range(len(method.param)):
            LHS = method .param[i]
            RHS = self.visit(ast.args[i], param)
            cannotBeInferred = self.LHS_RHS_expr(LHS, RHS , ast, param)
            if cannotBeInferred: return CannotBeInferredZcode()
        
        if method.typ is None:
            return method
        elif isinstance(method.typ, VoidType):
            raise TypeMismatchInExpression(ast)
        return method.typ      

    def visitCallStmt(self, ast, param):
        method = self.listFunction.get(ast.name.name)
        if method is None: raise Undeclared(Function(),ast.name.name)
        
        if len(method.param) != len(ast.args): raise TypeMismatchInStatement(ast)
        for i in range(len(method.param)):
            LHS = method .param[i]
            RHS = self.visit(ast.args[i], param)
            self.LHS_RHS_stmt(LHS, RHS, ast, param)
    
        if method.typ is None:
            method.typ = VoidType()
        elif not isinstance(method.typ, VoidType):
            raise TypeMismatchInStatement(ast)

    def visitIf(self, ast, param):
        """_typExpr_"""   
        LHS = BoolType()
        RHS = self.visit(ast.expr, param)
        self.LHS_RHS_stmt(LHS, RHS, ast, param)  
        self.visit(ast.thenStmt, param)    
        
        """_elifStmt_"""   
        for item in ast.elifStmt:
            LHS = BoolType()
            RHS = self.visit(item[0], param)
            self.LHS_RHS_stmt(LHS, RHS, ast, param)    
            self.visit(item[1], param)           

        """_elseStmt_
        """            
        if ast.elseStmt is not None:
            self.visit(ast.elseStmt, param)
        
    def visitFor(self, ast, param):
        # name: Id
        # condExpr: Expr
        # updExpr: Expr
        # body: Stmt
        """_typID_"""        
        LHS = NumberType()
        RHS = self.visit(ast.name, param)
        self.LHS_RHS_stmt(LHS, RHS, ast, param)        
        
        """_typCondExpr_"""    
        LHS = BoolType()
        RHS = self.visit(ast.condExpr, param)
        self.LHS_RHS_stmt(LHS, RHS, ast, param)     

        """_typUpdExpr_"""            
        LHS = NumberType()
        RHS = self.visit(ast.updExpr, param)
        self.LHS_RHS_stmt(LHS, RHS, ast, param)  
        
        
        self.BlockFor += 1 #! vào trong vòng for nào anh em
        self.visit(ast.body, param)
        self.BlockFor -= 1 #! cút khỏi vòng for nào anh em
    
    def visitReturn(self, ast, param):
        self.Return = True
        LHS = self.function.typ if self.function.typ else self.function
        RHS = self.visit(ast.expr, param) if ast.expr else VoidType()
        self.LHS_RHS_stmt(LHS, RHS, ast, param)

    def visitAssign(self, ast, param):
        LHS = self.visit(ast.lhs, param)
        RHS = self.visit(ast.rhs, param)
        self.LHS_RHS_stmt(LHS, RHS, ast, param)
            
    def visitBinaryOp(self, ast, param):
        op = ast.op
        cannotBeInferred = False
        if op in ['+', '-', '*', '/', '%']:
            LHS = NumberType()
            RHS = self.visit(ast.left, param)   
            cannotBeInferred = self.LHS_RHS_expr(LHS, RHS, ast, param) 
        elif op in ['=', '!=', '<', '>', '>=', '<=']:
            LHS = NumberType()
            RHS = self.visit(ast.left, param)   
            cannotBeInferred = self.LHS_RHS_expr(LHS, RHS, ast, param) 
        elif op in ['and', 'or']:
            LHS = BoolType()
            RHS = self.visit(ast.left, param)   
            cannotBeInferred = self.LHS_RHS_expr(LHS, RHS, ast, param) 
        elif op in ['==']:
            LHS = StringType()
            RHS = self.visit(ast.left, param)   
            cannotBeInferred = self.LHS_RHS_expr(LHS, RHS, ast, param)
        elif op in ['...']:
            LHS = StringType()
            RHS = self.visit(ast.left, param)   
            cannotBeInferred = self.LHS_RHS_expr(LHS, RHS, ast, param)
        
        if cannotBeInferred : return CannotBeInferredZcode()
        

        """_right_        
            TƯƠNG TỰ LEFT     
        """        
        if op in ['+', '-', '*', '/', '%']:
            LHS = NumberType()
            RHS = self.visit(ast.right, param)   
            cannotBeInferred = self.LHS_RHS_expr(LHS, RHS, ast, param) 
            return CannotBeInferredZcode() if cannotBeInferred else NumberType()
        elif op in ['=', '!=', '<', '>', '>=', '<=']:
            LHS = NumberType()
            RHS = self.visit(ast.right, param)   
            cannotBeInferred = self.LHS_RHS_expr(LHS, RHS, ast, param) 
            return CannotBeInferredZcode() if cannotBeInferred else BoolType()
        elif op in ['and', 'or']:
            LHS = BoolType()
            RHS = self.visit(ast.right, param)   
            cannotBeInferred = self.LHS_RHS_expr(LHS, RHS, ast, param) 
            return CannotBeInferredZcode() if cannotBeInferred else BoolType()
        elif op in ['==']:
            LHS = StringType()
            RHS = self.visit(ast.right, param)   
            cannotBeInferred = self.LHS_RHS_expr(LHS, RHS, ast, param) 
            return CannotBeInferredZcode() if cannotBeInferred else BoolType()
        elif op in ['...']:
            LHS = StringType()
            RHS = self.visit(ast.right, param)   
            cannotBeInferred = self.LHS_RHS_expr(LHS, RHS, ast, param) 
            return CannotBeInferredZcode() if cannotBeInferred else StringType()

    def visitUnaryOp(self, ast, param):
        op = ast.op
        if op in ['+', '-']:
            LHS = NumberType()
            RHS = self.visit(ast.operand, param)   
            cannotBeInferred = self.LHS_RHS_expr(LHS, RHS, ast, param) 
            return CannotBeInferredZcode() if cannotBeInferred else NumberType()
        if op in ['not']:
            LHS = BoolType()
            RHS = self.visit(ast.operand, param)   
            cannotBeInferred = self.LHS_RHS_expr(LHS, RHS, ast, param) 
            return CannotBeInferredZcode() if cannotBeInferred else BoolType() 
            
    def visitArrayCell(self, ast, param):
        arr = self.visit(ast.arr, param)
        if isinstance(arr, (BoolType, StringType, NumberType)):
            raise TypeMismatchInExpression(ast)
        elif not isinstance(arr, ArrayType):
            return CannotBeInferredZcode()

        for item in ast.idx:
            LHS = NumberType()
            RHS = self.visit(item, param)   
            cannotBeInferred = self.LHS_RHS_expr(LHS, RHS, ast, param)
            if cannotBeInferred: return CannotBeInferredZcode()
            
        if len(arr.size) < len(ast.idx): raise TypeMismatchInExpression(ast)
        elif len(arr.size) == len(ast.idx): return arr.eleType
        return ArrayType(arr.size[len(ast.idx):], arr.eleType)   

    def visitArrayLiteral(self, ast, param, mainTyp = None):
        
        if mainTyp is not None:
            result = mainTyp
            mainTyp = mainTyp.eleType if len(mainTyp.size) == 1 else ArrayType(mainTyp.size[1:], mainTyp.eleType) 
            
            
            for item in ast.value:
                RHS = self.visit(item, param)   
                if isinstance(RHS,CannotBeInferredZcode) or isinstance(mainTyp,CannotBeInferredZcode):
                    return CannotBeInferredZcode()
                if isinstance(mainTyp,ArrayType) and isinstance(RHS,ArrayZcode):
                    mainTyp = self.visitArrayLiteral(RHS.ast, param, mainTyp)
                elif isinstance(RHS, ArrayZcode):
                    return CannotBeInferredZcode()
                elif isinstance(RHS, Zcode):
                    RHS.typ = mainTyp
            
            return self.visitArrayLiteral(ast, param)
        
        for item in ast.value:
            checkTyp = self.visit(item, param)
            if mainTyp is None and isinstance(checkTyp, (BoolType, StringType, NumberType, ArrayType)):
                mainTyp = checkTyp
            elif isinstance(checkTyp, CannotBeInferredZcode):
                return CannotBeInferredZcode()
        if mainTyp is None: return ArrayZcode([self.visit(item, param) for item in ast.value], ast)

        for item in ast.value:
            LHS = mainTyp
            RHS = self.visit(item, param)   
            cannotBeInferred = self.LHS_RHS_expr(LHS, RHS, ast, param)
            if cannotBeInferred: return CannotBeInferredZcode()
        
        if isinstance(mainTyp, (BoolType, StringType, NumberType)):
            return ArrayType([len(ast.value)], mainTyp)
        return ArrayType([float(len(ast.value))] + mainTyp.size, mainTyp.eleType)
            
        
            
    """phần này sẽ là cố định do ngắn quá :(( """
    def visitBlock(self, ast, param):
        paramNew = [{}] + param #! tăng tầm vực
        for item in ast.stmt: 
            self.visit(item,paramNew)   
    def visitContinue(self, ast, param):
        #! kiểm tra đang ở vòng for hay không
        if self.BlockFor == 0: raise MustInLoop(ast)
    def visitBreak(self, ast, param):
        #! kiểm tra đang ở vòng for hay không
        if self.BlockFor == 0: raise MustInLoop(ast)   
    def visitNumberType(self, ast, param): return ast
    def visitBoolType(self, ast, param): return ast
    def visitStringType(self, ast, param): return ast
    def visitArrayType(self, ast, param): return ast
    def visitNumberLiteral(self, ast, param): return NumberType()
    def visitBooleanLiteral(self, ast, param): return BoolType()
    def visitStringLiteral(self, ast, param): return StringType()

        
        


