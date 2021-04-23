import string
import math

TT_INT		= 'INT'
TT_FLOAT    = 'FLOAT'
TT_STRING   = 'STRING'

TT_PLUS     = 'PLUS'
TT_MINUS    = 'MINUS'
TT_MUL      = 'MUL'
TT_DIV      = 'DIV'
TT_WDIV     = 'WDIV'
TT_MOD      = 'MOD'
TT_EQ       = 'EQ'
TT_LPAREN   = 'LPAREN'
TT_RPAREN   = 'RPAREN'
TT_LSQUARE   ="LSQUARE"
TT_RSQUARE   ="RSQUARE"
TT_COMMA    = 'COMMA'
TT_ARROW    = 'ARROW'

TT_NEWLINE  ='NEWLINE'

TT_THEN='THEN'

TT_EE	= 'EE' #
TT_NE	= 'NE' #
TT_LT	= 'LT' #
TT_GT	= 'GT' #
TT_LTE	= 'LTE' #
TT_GTE	= 'GTE' #

TT_IDENTIFIER='IDENTIFIER'
TT_KEYWORD   ='KEYWORD'

TT_EOF="EOF"
TT_NEWLINE  ='NEWLINE'
TT_END      ='END'

DIGITS = '0123456789'
LETTERS=string.ascii_letters
LETTERS_DIGITS=LETTERS+DIGITS

KEYWORDS = [
	'var',
    'if',
    'elsif',
    'else',
    'for',
    'to',
    'step',
    'while',
    'and',
    'or',
    'not',
    'func',
]

class Error:
    def __init__(self,position_start,position_end,error,details):
        self.position_start=position_start
        self.position_end=position_end
        self.error=error
        self.details=details

    def as_srting(self):
        
        result  = f'{self.error}: {self.details}\n'
        # result += f'File {self.position_start.file_name}, line {self.position_start.line + 1}'
        # result += '\n\n' + arrows.string_with_arrows(self.position_start.file_text, self.position_start, self.position_end)
        return result

class IllegalCharacterError(Error):
    def __init__(position_start,position_end,self, details):
        print(position_start)
        super().__init__(position_start,position_end,'Illegal Character', details)

class ExpectedCharError(Error):
	def __init__(self, position_start, position_end, details):
		super().__init__(position_start, position_end, 'Expected Character', details)

class InvalidSyntaxError(Error):
    def __init__(position_start,position_end,self, details):
        super().__init__(position_start,position_end,'Invalid Syntax', details)

class RuntimeError(Error):
    def __init__(position_start,position_end,self, details ,context):
        super().__init__(position_start,position_end,'Runtime Error', details)
  
class Token:
    def __init__(self,type,value=None,position_start=None, position_end=None):
        self.type=type
        self.value=value

        if position_start:
            self.position_start = position_start.copy()
            self.position_end = position_start.copy()
            self.position_end.advance()	

        if position_end:
            self.position_end = position_end

    def __repr__(self):
        if self.value: return f'{self.type}:{self.value}'
        return f'{self.type}'

    def matches(self, type_, value):
	    return self.type == type_ and self.value == value

class Position:
    def __init__(self, idx, line, col,file_name,file_text):
        self.idx = idx
        self.line = line
        self.col = col
        self.file_name=file_name
        self.file_text=file_text
    
    def advance(self,current_char=None):
        self.idx+=1
        self.col+=1

        if current_char=='\n':
            self.line+=1
            self.col=0

    def copy(self):
        return Position(self.idx, self.line, self.col,self.file_name,self.file_text)

class Lexer:
    def __init__(self,file_name,text):
        self.file_name=file_name
        self.text=text
        self.position=Position(-1, 0, -1,file_name,text)
        self.current_char=None
        self.advance()

    def advance(self):
        self.position.advance(self.current_char)
        
        self.current_char=self.text[self.position.idx] if self.position.idx < len(self.text) else None
    
    def makeTokens(self):
        tokens=[]
       
        while self.current_char!=None:
            if self.current_char in ' \t':
                self.advance()
            elif self.current_char in DIGITS:
                tokens.append(self.makeNumber())
            elif self.current_char in LETTERS:
                tokens.append(self.makeIdentifier())
            elif self.current_char == '"':
                tokens.append(self.make_string())
            elif self.current_char == '+':
                tokens.append(Token(TT_PLUS,position_start=self.position))
                self.advance()
            elif self.current_char == '-':
                tokens.append(Token(TT_MINUS,position_start=self.position))
                self.advance()
            elif self.current_char == '*':
                tokens.append(Token(TT_MUL,position_start=self.position))
                self.advance()
            elif self.current_char == '/':
                tokens.append(self.make_div())
            elif self.current_char == '%':
                tokens.append(Token(TT_MOD,position_start=self.position))
                self.advance()
            elif self.current_char == '(':
                tokens.append(Token(TT_LPAREN,position_start=self.position))
                self.advance()
            elif self.current_char == ')':
                tokens.append(Token(TT_RPAREN,position_start=self.position))
                self.advance()
            elif self.current_char == '[':
                tokens.append(Token(TT_LSQUARE,position_start=self.position))
                self.advance()
            elif self.current_char == ']':
                tokens.append(Token(TT_RSQUARE,position_start=self.position))
                self.advance()
            elif self.current_char == ':':
                tokens.append(Token(TT_THEN,position_start=self.position))
                self.advance()
            elif self.current_char == ',':
                tokens.append(Token(TT_COMMA,position_start=self.position))
                self.advance()
            elif self.current_char == '\n':
                tokens.append(Token(TT_NEWLINE,position_start=self.position))
                self.advance()  
            elif self.current_char == ';':
                tokens.append(Token(TT_END,position_start=self.position))
                self.advance()
            
            elif self.current_char == '!':
                token, error = self.make_not_equals()
                if error: return [], error
                tokens.append(token)
            elif self.current_char == '=':
                tokens.append(self.make_equals())
            elif self.current_char == '<':
                tokens.append(self.make_less_than())
            elif self.current_char == '>':
                tokens.append(self.make_greater_than())
            else:
                position_start=self.position.copy()
                char=self.current_char
                self.advance()
                return [],IllegalCharacterError(position_start,self.position,"'" + char + "'")

        tokens.append(Token(TT_EOF, position_start=self.position))

        return tokens,None

    def make_div(self):
        self.advance()

        if self.current_char == '/':
            self.advance()
            return Token(TT_WDIV,position_start=self.position)
        else:
            return Token(TT_DIV,position_start=self.position)

    def make_string(self):
        string=""
        position_start=self.position.copy()
        escape_character=False

        self.advance()

        escape_characters={
            'n':'\n',
            't':'\t'
        }

        while self.current_char!='"' and self.current_char is not None or escape_character:
            if escape_character:
                string+=escape_characters.get(self.current_char)
            else:
                if self.current_char== '\\' :
                    escape_character = True
                else:
                    string+=self.current_char

            self.advance()
            escape_character=False
        
        self.advance()
        return Token(TT_STRING,string,position_start,self.position)
        
    def makeNumber(self):
        number_str=''
        dotCount=0
        position_start=self.position.copy()

        while self.current_char != None and self.current_char in DIGITS + '.':
            if self.current_char==".":
                if dotCount==1: break
                dotCount+=1
                number_str+="."
            else:
                number_str+=self.current_char

            self.advance()
            
        if dotCount==0:
            return Token(TT_INT, int(number_str),position_start,self.position)
        else:
            return Token(TT_FLOAT, float(number_str),position_start,self.position)

    def makeIdentifier(self):
        identifier_str=''
        position_start=self.position.copy()

        while self.current_char != None and self.current_char in LETTERS_DIGITS+'_':
           identifier_str+=self.current_char
           self.advance()
        
        token_type=TT_KEYWORD if identifier_str in KEYWORDS else TT_IDENTIFIER
        return Token(token_type,identifier_str,position_start,self.position)
    
    def make_not_equals(self):
        position_start=self.position.copy()
        self.advance()
        if self.current_char=="=":
            self.advance()
            return Token(TT_NE,position_start=position_start,position_end=self.position),None

        self.advance()
        return None, ExpectedCharError(position_start, self.pos, "'=' (after '!')")

    def make_equals(self):
        position_start=self.position.copy()
        token_type=TT_EQ

        self.advance()

        if self.current_char=="=":
            self.advance()
            token_type=TT_EE

        return Token(token_type, position_start=position_start, position_end=self.position)

    def make_less_than(self):
        position_start=self.position.copy()
        token_type=TT_LT

        self.advance()

        if self.current_char=="=":
            self.advance()
            token_type=TT_LTE

        return Token(token_type, position_start=position_start, position_end=self.position)

    def make_greater_than(self):
        position_start=self.position.copy()
        token_type=TT_GT

        self.advance()

        if self.current_char=="=":
            self.advance()
            token_type=TT_GTE

        return Token(token_type, position_start=position_start, position_end=self.position)


class NumberNode:
    def __init__(self,token):
        self.token=token

        self.position_start=self.token.position_start
        self.position_end=self.token.position_end

    def __repr__(self):
	    return f'{self.token}'

class BinOpNode:
	def __init__(self, left_node, operation_token, right_node):
		self.left_node = left_node
		self.operation_token = operation_token
		self.right_node = right_node

		self.position_start = self.left_node.position_start
		self.position_end = self.right_node.position_end

	def __repr__(self):
		return f'({self.left_node}, {self.operation_token}, {self.right_node})'

class UnaryOpNode:
    def __init__(self,operation_token,node):
        self.operation_token=operation_token
        self.node=node
        self.position_start=self.operation_token.position_start
        self.position_end=node.position_end

    def __repr__(self):
        return f'({self.operation_token},{self.node})'

class StringNode:
    def __init__(self,token):
        self.token=token

        self.position_start=self.token.position_start
        self.position_end=self.token.position_end

    def __repr__(self):
	    return f'{self.token}'

class ListNode:
    def __init__(self,element_nodes,position_start,position_end):
        self.element_nodes=element_nodes

        self.position_start=position_start
        self.position_end=position_end

class VarAccessNode:
    def __init__(self, var_name_token):
        self.var_name_token=var_name_token
        self.position_start=self.var_name_token.position_start
        self.position_end=self.var_name_token.position_end
    def __repr__(self):
	    return f'{self.var_name_token}'

class VarAssignNode:
    def __init__(self,var_name_token,value_node):
        self.var_name_token=var_name_token
        self.value_node=value_node
        self.position_start=self.var_name_token.position_start
        self.position_end=self.var_name_token.position_end

class IfNode:
	def __init__(self, cases, else_case):
		self.cases = cases
		self.else_case = else_case

		self.position_start = self.cases[0][0].position_start
		self.position_end = (self.else_case or self.cases[len(self.cases) - 1][0]).position_end

class ForNode:
    def __init__(self,var_name_token,start_value_node,end_value_node,step_value_node, body_node):
        self.var_name_token = var_name_token
        self.start_value_node = start_value_node
        self.end_value_node = end_value_node
        self.step_value_node = step_value_node
        self.body_node = body_node

        self.position_start = self.var_name_token.position_start
        self.position_end = self.body_node.position_end

class WhileNode:
	def __init__(self, condition_node, body_node):
		self.condition_node = condition_node
		self.body_node = body_node

		self.position_start = self.condition_node.position_start
		self.position_end = self.body_node.position_end

class FuncDefNode:
    def __init__(self,var_name_token, arg_name_tokens,body_node):
        self.var_name_token = var_name_token
        self.arg_name_tokens = arg_name_tokens
        self.body_node = body_node

        if self.var_name_token:
            self.position_start = self.var_name_token.position_start
        elif len(self.arg_name_tokens) > 0:
            self.position_start = self.arg_name_tokens[0].position_start
        else:
            self.position_start = self.body_node.position_start

        self.position_end = self.body_node.position_end

class CallNode:
    def __init__(self, node_to_call, arg_nodes):
        self.node_to_call = node_to_call
        self.arg_nodes = arg_nodes

        self.position_start = self.node_to_call.position_start

        if len(self.arg_nodes) > 0:
            self.position_end = self.arg_nodes[len(self.arg_nodes) - 1].position_end
        else:
            self.position_end = self.node_to_call.position_end

class ParseResult:
  def __init__(self):
    self.error = None
    self.node = None
    self.last_registered_advance_count = 0
    self.advance_count = 0

  def register_advancement(self):
    self.last_registered_advance_count = 1
    self.advance_count += 1

  def register(self, res):
    self.last_registered_advance_count = res.advance_count
    self.advance_count += res.advance_count
    if res.error: self.error = res.error
    return res.node

  def success(self, node):
    self.node = node
    return self

  def failure(self, error):
    if not self.error or self.last_registered_advance_count == 0:
      self.error = error
    return self


class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.token_idx = -1
        self.advance()

    def advance(self):
        self.token_idx+=1

        if self.token_idx < len(self.tokens):
            self.current_token = self.tokens[self.token_idx]
        return self.current_token
    
    def parse(self):
        res = self.expression()
        if not res.error and self.current_token.type != TT_EOF:
            return res.failure(InvalidSyntaxError(
				None, None ,
				"Expected identifier '+', '-', '*' or '/'"
			))
        return res
    
    def list_expr(self):
        res=ParseResult()
        element_nodes=[]
        position_start=self.current_token.position_start.copy()

        if self.current_token.type!=TT_LSQUARE:
            return res.failure(InvalidSyntaxError(
				None, None ,
				"Expected '['"
			))
        
        res.register_advancement()
        self.advance()

        if self.current_token.type == TT_RSQUARE:
            res.register_advancement()
            self.advance()
        else:
            element_nodes.append(res.register(self.expression()))
            if res.error:
                return res.failure(InvalidSyntaxError(
                self.current_token.position_start, self.current_token.position_end,
                "Expected ']', 'VAR', 'IF', 'FOR', 'WHILE', 'FUN', int, float, identifier, '+', '-', '(', '[' or 'NOT'"
                ))

            while self.current_token.type == TT_COMMA:
                res.register_advancement()
                self.advance()

                element_nodes.append(res.register(self.expression()))
                if res.error: return res

            if self.current_token.type != TT_RSQUARE:
                return res.failure(InvalidSyntaxError(
                self.current_token.position_start, self.current_token.position_end,
                f"Expected ',' or ']'"
                ))
            
        res.register_advancement()
        self.advance()
            
        return res.success(ListNode(element_nodes,position_start, self.current_token.position_end.copy()))

    def if_expr(self):
        
        res = ParseResult()
        cases = []
        else_case = None

        if  self.current_token.type==TT_THEN:
            res.register_advancement()
            self.advance()

        res.register_advancement()
        self.advance()

        condition = res.register(self.expression())
        if res.error: return res

        

        if  self.current_token.type==TT_THEN:
                res.register_advancement()
                self.advance()

        expr = res.register(self.expression())
        if res.error: return res
        cases.append((condition, expr))

        while self.current_token.matches(TT_KEYWORD, 'elsif'):
            res.register_advancement()
            self.advance()

            condition = res.register(self.expression())
            if res.error: return res

           

            if  self.current_token.type!=TT_THEN:
                return res.failure(InvalidSyntaxError(
                    None, None ,
                    f"Expected ':'"
                ))

            res.register_advancement()
            self.advance()

            expr = res.register(self.expression())
            if res.error: return res
            cases.append((condition, expr))

        if self.current_token.matches(TT_KEYWORD, 'else'):
            res.register_advancement()
            self.advance()

            if  self.current_token.type==TT_THEN:
                res.register_advancement()
                self.advance()
            

            else_case = res.register(self.expression())
            if res.error: return res


        return res.success(IfNode(cases, else_case))

    def for_expr(self):
        res = ParseResult()

        if not self.current_token.matches(TT_KEYWORD, 'for'):
            return res.failure(InvalidSyntaxError(
                self.current_token.position_start, self.current_token.position_end,
                f"Expected 'for'"
            ))

        res.register_advancement()
        self.advance()

        if self.current_token.type != TT_IDENTIFIER:
            return res.failure(InvalidSyntaxError(
                self.current_token.position_start, self.current_token.position_end,
                f"Expected identifier"
            ))

        var_name = self.current_token
        res.register_advancement()
        self.advance()

        if self.current_token.type != TT_EQ:
            return res.failure(InvalidSyntaxError(
                self.current_token.position_start, self.current_token.position_end,
                f"Expected '='"
            ))
        
        res.register_advancement()
        self.advance()

        start_value = res.register(self.expression())
        if res.error: return res

        if not self.current_token.matches(TT_KEYWORD, 'to'):
            return res.failure(InvalidSyntaxError(
                self.current_token.position_start, self.current_token.position_end,
                f"Expected 'to'"
            ))
        
        res.register_advancement()
        self.advance()

        end_value = res.register(self.expression())
        if res.error: return res

        if self.current_token.matches(TT_KEYWORD, 'step'):
            res.register_advancement()
            self.advance()

            step_value = res.register(self.expression())
            if res.error: return res
        else:
            step_value = None

        if self.current_token.type!=TT_THEN:
            return res.failure(InvalidSyntaxError(
                None,None,
                f"Expected ':'"
            ))

        res.register_advancement()
        self.advance()

        body = res.register(self.expression())
        if res.error: return res

        return res.success(ForNode(var_name, start_value, end_value, step_value, body))
    
    def while_expr(self):
        res = ParseResult()

        if not self.current_token.matches(TT_KEYWORD, 'while'):
            return res.failure(InvalidSyntaxError(
               None,None,
                f"Expected 'while'"
            ))

        res.register_advancement()
        self.advance()

        condition = res.register(self.expression())
        if res.error: return res

        if not self.current_token.type==TT_THEN:
            return res.failure(InvalidSyntaxError(
                None,None,
                f"Expected ':'"
            ))

        res.register_advancement()
        self.advance()

        body = res.register(self.expression())
        if res.error: return res

        return res.success(WhileNode(condition, body))
    
    def func_def(self):
        res = ParseResult()

        if not self.current_token.matches(TT_KEYWORD, 'func'):
            return res.failure(InvalidSyntaxError(
                self.current_token.position_start, self.current_token.position_end,
                f"Expected 'FUN'"
            ))

        res.register_advancement()
        self.advance()

        if self.current_token.type==TT_IDENTIFIER:
            var_name_token=self.current_token
            res.register_advancement()
            self.advance()

            if self.current_token.type != TT_LPAREN:
                return res.failure(InvalidSyntaxError(
                    self.current_token.position_start, self.current_token.position_end,
                    f"Expected '('"
                ))
        else: 
            var_name_token=None

            if self.current_token.type != TT_LPAREN:
                return res.failure(InvalidSyntaxError(
                    self.current_token.position_start, self.current_token.position_end,
                    f"Expected identifier or '('"
                ))

        res.register_advancement()
        self.advance()
        arg_name_tokens=[]

        if self.current_token.type == TT_IDENTIFIER:
            arg_name_tokens.append(self.current_token)
            res.register_advancement()
            self.advance()

            while self.current_token.type == TT_COMMA:
                res.register_advancement()
                self.advance()

                if self.current_token.type != TT_IDENTIFIER:
                    return res.failure(InvalidSyntaxError(
                        self.current_token.position_start, self.current_token.position_end,
                        f"Expected identifier"
                    ))
                
                arg_name_tokens.append(self.current_token)
                res.register_advancement()
                self.advance()
            
            if self.current_token.type != TT_RPAREN:
                return res.failure(InvalidSyntaxError(
                    self.current_token.position_start, self.current_token.position_end,
                    f"Expected ',' or ')'"
                ))
        else:
            if self.current_token.type != TT_RPAREN:
                return res.failure(InvalidSyntaxError(
                    self.current_token.position_start, self.current_token.position_end,
                    f"Expected identifier or ')'"
                ))
        
        res.register_advancement()
        self.advance()

        if self.current_token.type != TT_THEN:
            return res.failure(InvalidSyntaxError(
                self.current_token.position_start, self.current_token.position_end,
                f"Expected ':'"
            ))
        
        res.register_advancement()
        self.advance()
        
        node_to_return=res.register(self.expression())
       
        if res.error: return res

        return res.success(FuncDefNode(
			var_name_token,
			arg_name_tokens,
			node_to_return
		))
    
    def call(self):
        res = ParseResult()
        factor = res.register(self.factor())
        if res.error: return res

        if self.current_token.type == TT_LPAREN:
            res.register_advancement()
            self.advance()
            arg_nodes = []

            if self.current_token.type == TT_RPAREN:
                res.register_advancement()
                self.advance()
            else:
                arg_nodes.append(res.register(self.expression()))
                if res.error:
                    return res.failure(InvalidSyntaxError(
                        self.current_token.position_start, self.current_token.position_end,
                        "Expected ')', 'VAR', 'IF', 'for', 'WHILE', 'FUN', int, float, identifier, '+', '-', '(' or 'NOT'"
                    ))

                while self.current_token.type == TT_COMMA:
                    res.register_advancement()
                    self.advance()

                    arg_nodes.append(res.register(self.expression()))
                    if res.error: return res

                if self.current_token.type != TT_RPAREN:
                    return res.failure(InvalidSyntaxError(
                        self.current_token.position_start, self.current_token.position_end,
                        f"Expected ',' or ')'"
                    ))

                res.register_advancement()
                self.advance()
            return res.success(CallNode(factor, arg_nodes))
        return res.success(factor)

    def factor(self):
        res = ParseResult()
        token=self.current_token

        if token.type in (TT_PLUS,TT_MINUS):
            res.register_advancement()
            self.advance()
            factor=res.register(self.factor())
            if res.error:return res
            return res.success(UnaryOpNode(token,factor))

        elif token.type in (TT_INT,TT_FLOAT):
            res.register_advancement()
            self.advance()
            return res.success(NumberNode(token))
        
        elif token.type in (TT_STRING):
            res.register_advancement()
            self.advance()
            return res.success(StringNode(token))

        elif token.type==TT_IDENTIFIER:
            res.register_advancement()
            self.advance()
            return res.success(VarAccessNode(token))
        
        elif token.type == TT_LPAREN:
            res.register_advancement()
            self.advance()
            expr = res.register(self.expression())
            if res.error: return res
            if self.current_token.type == TT_RPAREN:
                res.register_advancement()
                self.advance()
                return res.success(expr)
            else:
                return res.failure(InvalidSyntaxError(
                    token.position_start, token.position_end,
                    "Expected ')'"
                ))
        
        elif token.type==TT_LSQUARE:
            list_expr = res.register(self.list_expr())
            if res.error: return res
            return res.success(list_expr)

        elif token.matches(TT_KEYWORD, 'if'):
            if_expr = res.register(self.if_expr())
            if res.error: return res
            return res.success(if_expr)
        
        elif token.matches(TT_KEYWORD, 'for'):
            for_expr = res.register(self.for_expr())
            if res.error: return res
            return res.success(for_expr)
        
        elif token.matches(TT_KEYWORD, 'while'):
            while_expr = res.register(self.while_expr())
            if res.error: return res
            return res.success(while_expr)
        
        elif token.matches(TT_KEYWORD, 'func'):
            while_expr = res.register(self.func_def())
            if res.error: return res
            return res.success(while_expr)
        

        # print(token)

        return res.failure(InvalidSyntaxError(
			token.position_start, token.position_end,
			"Expected int, float or identifier"
		))        

    def term(self):
        return self.binaryOperation(self.call, (TT_MUL, TT_DIV,TT_WDIV,TT_MOD ))
    
    def comp_exp(self):
        res=ParseResult()

        if self.current_token.matches(TT_KEYWORD, 'not'):
            operation_token=self.current_token
            res.register_advancement()
            self.advance()
        
            node= res.register(self.comp_exp())
            if res.error:return res
            return res.success(UnaryOpNode(operation_token, node))
        
        node = res.register(self.binaryOperation(self.arith_exp, (TT_EE,TT_NE,TT_LT,TT_GT,TT_LTE,TT_GTE)))

        if res.error: 
            return res.failure(InvalidSyntaxError(
				self.current_token.position_start, self.current_token.position_end,
				"Expected int, float, identifier, '+', '-', '(' or 'not'"
			))
        
        return res.success(node)
    
    def arith_exp(self):
        return self.binaryOperation(self.term, (TT_PLUS,TT_MINUS))

    def expression(self):
        res=ParseResult()
        
        if self.current_token.matches(TT_KEYWORD,'var'):
            res.register_advancement()
            self.advance()

            if self.current_token.type!=TT_IDENTIFIER:
                return res.failure(InvalidSyntaxError(None,None,    
                    "Expected a variable name"
                ))
            
            var_name=self.current_token
            res.register_advancement()
            self.advance()

            if self.current_token.type!=TT_EQ:
                return res.failure(InvalidSyntaxError(None,None,
                    "Expected '='"
                ))
            
            res.register_advancement()
            self.advance()
            expr=res.register(self.expression())
            if res.error:return res
            return res.success(VarAssignNode(var_name,expr))
       
            

        node= res.register(self.binaryOperation(self.comp_exp, ((TT_KEYWORD,"and"),(TT_KEYWORD,"or"))))
        if res.error:
            return res.failure(InvalidSyntaxError(
				self.current_token.position_start, self.current_token.position_end,
				"Expected 'VAR', int, float, identifier, '+', '-' or '('"
			))

        return res.success(node)
    
    

    def binaryOperation(self,func,ops):
        res = ParseResult()
        left = res.register(func())
        if res.error: return res
      
        while self.current_token.type in ops or (self.current_token.type, self.current_token.value) in ops:
            operation_token=self.current_token
            res.register_advancement()
            self.advance()
            right = res.register(func())
            if res.error: return res
            left = BinOpNode(left, operation_token, right)

        return res.success(left)


class RuntimeResult:
	def __init__(self):
		self.value = None
		self.error = None

	def register(self, res):
		if res.error: self.error = res.error
		return res.value

	def success(self, value):
		self.value = value
		return self

	def failure(self, error):
		self.error = error
		return self

class Value:
	def __init__(self):
		self.set_position()
		self.set_context()

	def set_position(self, position_start=None, position_end=None):
		self.position_start = position_start
		self.position_end = position_end
		return self

	def set_context(self, context=None):
		self.context = context
		return self

	def added_to(self, other):
		return None, self.illegal_operation(other)

	def subtracted_by(self, other):
		return None, self.illegal_operation(other)

	def multiplied_by(self, other):
		return None, self.illegal_operation(other)

	def divided_by(self, other):
		return None, self.illegal_operation(other)

	def get_comparison_eq(self, other):
		return None, self.illegal_operation(other)

	def get_comparison_ne(self, other):
		return None, self.illegal_operation(other)

	def get_comparison_lt(self, other):
		return None, self.illegal_operation(other)

	def get_comparison_gt(self, other):
		return None, self.illegal_operation(other)

	def get_comparison_lte(self, other):
		return None, self.illegal_operation(other)

	def get_comparison_gte(self, other):
		return None, self.illegal_operation(other)

	def anded_by(self, other):
		return None, self.illegal_operation(other)

	def ored_by(self, other):
		return None, self.illegal_operation(other)

	def notted(self):
		return None, self.illegal_operation(other)

	def execute(self, args):
		return RuntimeResult()().failure(self.illegal_operation())

	def copy(self):
		raise Exception('No copy method defined')

	def is_true(self):
		return False

	def illegal_operation(self, other=None):
		if not other: other = self
		return RuntimeError(
			self.position_start, other.position_end,
			'Illegal operation',
			self.context
		)

class Number(Value):
    def __init__(self,value):
        self.value=value
        self.set_position()
        self.set_context()

    def set_position(self,position_start=None,position_end=None):
        self.position_start=position_start
        self.position_end=position_end
        return self
    
    def set_context(self, context=None):
	    self.context = context
	    return self
    
    def added_to(self,other):
        if isinstance(other, Number):
            return(Number(self.value+other.value)).set_context(self.context),None
        else:
            return None, Value.illegal_operation(self, other)

    def subtracted_by(self,other):
        if isinstance(other, Number):
            return(Number(self.value-other.value)).set_context(self.context),None
        else:
            return None, Value.illegal_operation(self, other)

    def multiplied_by(self,other):
        if isinstance(other, Number):
            return(Number(self.value*other.value)).set_context(self.context),None
        else:
            return None, Value.illegal_operation(self, other)

    def divided_by(self,other):
        if isinstance(other, Number):
            if other.value==0:
                return None,RuntimeError(other.position_start, other.position_end, "Division By Zero", self.context)

            return(Number(self.value/other.value)).set_context(self.context),None
        else:
            return None, Value.illegal_operation(self, other)

        
    def w_divided_by(self,other):
        if isinstance(other, Number):
            if other.value==0:
                return None,RuntimeError(other.position_start, other.position_end, "Division By Zero", self.context)

            return(Number(math.floor(self.value/other.value))).set_context(self.context),None
        else:
            return None, Value.illegal_operation(self, other)
    
    def moduled_by(self,other):
        if isinstance(other, Number):
            return(Number(self.value%other.value)).set_context(self.context),None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_eq(self, other):
        if isinstance(other, Number):
            return Number(int(self.value == other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_ne(self, other):
        if isinstance(other, Number):
            return Number(int(self.value != other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_lt(self, other):
        if isinstance(other, Number):
            return Number(int(self.value < other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_gt(self, other):
        if isinstance(other, Number):
            return Number(int(self.value > other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_lte(self, other):
        if isinstance(other, Number):
            return Number(int(self.value <= other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_gte(self, other):
        if isinstance(other, Number):
            return Number(int(self.value >= other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def anded_by(self, other):
        if isinstance(other, Number):
            return Number(int(self.value and other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def ored_by(self, other):
        if isinstance(other, Number):
            return Number(int(self.value or other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def notted(self):
        return Number(1 if self.value == 0 else 0).set_context(self.context), None
    
    def is_true(self):
        return self.value != 0

    def copy(self):
        copy = Number(self.value)
        copy.set_position(self.position_start, self.position_end)
        copy.set_context(self.context)
        return copy

    def __repr__(self):
	    return str(self.value)

Number.null = Number(0)
Number.false = Number(0)
Number.true = Number(1)
Number.math_PI = Number(math.pi)

class String(Value):
    def __init__(self,value):
        super().__init__()
        self.value=value
    
    def added_to(self, other):
        if isinstance(other, String):
            return String(self.value + other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def multiplied_by(self, other):
        if isinstance(other, Number):
            return String(self.value * other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)
        
    def get_comparison_eq(self, other):
        if isinstance(other, String):
            return Number(int(self.value == other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_ne(self, other):
        if isinstance(other, String):
            return Number(int(self.value != other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def is_true(self):
        return len(self.value) > 0

    def copy(self):
        copy = String(self.value)
        copy.set_position(self.position_start, self.position_end)
        copy.set_context(self.context)
        return copy

    def __repr__(self):
        return f'"{self.value}"'

class List(Value):
    def __init__(self,elements):
        super().__init__()
        self.elements=elements

    def added_to(self, other):
        if isinstance(other, List):
            new_list = self.copy()
            new_list.elements.extend(other.elements)
            return new_list, None

        elif isinstance(other, Number):
            new_list=self.copy()
            new_list.elements.append(other)
            return new_list,None
        else:
            return None, Value.illegal_operation(self, other)

    def subtracted_by(self,other):
        if isinstance(other, Number):
            new_list = self.copy()
            try:
                new_list.elements.pop(other.value)
                return new_list, None
            except:
                return None, RuntimeError(
                other.position_start, other.position_end,
                'Element at this index could not be removed from list because index is out of bounds',
                self.context
                )
        else:
            return None, Value.illegal_operation(self, other)

    def divided_by(self, other):
        if isinstance(other, Number):
            try:
                return self.elements[other.value], None
            except:
                return None, RuntimeError(
                    other.position_start, other.position_end,
                    'Element at this index could not be retrieved from list because index is out of bounds',
                    self.context
                )
        else:
            return None, Value.illegal_operation(self, other)
    
    def copy(self):
        copy = List(self.elements)
        copy.set_position(self.position_start, self.position_end)
        copy.set_context(self.context)
        return copy

    def __repr__(self):
        return f'[{", ".join([str(x) for x in self.elements])}]'

class BaseFunction(Value):
    def __init__(self,name):
        super().__init__()
        self.name = name or "<anonymous>"
    
    def generate_new_context(self):
        new_context = Context(self.name, self.context, self.position_start)
        new_context.symbols_table = SymbolTable(new_context.parent.symbols_table)
        return new_context
    
    def check_args(self, arg_names, args):
        res = RuntimeResult()

        if len(args) > len(arg_names):
            return res.failure(RuntimeError(
                self.position_start, self.position_end,
                f"{len(args) - len(arg_names)} too many args passed into {self}",
                self.context
            ))
        
        if len(args) < len(arg_names):
            return res.failure(RuntimeError(
                self.position_start, self.position_end,
                f"{len(arg_names) - len(args)} too few args passed into {self}",
                self.context
            ))

        return res.success(None)

    def populate_args(self, arg_names, args, exec_ctx):
        for i in range(len(args)):
            arg_name = arg_names[i]
            arg_value = args[i]
            arg_value.set_context(exec_ctx)
            exec_ctx.symbols_table.set(arg_name, arg_value)

    def check_and_populate_args(self, arg_names, args, exec_ctx):
        res = RuntimeResult()
        res.register(self.check_args(arg_names, args))
        if res.error: return res
        self.populate_args(arg_names, args, exec_ctx)
        return res.success(None)

class Function(BaseFunction):
    def __init__(self, name, body_node, arg_names):
        super().__init__(name)
        self.name = name or "<anonymous>"
        self.body_node = body_node
        self.arg_names = arg_names

    def execute(self, args):
        res = RuntimeResult()
        interpreter = Interpreter()
        exec_ctx = self.generate_new_context()

        res.register(self.check_and_populate_args(self.arg_names, args, exec_ctx))
        if res.error: return res

        value = res.register(interpreter.visit(self.body_node, exec_ctx))
        if res.error: return res
        return res.success(value)
    
    def copy(self):
        copy = Function(self.name, self.body_node, self.arg_names)
        copy.set_context(self.context)
        copy.set_position(self.position_start, self.position_end)
        return copy

    def __repr__(self):
        return f"<function {self.name}>"

class BuiltInFunction(BaseFunction):
    def __init__(self, name):
        super().__init__(name)

    def execute(self, args):
        res = RuntimeResult()
        exec_ctx = self.generate_new_context()

        method_name = f'execute_{self.name}'
        method = getattr(self, method_name, self.no_visit_method)

        res.register(self.check_and_populate_args(method.arg_names, args, exec_ctx))
        if res.error: return res

        return_value = res.register(method(exec_ctx))
        if res.error: return res
        return res.success(return_value)
    
    def no_visit_method(self, node, context):
        raise Exception(f'No execute_{self.name} method defined')
    
    def copy(self):
        copy = BuiltInFunction(self.name)
        copy.set_context(self.context)
        copy.set_position(None, None)
        return copy

    def __repr__(self):
        return f"<built-in function {self.name}>"

    
    def execute_print(self, exec_ctx):
        print(str(exec_ctx.symbols_table.get('value')))
        return RuntimeResult().success(Number.null)
    execute_print.arg_names = ['value']

    def execute_input(self, exec_ctx):
        text = input()
        return RuntimeResult().success(String(text))
    execute_input.arg_names = []

    def execute_inputInt(self, exec_ctx):
        while True:
            text = input()
            try:
                number = int(text)
                break
            except ValueError:
                print(f"'{text}' must be an integer")
        return RuntimeResult().success(Number(number))
    execute_inputInt.arg_names = []

    def execute_clear(self, exec_ctx):
        os.system('cls' if os.name == 'nt' else 'cls') 
        return RuntimeResult().success(Number.null)
    execute_clear.arg_names = []

    
    def execute_append(self, exec_ctx):
        list_ = exec_ctx.symbols_table.get("list")
        value = exec_ctx.symbols_table.get("value")

        if not isinstance(list_, List):
            return RuntimeResult().failure(RuntimeError(
                self.position_start, self.position_end,
                "First argument must be list",
                exec_ctx
            ))

        list_.elements.append(value)
        return RuntimeResult().success(Number.null)
    execute_append.arg_names = ["list", "value"]

    def execute_pop(self, exec_ctx):
        list_ = exec_ctx.symbols_table.get("list")
        index = exec_ctx.symbols_table.get("index")

        if not isinstance(list_, List):
            return RuntimeResult().failure(RuntimeError(
                self.position_start, self.position_end,
                "First argument must be list",
                exec_ctx
            ))

        if not isinstance(index, Number):
            return RuntimeResult().failure(RuntimeError(
                self.position_start, self.position_end,
                "Second argument must be number",
                exec_ctx
            ))

        try:
            element = list_.elements.pop(index.value)
        except:
            return RuntimeResult().failure(RuntimeError(
                self.position_start, self.position_end,
                'Element at this index could not be removed from list because index is out of bounds',
                exec_ctx
            ))
        return RuntimeResult().success(element)
    execute_pop.arg_names = ["list", "index"]

    def execute_extend(self, exec_ctx):
        listA = exec_ctx.symbols_table.get("listA")
        listB = exec_ctx.symbols_table.get("listB")

        if not isinstance(listA, List):
            return RuntimeResult().failure(RuntimeError(
                self.position_start, self.position_end,
                "First argument must be list",
                exec_ctx
            ))

        if not isinstance(listB, List):
            return RuntimeResult().failure(RuntimeError(
                self.position_start, self.position_end,
                "Second argument must be list",
                exec_ctx
            ))

        listA.elements.extend(listB.elements)
        return RuntimeResult().success(Number.null)
    execute_extend.arg_names = ["listA", "listB"]

    def execute_len(self,exec_ctx):
        list=exec_ctx.symbols_table.get("list")

        if not isinstance(list, List):
            return RuntimeResult().failure(RuntimeError(
                self.position_start, self.position_end,
                "Argument must be list",
                exec_ctx
            ))
        
        return RuntimeResult().success(Number(len(list.elements)))
    execute_len.arg_names = ["list"]

    def execute_lenStr(self,exec_ctx):
        string=exec_ctx.symbols_table.get("string")

        if not isinstance(string, String):
            return RuntimeResult().failure(RuntimeError(
                self.position_start, self.position_end,
                "Argument must be string",
                exec_ctx
            ))
        
        return RuntimeResult().success(Number(len(string.value)))
    execute_lenStr.arg_names = ["string"]

    def execute_floor(self,exec_ctx):
        num=exec_ctx.symbols_table.get("num")

        if not isinstance(num, Number):
            return RuntimeResult().failure(RuntimeError(
                self.position_start, self.position_end,
                "Argument must be int or float",
                exec_ctx
            ))

        return RuntimeResult().success(Number(math.floor(num.value)))
    execute_floor.arg_names=["num"]

BuiltInFunction.print       = BuiltInFunction("print")
BuiltInFunction.input       = BuiltInFunction("input")
BuiltInFunction.inputInt    = BuiltInFunction("inputInt")
BuiltInFunction.clear       = BuiltInFunction("clear")
BuiltInFunction.append      = BuiltInFunction("append")
BuiltInFunction.pop         = BuiltInFunction("pop")
BuiltInFunction.extend      = BuiltInFunction("extend")
BuiltInFunction.len         = BuiltInFunction("len")
BuiltInFunction.lenStr      = BuiltInFunction("lenStr")
BuiltInFunction.floor      = BuiltInFunction("floor")

class Context:
    def __init__(self, display_name, parent=None, parent_entry_pos=None):
        self.display_name = display_name
        self.parent = parent
        self.parent_entry_pos = parent_entry_pos
        self.symbols_table=SymbolTable()

class SymbolTable:
    def __init__(self, parent=None):
        self.symbols={}
        self.parent=parent

    def get(self,name,context=None):
        value=self.symbols.get(name, None)
        if value==None and self.parent:
            return self.parent.get(name)
        return value

    def set(self,name,value):
        self.symbols[name]=value
    
    def remove(self,name):
        del self.symbols[name]
        
class Interpreter:
    def visit(self,node, context):
        method_name=f'visit_{type(node).__name__}'
        method=getattr(self, method_name,self.no_visit_method)

        # print(method_name)

        return method(node, context)

    def no_visit_method(self,node, context):
        raise Exception(f'No visit_{type(node).__name__} method')
    
    def visit_NumberNode(self,node, context): 
        return RuntimeResult().success(Number(node.token.value).set_context(context).set_position(node.position_start,node.position_end))
    
    def visit_StringNode(self, node, context):
        return RuntimeResult().success(
            String(node.token.value).set_context(context).set_position(node.position_start, node.position_end)
        )
    
    def visit_ListNode(self, node, context):
        res = RuntimeResult()
        elements = []

        for element_node in node.element_nodes:
            elements.append(res.register(self.visit(element_node, context)))
            if res.error: return res

        return res.success(
        List(elements).set_context(context).set_position(node.position_start, node.position_end)
        )

    def visit_BinOpNode(self,node, context):
        # print(f'v bon {node}')
        res=RuntimeResult()
        left = res.register(self.visit(node.left_node, context))
        if res.error: return res
        right = res.register(self.visit(node.right_node, context))
        if res.error: return res
        
        if node.operation_token.type==TT_PLUS:
            result,error=left.added_to(right)
        elif node.operation_token.type==TT_MINUS:
            result,error=left.subtracted_by(right)
        elif node.operation_token.type==TT_MUL:
            result,error=left.multiplied_by(right)
        elif node.operation_token.type==TT_DIV:
            result,error=left.divided_by(right)
        elif node.operation_token.type==TT_WDIV:
            result,error=left.w_divided_by(right)
        elif node.operation_token.type==TT_MOD:
            result,error=left.moduled_by(right)        
        elif node.operation_token.type == TT_EE:
            result, error = left.get_comparison_eq(right)
        elif node.operation_token.type == TT_NE:
            result, error = left.get_comparison_ne(right)
        elif node.operation_token.type == TT_LT:
            result, error = left.get_comparison_lt(right)
        elif node.operation_token.type == TT_GT:
            result, error = left.get_comparison_gt(right)
        elif node.operation_token.type == TT_LTE:
            result, error = left.get_comparison_lte(right)
        elif node.operation_token.type == TT_GTE:
            result, error = left.get_comparison_gte(right)
        elif node.operation_token.matches(TT_KEYWORD, 'and'):
            result, error = left.anded_by(right)
        elif node.operation_token.matches(TT_KEYWORD, 'or'):
            result, error = left.ored_by(right)
        
        if error:
            return res.failure(error)
        else:
            return res.success(result.set_position(node.position_start,node.position_end)) 

    def visit_UnaryOpNode(self,node, context):
        # print(f'v uon {node}')
        res=RuntimeResult()
        number=res.register(self.visit(node.node,context))
        if res.error :return res

        error=None

        if node.operation_token.type==TT_MINUS:
            number ,error=number.multiplied_by(Number(-1))
        elif node.operation_token.matches(TT_KEYWORD, 'not'):
            number, error = number.notted()
        
        if error:
            return res.failure(error)
        else:
            # print(number)
            return res.success(number.set_position( node.position_start,node.position_end))
    
    
    def visit_IfNode(self, node, context):
        res = RuntimeResult()

        for condition, expr in node.cases:
            condition_value = res.register(self.visit(condition, context))
            if res.error: return res

            if condition_value.is_true():
                expr_value = res.register(self.visit(expr, context))
                if res.error: return res
                return res.success(expr_value)

        if node.else_case:
            else_value = res.register(self.visit(node.else_case, context))
            if res.error: return res
            return res.success(else_value)

        return res.success(None)

    def visit_ForNode(self, node, context):
        res = RuntimeResult()
        elements = []

        start_value = res.register(self.visit(node.start_value_node, context))
        if res.error: return res

        end_value = res.register(self.visit(node.end_value_node, context))
        if res.error: return res

        if node.step_value_node:
            step_value = res.register(self.visit(node.step_value_node, context))
            if res.error: return res
        else:
            step_value = Number(1)

        i = start_value.value

        if step_value.value >= 0:
            condition = lambda: i < end_value.value
        else:
            condition = lambda: i > end_value.value
        
        while condition():
            context.symbols_table.set(node.var_name_token.value, Number(i))
            i += step_value.value

            elements.append(res.register(self.visit(node.body_node, context)))
            if res.error: return res

        return res.success(List(elements).set_context(context).set_position(node.position_start, node.position_end))

    def visit_WhileNode(self, node, context):
        res = RuntimeResult()

        while True:
            condition = res.register(self.visit(node.condition_node, context))
            if res.error: return res

            if not condition.is_true(): break

            res.register(self.visit(node.body_node, context))
            if res.error: return res

        return res.success(None)

            
    def visit_VarAccessNode(self, node, context):
        res = RuntimeResult()
        var_name = node.var_name_token.value
        value = context.symbols_table.get(var_name,context)

        if not value:
            return res.failure(RuntimeError(
                node.position_start, node.position_end,
                f"'{var_name}' is not defined",
                context
            ))

        value = value.copy().set_position(node.position_start, node.position_end).set_context(context)
        return res.success(value)

    def visit_VarAssignNode(self, node, context):
        res = RuntimeResult()
        var_name = node.var_name_token.value
        value = res.register(self.visit(node.value_node, context))
        if res.error: return res

        context.symbols_table.set(var_name, value)
        return res.success(value)
    
    def visit_FuncDefNode(self, node, context):
        res = RuntimeResult()

        func_name = node.var_name_token.value if node.var_name_token else None
        body_node = node.body_node
        arg_names = [arg_name.value for arg_name in node.arg_name_tokens]
        func_value = Function(func_name, body_node, arg_names).set_context(context).set_position(node.position_start, node.position_end)
        
        if node.var_name_token:
            context.symbols_table.set(func_name, func_value)

        return res.success(func_value)

    def visit_CallNode(self, node, context):
        res = RuntimeResult()
        args = []
        
        # print(node.node_to_call)
        # print(node.node_to_call.type)

        value_to_call = res.register(self.visit(node.node_to_call, context))
        # print (value_to_call)
        if res.error: return res
        value_to_call = value_to_call.copy().set_position(node.position_start, node.position_end)

        for arg_node in node.arg_nodes:
            args.append(res.register(self.visit(arg_node, context)))
            if res.error: return res

        return_value = res.register(value_to_call.execute(args))
        if res.error: return res
        return_value = return_value.copy().set_position(node.position_start, node.position_end).set_context(context)
        return res.success(return_value)
        
    def __repr__(self):
        return str(self.value)

global_symbols_table = SymbolTable()
global_symbols_table.set("null", Number.null)
global_symbols_table.set("false", Number.false)
global_symbols_table.set("true", Number.true)
global_symbols_table.set("pi", Number.math_PI)

global_symbols_table.set("print", BuiltInFunction.print)
global_symbols_table.set("input", BuiltInFunction.input)
global_symbols_table.set("inputInt", BuiltInFunction.inputInt)
global_symbols_table.set("clear", BuiltInFunction.clear)
global_symbols_table.set("cls", BuiltInFunction.clear)
global_symbols_table.set("append", BuiltInFunction.append)
global_symbols_table.set("pop", BuiltInFunction.pop)
global_symbols_table.set("extend", BuiltInFunction.extend)
global_symbols_table.set("len", BuiltInFunction.len)
global_symbols_table.set("lenStr", BuiltInFunction.lenStr)
global_symbols_table.set("floor", BuiltInFunction.floor)


def run(file_name,text):
    lexer=Lexer(file_name, text)
    tokens ,error=lexer.makeTokens()

    if error: return None,error

    # print(tokens)

    parser=Parser(tokens)
    ast=parser.parse()
    if ast.error : return None, ast.error

    # print(ast.node)

    interpreter=Interpreter()
    context=Context('<program>')
    context.symbols_table=global_symbols_table
    result=interpreter.visit(ast.node, context)

    return result.value, result.error