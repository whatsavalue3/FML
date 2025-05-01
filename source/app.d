import std.stdio;
import std.file;
import std.algorithm;
import std.string;
import std.ascii;
import std.conv;
import gatherer;
import generator;
import std.format;

enum KEYWORD
{
	SKIP,
	
	NAME,
	NUMBER,
	
	VOID,
	BOOL,
	U8,
	U16,
	S8,
	S16,
	UFIXED,
	SFIXED,
	FUNCTION,
	END,
	LOCAL,
	THEN,
	RETURN,
	ALIAS,
	IF,
	CONST,
	WHILE,
	DO,
	CAST,
	
	LEFTPAREN,
	RIGHTPAREN,
	STAR,
	EQUAL,
	SEMICOLON,
	PLUS,
	COMMA,
	MINUS,
	GREATER,
	DOUBLEEQUAL,
	LEFTBRACKET,
	RIGHTBRACKET,
	LEFTSHIFT,
	RIGHTSHIFTARITH,
	RIGHTSHIFTLOGI,
	BITAND,
	NOTEQUAL,
	TYLDE,
	LESS,
	XOR,
	AT,
}

enum StatementType
{
	ASSIGN,
	CALL,
	CONDITIONAL,
	RETURN
}

enum ValueType
{
	INT,
	UINT,
	FIXED,
	UFIXED,
	BOOL,
}

struct Token
{
	KEYWORD type;
	string name;
	uint line;
}

struct Type
{
	ubyte size;
	ubyte point;
	ubyte ptrdepth;
	bool signed;
	bool isconst = false;
}

struct Value
{
	Type type;
	ushort value;
}

class Variable
{
	string name;
	Type type;
	Number defaultval;
	bool hasloc = false;
	ushort location = 0;
	bool isarg = false;
	ubyte arg = 0;
	ushort elements = 0;
	ushort[] defaultelements;
}

class Scope
{
	Variable[string] variables;
	Statement[] statements;
}

enum ExpressionType
{
	VARIABLE_ACCESS,
	VALUE,
	OPERATION,
	CALL
}

class Statement
{
	StatementType type;
	bool loop;
	Number lvalue;
	Number rvalue;
	Scope inside;
}

class Function
{
	string name;
	Type returntype;
	Variable[] args;
	Scope inside;
}

class Module
{
	string name;
	Function[string] functions;
	Variable[string] variables;
}

Module[string] modules;

Token[] tokens;

KEYWORD[string] zerothStringToToken = 
[
	">>>":KEYWORD.RIGHTSHIFTLOGI
];

KEYWORD[string] firstStringToToken = 
[
	"==":KEYWORD.DOUBLEEQUAL,
	"<<":KEYWORD.LEFTSHIFT,
	">>":KEYWORD.RIGHTSHIFTARITH,
];

KEYWORD[string] stringToToken = [
	"void": KEYWORD.VOID,
	"bool":KEYWORD.BOOL,
	"u8":KEYWORD.U8,
	"u16":KEYWORD.U16,
	"s8":KEYWORD.S8,
	"s16":KEYWORD.S16,
	"sfixed":KEYWORD.SFIXED,
	"ufixed":KEYWORD.UFIXED,
	"function":KEYWORD.FUNCTION,
	"end":KEYWORD.END,
	"local":KEYWORD.LOCAL,
	"then":KEYWORD.THEN,
	"return":KEYWORD.RETURN,
	"alias":KEYWORD.ALIAS,
	"const":KEYWORD.CONST,
	"while":KEYWORD.WHILE,
	"do":KEYWORD.DO,
	"cast":KEYWORD.CAST,
	"if":KEYWORD.IF,
	" ":KEYWORD.SKIP,
	"\n":KEYWORD.SKIP,
	"\t":KEYWORD.SKIP,
	"\r":KEYWORD.SKIP,
	"(":KEYWORD.LEFTPAREN,
	")":KEYWORD.RIGHTPAREN,
	"*":KEYWORD.STAR,
	"=":KEYWORD.EQUAL,
	"!=":KEYWORD.NOTEQUAL,
	";":KEYWORD.SEMICOLON,
	"+":KEYWORD.PLUS,
	",":KEYWORD.COMMA,
	"-":KEYWORD.MINUS,
	">":KEYWORD.GREATER,
	"<":KEYWORD.LESS,
	"[":KEYWORD.LEFTBRACKET,
	"]":KEYWORD.RIGHTBRACKET,
	"&":KEYWORD.BITAND,
	"~":KEYWORD.TYLDE,
	"^":KEYWORD.XOR,
	"@":KEYWORD.AT,
];

uint linecount = 0;

string TryToken(string tok, string str, KEYWORD kw, ref bool found)
{
	if(startsWith(str,tok) && (tok.length == str.length || kw == KEYWORD.SKIP || !isAlphaNum(str[tok.length-1]) || !isAlphaNum(str[tok.length])))
	{
		if(tok == "\n")
		{
			linecount++;
		}
		if(kw != KEYWORD.SKIP)
		{
			tokens ~= Token(type:kw,name:"",line:linecount);
		}
		found = true;
	}
	return chompPrefix(str,tok);
}

void Tokenize(string code)
{
	bool found = false;
	while(!found)
	{
		foreach(string kwStr, KEYWORD kw; stringToToken)
		{
			foreach(string kwStr, KEYWORD kw; zerothStringToToken)
			{
				code = TryToken(kwStr,code,kw,found);
			}
			foreach(string kwStr, KEYWORD kw; firstStringToToken)
			{
				code = TryToken(kwStr,code,kw,found);
			}
			code = TryToken(kwStr,code,kw,found);
		}
		if(!found)
		{
			if(code == "")
			{
				break;
			}
			string name;
			bool numeric = isDigit(code[0]);
			foreach(dchar k; code)
			{
				if(!isAlphaNum(k))
				{
					break;
				}
				name ~= k;
			}
			if(name != "")
			{
				tokens ~= Token(type:numeric ? KEYWORD.NUMBER : KEYWORD.NAME,name:name,line:linecount);
				code = chompPrefix(code,name);
				continue;
			}
			break;
		}
		found = false;
	}
	writeln(tokens);
}

class TokenVomiter
{
	uint i = 0;
	Token next()
	{
		if(i >= tokens.length)
		{
			return Token(type:KEYWORD.SKIP,name:"",line:0);
		}
		writeln(tokens[i]);
		return tokens[i++];
	}
	
	Token peek()
	{
		//writeln(tokens[i]);
		if(i >= tokens.length)
		{
			return Token(type:KEYWORD.SKIP,name:"",line:0);
		}
		return tokens[i];
	}
	
	Token expect(KEYWORD type)
	{
		if(this.check(type))
		{
			return this.next();
		}
		throw new Exception("Expected " ~ to!string(type));
	}
	
	bool check(KEYWORD type)
	{
		return this.peek().type == type;
	}
	
	bool optional(KEYWORD type)
	{
		if(this.check(type))
		{
			this.next();
			return true;
		}
		return false;
	}
	
	bool isEOF()
	{
		return i >= tokens.length;
	}
}

struct Types
{
	Type u16 = {size:16, point:0, ptrdepth:0, signed:false};
	Type u8 = {size:8, point:0, ptrdepth:0, signed:false};
	Type s16 = {size:16, point:0, ptrdepth:0, signed:true};
	Type s8 = {size:8, point:0, ptrdepth:0, signed:true};
	Type boolean = {size:1, point:0, ptrdepth:0, signed:false};
	Type voidtype = {size:0, point:0, ptrdepth:0, signed:false};
};
Types types;



enum BinaryOperationType
{
	ADD,
	SUB,
	MUL,
	DIV,
	MOD,
	AND,
	OR,
	XOR,
	EQUAL,
	NOTEQUAL,
	LESS,
	GREATER,
	LESSEQUAL,
	GREATEREQUAL,
	LEFTSHIFT,
	RIGHTSHIFTLOGI,
	RIGHTSHIFTARITH
}

class BinaryOperation
{
	BinaryOperationType type;
	Number a;
	Number b;
}

enum UnaryOperationType
{
	NEGATE,
	NOT,
	DEREF,
	REF,
	CAST
}

class UnaryOperation
{
	UnaryOperationType type;
	Number val;
	Type castto;
}

enum NumberType
{
	IMMEDIATE,
	BINARY_OPERATION,
	UNARY_OPERATION,
	VARIABLE,
	CALL,
	ARRAYACCESS
}

class Number
{
	NumberType type;
	bool chill; // is computable in compile-time
	union
	{
		ushort imm;
		BinaryOperation binary_op;
		UnaryOperation unary_op;
		struct
		{
			string var;
			Number[] args;
		}
	}
}

void GetNumberUnary(TokenVomiter tv, ref Number num)
{
	if(tv.check(KEYWORD.MINUS) || tv.check(KEYWORD.STAR) || tv.check(KEYWORD.CAST) || tv.check(KEYWORD.TYLDE))
	{
		Token operator = tv.next();
		num = new Number();
		num.type = NumberType.UNARY_OPERATION;
		num.unary_op = new UnaryOperation();
		if(operator.type == KEYWORD.CAST)
		{
			tv.expect(KEYWORD.LEFTPAREN);
			num.unary_op.castto = GetType(tv);
			tv.expect(KEYWORD.RIGHTPAREN);
		}
		Number newnum;
		GetNumberUnary(tv,newnum);
		switch(operator.type)
		{
			case KEYWORD.MINUS:
				num.unary_op.type = UnaryOperationType.NEGATE;
				break;
			case KEYWORD.TYLDE:
				num.unary_op.type = UnaryOperationType.NOT;
				break;
			case KEYWORD.STAR:
				num.unary_op.type = UnaryOperationType.DEREF;
				break;
			case KEYWORD.CAST:
				num.unary_op.type = UnaryOperationType.CAST;
				break;
			default:
				throw new Exception("Unknown unary operation");
				return;
		}
		num.unary_op.val = newnum;
		return;
	}
	GetNumberImmediate(tv,num);
}

void GetNumberBinary(TokenVomiter tv, ref Number num)
{
	GetNumberUnary(tv,num);
	
	while(tv.check(KEYWORD.PLUS) || tv.check(KEYWORD.MINUS) || tv.check(KEYWORD.GREATER) || tv.check(KEYWORD.DOUBLEEQUAL) || tv.check(KEYWORD.LEFTSHIFT)
	 || tv.check(KEYWORD.BITAND)
	 || tv.check(KEYWORD.NOTEQUAL)
	 || tv.check(KEYWORD.RIGHTSHIFTLOGI)
	 || tv.check(KEYWORD.LESS)
	 || tv.check(KEYWORD.XOR)
	 || tv.check(KEYWORD.STAR)
	 )
	{
		Token operator = tv.next();
		Number newnum = new Number();
		newnum.type = NumberType.BINARY_OPERATION;
		newnum.binary_op = new BinaryOperation();
		switch(operator.type)
		{
			case KEYWORD.PLUS:
				newnum.binary_op.type = BinaryOperationType.ADD;
				break;
			case KEYWORD.MINUS:
				newnum.binary_op.type = BinaryOperationType.SUB;
				break;
			case KEYWORD.GREATER:
				newnum.binary_op.type = BinaryOperationType.GREATER;
				break;
			case KEYWORD.LESS:
				newnum.binary_op.type = BinaryOperationType.LESS;
				break;
			case KEYWORD.XOR:
				newnum.binary_op.type = BinaryOperationType.XOR;
				break;
			case KEYWORD.DOUBLEEQUAL:
				newnum.binary_op.type = BinaryOperationType.EQUAL;
				break;
			case KEYWORD.LEFTSHIFT:
				newnum.binary_op.type = BinaryOperationType.LEFTSHIFT;
				break;
			case KEYWORD.STAR:
				newnum.binary_op.type = BinaryOperationType.MUL;
				break;
			case KEYWORD.BITAND:
				newnum.binary_op.type = BinaryOperationType.AND;
				break;
			case KEYWORD.NOTEQUAL:
				newnum.binary_op.type = BinaryOperationType.NOTEQUAL;
				break;
			case KEYWORD.RIGHTSHIFTLOGI:
				newnum.binary_op.type = BinaryOperationType.RIGHTSHIFTLOGI;
				break;
			default:
				throw new Exception("Unknown operator");
				return;
		}
		newnum.binary_op.a = num;
		newnum.binary_op.b = GetNumber(tv);
		num = newnum;
	}
}

ushort ParseNumber(string str)
{
	if(startsWith(str,"0x"))
	{
		ushort o;
		str.formattedRead("0x%x",o);
		return o;
	}
	return to!ushort(str);
}

void GetNumberImmediate(TokenVomiter tv, ref Number num)
{
	if(tv.check(KEYWORD.NUMBER))
	{
		num = new Number();
		num.imm = ParseNumber(tv.next().name);
		num.type = NumberType.IMMEDIATE;
		return;
	}
	if(tv.check(KEYWORD.NAME))
	{
		num = new Number();
		num.var = tv.expect(KEYWORD.NAME).name;
		num.type = NumberType.VARIABLE;
		if(tv.optional(KEYWORD.LEFTPAREN))
		{
			num.type = NumberType.CALL;
			while(tv.peek().type != KEYWORD.RIGHTPAREN)
			{
				num.args ~= GetNumber(tv);
				Token tok = tv.next();
				if(tok.type == KEYWORD.RIGHTPAREN)
				{
					break;
				}
				else if(tok.type != KEYWORD.COMMA)
				{
					throw new Exception("Expected comma");
				}
			}
			if(tv.peek().type == KEYWORD.RIGHTPAREN)
			{
				tv.next();
			}
		}
		if(tv.optional(KEYWORD.LEFTBRACKET))
		{
			num.type = NumberType.ARRAYACCESS;
			num.args ~= GetNumber(tv);
			tv.expect(KEYWORD.RIGHTBRACKET);
		}
		return;
	}
	GetNumberParens(tv,num);
}

void GetNumberParens(TokenVomiter tv, ref Number num)
{
	tv.expect(KEYWORD.LEFTPAREN);
	num = GetNumber(tv);
	tv.expect(KEYWORD.RIGHTPAREN);
}

Number GetNumber(TokenVomiter tv)
{
	Number num;
	GetNumberBinary(tv,num);
	return num;
}

ushort CalculateNumber(Number num)
{
	switch(num.type)
	{
		case NumberType.IMMEDIATE:
			return num.imm;
		case NumberType.UNARY_OPERATION:
			return cast(ushort)(-CalculateNumber(num.unary_op.val));
		case NumberType.BINARY_OPERATION:
			switch(num.binary_op.type)
			{
				case BinaryOperationType.ADD:
					return cast(ushort)(CalculateNumber(num.binary_op.a)+CalculateNumber(num.binary_op.b));
				case BinaryOperationType.SUB:
					return cast(ushort)(CalculateNumber(num.binary_op.a)-CalculateNumber(num.binary_op.b));
				default:
					throw new Exception("This should not happen!");
					return 0;
			}
			break;
		case NumberType.VARIABLE:
			
		default:
			throw new Exception("invalid number");
	}
}

Type[string] aliases;

Type GetType(TokenVomiter tv)
{
	Type ret;
	bool isconst = tv.optional(KEYWORD.CONST);
	Token tok = tv.next();
	switch(tok.type)
	{
		case KEYWORD.U16:
			ret = types.u16;break;
		case KEYWORD.U8:
			ret = types.u8;break;
		case KEYWORD.S16:
			ret = types.s16;break;
		case KEYWORD.S8:
			ret = types.s8;break;
		case KEYWORD.UFIXED:
		case KEYWORD.SFIXED:
			tv.expect(KEYWORD.LEFTPAREN);
			Number size = GetNumber(tv);
			tv.expect(KEYWORD.COMMA);
			Number point = GetNumber(tv);
			//ushort point = GetNumber(tv);
			tv.expect(KEYWORD.RIGHTPAREN);
			ret = Type(size:cast(ubyte)CalculateNumber(size),point:cast(ubyte)CalculateNumber(point),ptrdepth:0,signed:tok.type == KEYWORD.SFIXED);break;
		case KEYWORD.BOOL:
			ret = types.boolean;break;
		case KEYWORD.VOID:
			ret = types.voidtype;break;
		case KEYWORD.NAME:
			ret = aliases[tok.name];
			break;
		default:
			throw new Exception("Expected type");
	}
	if(isconst)
	{
		ret.isconst = true;
	}
	while(tv.optional(KEYWORD.STAR))
	{
		ret.ptrdepth++;
	}
	return ret;
}

Statement ParseIf(TokenVomiter tv, Scope outer)
{
	Statement stat = new Statement();
	tv.expect(KEYWORD.IF);
	stat.type = StatementType.CONDITIONAL;
	stat.rvalue = GetNumber(tv);
	stat.loop = false;
	tv.expect(KEYWORD.THEN);
	stat.inside = GetScope(tv);
	foreach(name, var; outer.variables)
	{
		stat.inside.variables[name] = var;
	}
	return stat;
}

Statement ParseWhile(TokenVomiter tv, Scope outer)
{
	Statement stat = new Statement();
	tv.expect(KEYWORD.WHILE);
	stat.type = StatementType.CONDITIONAL;
	stat.rvalue = GetNumber(tv);
	stat.loop = true;
	tv.expect(KEYWORD.DO);
	stat.inside = GetScope(tv);
	foreach(name, var; outer.variables)
	{
		stat.inside.variables[name] = var;
	}
	return stat;
}

Statement ParseReturn(TokenVomiter tv)
{
	Statement stat = new Statement();
	writeln("ParseReturn ",tv.peek());
	tv.expect(KEYWORD.RETURN);
	stat.type = StatementType.RETURN;
	stat.rvalue = GetNumber(tv);
	tv.expect(KEYWORD.SEMICOLON);
	return stat;
}

Statement ParseExpressionStatement(TokenVomiter tv)
{
	Statement stat = new Statement();
	stat.type = StatementType.ASSIGN;
	stat.lvalue = GetNumber(tv);
	if(!tv.optional(KEYWORD.EQUAL))
	{
		tv.expect(KEYWORD.SEMICOLON);
		stat.type = StatementType.CALL;
		return stat;
	}
	stat.rvalue = GetNumber(tv);
	tv.expect(KEYWORD.SEMICOLON);
	return stat;
}

Scope GetScope(TokenVomiter tv)
{
	Scope inside = new Scope();
	Token tok = tv.peek();
	writeln("BR ",tv.peek());
	while(tok.type != KEYWORD.END)
	{
		switch(tok.type)
		{
			case KEYWORD.IF:
				inside.statements ~= ParseIf(tv, inside);
				break;
			case KEYWORD.WHILE:
				inside.statements ~= ParseWhile(tv, inside);
				break;
			case KEYWORD.LOCAL:
				Variable var = ParseLocal(tv);
				inside.variables[var.name] = var;
				break;
			case KEYWORD.RETURN:
				inside.statements ~= ParseReturn(tv);
				break;
			default:
				inside.statements ~= ParseExpressionStatement(tv);
				break;
		}
		tok = tv.peek();
	}
	tv.expect(KEYWORD.END);
	return inside;
}

Function ParseFunction(TokenVomiter tv)
{
	Function func = new Function();
	func.returntype = GetType(tv);
	func.name = tv.expect(KEYWORD.NAME).name;
	tv.expect(KEYWORD.LEFTPAREN);
	while(tv.peek().type != KEYWORD.RIGHTPAREN)
	{
		Variable var = new Variable();
		var.type = GetType(tv);
		var.name = tv.expect(KEYWORD.NAME).name;
		func.args ~= var;
		if(tv.peek().type != KEYWORD.RIGHTPAREN)
		{
			tv.expect(KEYWORD.COMMA);
		}
	}
	tv.expect(KEYWORD.RIGHTPAREN);
	func.inside = GetScope(tv);
	foreach(i, var; func.args)
	{
		var.isarg = true;
		var.arg = cast(ubyte)i;
		func.inside.variables[var.name] = var;
	}
	return func;
}

Variable ParseLocal(TokenVomiter tv)
{
	Variable var = new Variable();
	var.type = GetType(tv);
	var.name = tv.expect(KEYWORD.NAME).name;
	if(tv.optional(KEYWORD.LEFTBRACKET))
	{
		var.type.ptrdepth++;
		var.elements = CalculateNumber(GetNumber(tv));
		tv.expect(KEYWORD.RIGHTBRACKET);
		if(tv.optional(KEYWORD.SEMICOLON))
		{
			return var;
		}
		if(tv.optional(KEYWORD.AT))
		{
			var.hasloc = true;
			var.location = CalculateNumber(GetNumber(tv));
			tv.expect(KEYWORD.SEMICOLON);
			return var;
		}
		tv.expect(KEYWORD.EQUAL);
		tv.expect(KEYWORD.LEFTBRACKET);
		while(!tv.optional(KEYWORD.RIGHTBRACKET))
		{
			var.defaultelements ~= CalculateNumber(GetNumber(tv));
			if(tv.optional(KEYWORD.COMMA))
			{
				continue;
			}
			tv.expect(KEYWORD.RIGHTBRACKET);
			break;
		}
		
		
		tv.expect(KEYWORD.SEMICOLON);
		return var;
	}
	else if(tv.optional(KEYWORD.SEMICOLON))
	{
		return var;
	}
	
	if(tv.optional(KEYWORD.AT))
	{
		var.hasloc = true;
		var.location = CalculateNumber(GetNumber(tv));
		tv.expect(KEYWORD.SEMICOLON);
		return var;
	}
	
	tv.expect(KEYWORD.EQUAL);
	var.defaultval = GetNumber(tv);
	tv.expect(KEYWORD.SEMICOLON);
	return var;
}



void ParseAlias(TokenVomiter tv)
{
	string name = tv.expect(KEYWORD.NAME).name;
	tv.expect(KEYWORD.EQUAL);
	Type type = GetType(tv);
	aliases[name] = type;
	writeln(name," = ", type);
	tv.expect(KEYWORD.SEMICOLON);
}

void Parse(TokenVomiter tv, string name)
{
	Token tok = tv.next();
	Module mod = new Module();
	mod.name = name;
	while(tok.type != KEYWORD.SKIP)
	{
		switch(tok.type)
		{
			case KEYWORD.FUNCTION:
				Function func = ParseFunction(tv);
				mod.functions[func.name] = func;
				break;
			case KEYWORD.LOCAL:
				Variable var = ParseLocal(tv);
				writeln(__LINE__, ": ", var.type);
				mod.variables[var.name] = var;
				break;
			case KEYWORD.ALIAS:
				ParseAlias(tv);
				break;
			default:
				throw new Exception("you done goofed");
				break;
		}
		tok = tv.next();
	}
	modules[name] = mod;
}
static this()
{

}
void main(string[] args)
{
	if(args.length == 1)
	{
		return;
	}
	
	string code = cast(string)read(args[1]);
	Tokenize(code);
	
	auto tv = new TokenVomiter();
	Parse(tv,args[1]);
	foreach(mod; modules)
	{
		Gather(mod);
	}
	Generate();
}

