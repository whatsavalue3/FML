import app;
import std.stdio;
import std.conv;
import std.algorithm;
import generator;

class Restriction
{
	bool[16] used_registers;
	ubyte result_register;
	
	Node[8] register_contain;
	void SetResult(Node n)
	{
		this.register_contain[this.result_register>>1] = n;
	}
	
	bool ResultContains(Node n)
	{
		return this.register_contain[this.result_register>>1] == n;
	}
}

class Node
{
	Node[] inputs;
	Node[] outputs;
	bool printed = false;
	
	this()
	{
		//nodes ~= this;
	}
	
	void AddInput(Node other)
	{
		this.inputs ~= other;
		other.outputs ~= this;
	}
	
	bool Unused()
	{
		return this.outputs.length == 0;
	}
	
	override string toString()
	{
		if(this.printed)
		{
			return "@";
		}
		this.printed = true;
		string ret = "inputs:"~to!string(inputs);
		return ret;
	}
	
	void Generate()
	{
	
	}
	
	void Generate(Restriction res)
	{
		throw new Exception("tried to generate a restricted Node");
	}
	
	Node Optimize()
	{
		foreach(i, input; inputs)
		{
			writeln(input);
			Node[] outputs = input.outputs;
			Node newnode = input.Optimize();
			foreach(o; outputs)
			{
				o.inputs[countUntil(o.inputs,input)] = newnode;
				newnode.outputs ~= o;
			}
			inputs[i] = newnode;
		}
		return this;
	}
	
	Type GetType()
	{
		throw new Exception("fuck yuo");
	}
}

class FunctionNode : Node
{
	static bool funcprint = false;
	string name;
	this(string name)
	{
		super();
		this.name = name;
		nodes ~= this;
	}
	override string toString()
	{
		if(this.printed || this.funcprint)
		{
			return this.name;
		}
		this.printed = true;
		this.funcprint = true;
		string ret = this.name ~ "() inputs:"~to!string(inputs);
		this.funcprint = false;
		return ret;
	}
	
	override void Generate(Restriction res)
	{
		writeln("Begin Function ", this.name);
		foreach(input; inputs)
		{
			input.Generate(res);
		}
		writeln("End Function ", this.name);
	}
}

class ConstantNode : Node
{
	ushort val;
	private Type type;
	this(ushort val)
	{
		super();
		this.val = val;
		this.type = types.u16;
	}
	
	this(ushort val, Type type)
	{
		super();
		this.val = val;
		this.type = type;
	}
	
	override string toString()
	{
		return to!string(this.val);
	}
	
	override void Generate()
	{
		writeln("Constant ", val);
	}
	
	override void Generate(Restriction res)
	{
		if(this.val < 0x40)
		{
			Emit(cast(ushort)(0xE000 | (res.result_register<<8) | this.val));
		}
		else if(this.val >= 0xffc0)
		{
			Emit(cast(ushort)(0xE000 | (res.result_register<<8) | (this.val&0x7f)));
		}
		else
		{
			Emit(cast(ushort)((res.result_register<<8)|(this.val&0xff)));
			Emit(cast(ushort)(((res.result_register+1)<<8)|((this.val>>>8)&0xff)));
		}
	}
}

ConstantNode[] constantnodes;

ConstantNode FindConstant(ushort val, Type type)
{
	foreach(cn; constantnodes)
	{
		if(cn.val == val && cn.type == type)
		{
			return cn;
		}
	}
	return new ConstantNode(val,type);
}

class ScopeNode : Node
{
	ulong index;

	this()
	{
		index = scopestack.length;
		scopestack ~= this;
	}
	
	void pop()
	{
		scopestack.length = index;
	}
	
	override void Generate(Restriction res)
	{
		writeln("Begin Scope ", index);
		foreach(input; inputs)
		{
			input.Generate(res);
		}
		writeln("End Scope ", index);
	}
}

Type[VariableNode] globalvariables; // fuck this shit
ArrayVariableNode[string] globalarrays;

class VariableNode : Node
{
	string name;
	Type type;
	
	this(string name, Node value, Type type)
	{
		this.name = name;
		this.type = type;
		scopestack[$-1].AddInput(this);
		this.AddInput(value);
	}
	
	override string toString()
	{
		return this.name;
	}
	
	override void Generate()
	{
		writeln("Variable " ~ name);
	}
	
	override Node Optimize()
	{
		super.Optimize();
		if(cast(ConstantNode)this.inputs[0] && this.type.isconst)
		{
			globalvariables.remove(this);
			return FindConstant((cast(ConstantNode)this.inputs[0]).val,this.type);
		}
		else
		{
			globalvariables[this] = this.type;
		}
		return this;
	}
}

class ArrayVariableNode : Node
{
	Variable var;
	this(Variable var)
	{
		this.var = var;
		globalarrays[this.var.name] = this;
	}
	
	override Node Optimize()
	{
		return this;
	}
}

class CallNode : Node
{
	this(FunctionNode func)
	{
		this.AddInput(func);
	}
	
	override string toString()
	{
		return "Call " ~ super.toString();
	}
	
	override void Generate(Restriction res)
	{
		writeln("Begin Call ", (cast(FunctionNode)this.inputs[0]).name);
		ubyte result = res.result_register;
		foreach(input; inputs[1..$])
		{
			input.Generate(res);
			res.result_register += 2;
		}
		
		Emit(cast(ushort)(0xF001));
		LinkFunction(cast(FunctionNode)this.inputs[0]);
		if(result != 0)
		{
			Emit(0xF005 | (result<<8));
		}
		writeln("End Call ", (cast(FunctionNode)this.inputs[0]).name);
	}
}

class ConstantPointerNode : ConstantNode
{
	this(ushort val)
	{
		super(val);
	}

	override void Generate()
	{
		writeln("ConstantPointerNode ", this.val);
	}
}

class DerefImmNode : Node
{
	this(ConstantNode a)
	{
		super();
		this.AddInput(a);
	}

	override void Generate(Restriction res)
	{
		if(res.ResultContains(this.inputs[0]))
		{
			return;
		}
		ConstantNode cn = cast(ConstantNode)(this.inputs[0]);
		if(cn.type.size == 16)
		{
			Emit(cast(ushort)(0x9012 | ((cast(ushort)res.result_register)<<8)));
		}
		else if(cn.type.size == 8)
		{
			Emit(cast(ushort)(0x9010 | ((cast(ushort)res.result_register)<<8)));
		}
		else
		{
			throw new Exception("bruh type.size = " ~ to!string(cn.type.size));
		}
		Emit(cn.val);
		res.SetResult(this.inputs[0]);
	}
}

class ReferenceNode : Node
{
	this(VariableNode var)
	{
		this.AddInput(var);
	}
	
	override string toString()
	{
		return "Ref " ~ super.toString();
	}
	
	override void Generate()
	{
		writeln("Ref ", (cast(VariableNode)inputs[0]).name);
	}
	
	override void Generate(Restriction res)
	{
		if(res.ResultContains(this.inputs[0]))
		{
			return;
		}
		Emit(cast(ushort)(0x9012 | (res.result_register<<8)));
		LinkVariable(cast(VariableNode)this.inputs[0]);
		res.SetResult(this.inputs[0]);
	}
	
	override Node Optimize()
	{
		super.Optimize();
		if(cast(ConstantNode)this.inputs[0])
		{
			//return FindConstant((cast(ConstantNode)this.inputs[0]).val, (cast(ConstantNode)this.inputs[0]).type);
			return this.inputs[0];
		}
		return this;
	}
}

class DerefVariableNode : Node
{
	this(VariableNode var)
	{
		this.AddInput(var);
	}
	
	override string toString()
	{
		return "DerefVar " ~ super.toString();
	}
	
	override void Generate()
	{
		writeln("DerefVar ", (cast(VariableNode)inputs[0]).name);
	}
}


class UnaryNode : Node
{
	UnaryOperationType type;
	Type castto;
	
	this(Node node, UnaryOperationType type, Type castto)
	{
		this.AddInput(node);
		this.type = type;
		this.castto = castto;
	}
	
	override string toString()
	{
		return "Unary " ~ super.toString();
	}
	
	override void Generate()
	{
		writeln("Begin Unary ", type);
		foreach(input; inputs)
		{
			input.Generate();
		}
		writeln("End Unary ", type);
	}
	
	override void Generate(Restriction res)
	{
		inputs[0].Generate(res);
		if(this.type == UnaryOperationType.DEREF)
		{
			Emit(cast(ushort)(0x9002 | (res.result_register << 4) | (res.result_register << 8)));
		}
		else if(this.type == UnaryOperationType.CAST)
		{
			//Emit(cast(ushort)(0x9002 | (res.result_register << 4) | (res.result_register << 8)));
		}
		else
		{
			throw new Exception("idk what to do with " ~ to!string(this.type));
		}
	}
	
	override Node Optimize()
	{
		super.Optimize();
		if(this.type == UnaryOperationType.DEREF && cast(ReferenceNode)(this.inputs[0]) !is null)
		{
			return new DerefVariableNode(cast(VariableNode)(cast(ReferenceNode)this.inputs[0]).inputs[0]);
		}
		if(this.type == UnaryOperationType.DEREF && cast(ConstantNode)(this.inputs[0]) !is null)
		{
			return new DerefImmNode(cast(ConstantNode)this.inputs[0]);
		}
		return this;
	}
	
	override Type GetType()
	{
		if(this.type == UnaryOperationType.CAST)
		{
			return this.castto;
		}
		Type innertype = inputs[0].GetType();
		if(this.type == UnaryOperationType.DEREF)
		{
			if(innertype.ptrdepth != 0)
			{
				innertype.ptrdepth--;
			}
		}
		return innertype;
	}
}

class AddImmNode : Node
{
	ubyte val;
	this(Node a, ubyte val)
	{
		this.AddInput(a);
		this.val = val;
	}
	
	override string toString()
	{
		return "AddImmNode " ~ super.toString();
	}
	
	override void Generate(Restriction res)
	{
		inputs[0].Generate(res);
		Emit(cast(ushort)(0xE080 | this.val | (res.result_register<<8)));
		res.SetResult(this);
	}
}

class LshImmNode : Node
{
	ubyte val;
	this(Node a, ubyte val)
	{
		this.AddInput(a);
		this.val = val;
	}
	
	override string toString()
	{
		return "LshImmNode " ~ super.toString();
	}
	
	override void Generate(Restriction res)
	{
		inputs[0].Generate(res);
		Emit(0x900B | cast(ushort)((res.result_register+1)<<8) | cast(ushort)(this.val<<4));
		Emit(0x900A | cast(ushort)(res.result_register<<8) | cast(ushort)(this.val<<4));
		res.SetResult(this);
	}
}

class BinaryNode : Node
{
	BinaryOperationType type;
	
	this(Node a, Node b, BinaryOperationType type)
	{
		this.AddInput(a);
		this.AddInput(b);
		this.type = type;
	}
	
	override string toString()
	{
		return "Binary " ~ super.toString();
	}
	
	override void Generate(Restriction res)
	{
		writeln("Begin Binary ", this.type);
		inputs[0].Generate(res);
		res.result_register += 2;
		inputs[1].Generate(res);
		res.result_register -= 2;
		if(this.type == BinaryOperationType.ADD)
		{
			Emit(0xF006 | cast(ushort)(res.result_register<<8) | cast(ushort)((res.result_register + 2)<<4));
		}
		else if(this.type == BinaryOperationType.EQUAL)
		{
			Emit(0xF007 | cast(ushort)(res.result_register<<8) | cast(ushort)((res.result_register + 2)<<4));
		}
		else if(this.type == BinaryOperationType.GREATER)
		{
			Emit(0xF007 | cast(ushort)(res.result_register<<8) | cast(ushort)((res.result_register + 2)<<4));
		}
		else
		{
			throw new Exception("idk how to emit " ~ to!string(this.type));
		}
		res.SetResult(this);
		writeln("End Binary ", this.type);
	}
	
	override Node Optimize()
	{
		super.Optimize();
		if((cast(ConstantNode)this.inputs[0]) && (cast(ConstantNode)this.inputs[1]))
		{
			ushort a = (cast(ConstantNode)this.inputs[0]).val;
			ushort b = (cast(ConstantNode)this.inputs[1]).val;
			if(this.type == BinaryOperationType.ADD)
			{
				return FindConstant(cast(ushort)(a+b), types.u16);
			}
		}
		else if((cast(ConstantNode)this.inputs[0]))
		{
			ushort a = (cast(ConstantNode)this.inputs[0]).val;
			if(this.type == BinaryOperationType.ADD && (a < 0x40 || a > 0xffc0))
			{
				return new AddImmNode(this.inputs[1],cast(ubyte)a&0x7f);
			}
		}
		else if((cast(ConstantNode)this.inputs[1]))
		{
			ushort a = (cast(ConstantNode)this.inputs[1]).val;
			if(this.type == BinaryOperationType.ADD && (a < 0x40 || a > 0xffc0))
			{
				return new AddImmNode(this.inputs[0],cast(ubyte)a&0x7f);
			}
			if(this.type == BinaryOperationType.LEFTSHIFT && (a < 0x10))
			{
				return new LshImmNode(this.inputs[0],cast(ubyte)a&0xf);
			}
		}
		return this;
	}
}

class ReturnNode : Node
{
	this(Node a)
	{
		this.AddInput(a);
	}
	
	override string toString()
	{
		return "Return " ~ super.toString();
	}
	
	override void Generate(Restriction res)
	{
		//Restriction res = new Restriction();
		res.result_register = 0;
		writeln("Begin Return");
		foreach(input; inputs)
		{
			input.Generate(res);
		}
		Emit(0xFE1F);
		writeln("End Return");
	}
}

ulong templabel = 0;

class ConditionNode : Node
{
	bool loop;
	bool alwayspass;
	bool alwaysfail;
	ushort conditiontype = 0xC800;
	this(Node a, Node b, bool loop)
	{
		this.AddInput(a);
		this.AddInput(b);
		this.loop = loop;
	}
	
	override string toString()
	{
		return "Condition " ~ super.toString();
	}
	
	override void Generate(Restriction res)
	{
		//Restriction res = new Restriction();
		res.result_register = 0;
		writeln("Begin Condition");
		ulong beginif = templabel;
		++templabel;
		ulong endif = templabel;
		++templabel;
		Label(beginif);
		if(!(alwayspass || alwaysfail))
		{
			inputs[0].Generate(res);
			LinkLabelOffset(this.conditiontype,endif);
		}
		if(!alwaysfail)
		{
			inputs[1].Generate(res);
			if(loop)
			{
				LinkLabelOffset(0xCE00,beginif);
			}
		}
		Label(endif);
		writeln("End Condition");
	}
	
	override Node Optimize()
	{
		super.Optimize();
		if((cast(ConstantNode)this.inputs[0]))
		{
			ushort a = (cast(ConstantNode)this.inputs[0]).val;
			alwayspass = a != 0;
			alwaysfail = a == 0;
		}
		else if((cast(BinaryNode)this.inputs[0]))
		{
			BinaryNode bn = (cast(BinaryNode)this.inputs[0]);
			if(bn.type == BinaryOperationType.GREATER)
			{
				this.conditiontype = 0xC100;
			}
		}
		return this;
	}
}

class AssignNode : Node
{
	this(Node a, Node b)
	{
		this.AddInput(a);
		this.AddInput(b);
	}
	
	override string toString()
	{
		return "Assign " ~ super.toString();
	}
	
	override void Generate(Restriction res)
	{
		writeln("Begin Assign");
		//Restriction res = new Restriction();
		res.result_register = 0;
		//inputs[0].Generate();
		if(cast(DerefImmNode)inputs[0])
		{
			inputs[1].Generate(res);
			Emit(cast(ushort)(0x9013));
			Emit((cast(ConstantNode)inputs[0].inputs[0]).val);
		}
		else if(cast(UnaryNode)inputs[0])
		{
			if((cast(UnaryNode)inputs[0]).type == UnaryOperationType.DEREF)
			{
				inputs[0].inputs[0].Generate(res);
				res.result_register += 2;
				inputs[1].Generate(res);
				
				if(inputs[0].GetType().size == 16)
				{
					Emit(cast(ushort)(0x9203));
				}
				else if(inputs[0].GetType().size == 8)
				{
					Emit(cast(ushort)(0x9201));
				}
				else
				{
					throw new Exception("i HAVE to shoot myself... ");
				}
			}
			else
			{
				throw new Exception("i HAVE to kill myself... ");
			}
		}
		else if(cast(ReferenceNode)inputs[0])
		{
			inputs[1].Generate(res);
			Emit(cast(ushort)(0x9013));
			LinkVariable(cast(VariableNode)(inputs[0].inputs[0]));
		}
		else
		{
			throw new Exception("idfk how to emit this im paid too little " ~ to!string(inputs[0]));
		}
		writeln("End Assign");
	}
}

class ArrayAccessNode : Node
{
	this(Node a, Node b)
	{
		this.AddInput(a);
		this.AddInput(b);
	}
	
	override void Generate(Restriction res)
	{
		ArrayVariableNode avn = cast(ArrayVariableNode)inputs[0];
		inputs[1].Generate(res);
		if(avn.var.type.size == 8)
		{
			Emit(cast(ushort)(0x9008 | (res.result_register << 8) | (res.result_register << 4)));
		}
		else
		{
			throw new Exception("avn.var.type.size = " ~ to!string(avn.var.type.size));
		}
		LinkArrayVariable(avn);
		res.SetResult(this);
	}
}

Node[] nodes;

ScopeNode[] scopestack;

void GatherVariableFromName(Scope s, string name)
{
	
}

FunctionNode FindFunction(string name)
{
	foreach(node; nodes)
	{
		if(cast(FunctionNode)(node) !is null)
		{
			if((cast(FunctionNode)(node)).name == name)
			{
				return cast(FunctionNode)node;
			}
		}
	}
	return new FunctionNode(name);
}

VariableNode FindVariable(string name, Node defaultval, Type type)
{
	foreach(scop; scopestack)
	{
		foreach(node; scop.inputs)
		{
			if(cast(VariableNode)(node) !is null)
			{
				if((cast(VariableNode)(node)).name == name)
				{
					return cast(VariableNode)node;
				}
			}
		}
	}
	
	return new VariableNode(name, defaultval, type);
}

ArrayVariableNode FindArrayVariable(string name, Variable var)
{
	foreach(n, a; globalarrays)
	{
		if(n == name)
		{
			return a;
		}
	}
	return new ArrayVariableNode(var);
}

Node GatherNumber(Number num)
{
	Node numnode = null;
	if(num.type == NumberType.CALL)
	{
		numnode = new CallNode(FindFunction(num.var));
	}
	else if(num.type == NumberType.IMMEDIATE)
	{
		numnode = FindConstant(num.imm, types.u16);
	}
	else if(num.type == NumberType.BINARY_OPERATION)
	{
		numnode = new BinaryNode(GatherNumber(num.binary_op.a), GatherNumber(num.binary_op.b), num.binary_op.type);
	}
	else if(num.type == NumberType.UNARY_OPERATION)
	{
		numnode = new UnaryNode(GatherNumber(num.unary_op.val),num.unary_op.type,num.unary_op.castto);
	}
	else if(num.type == NumberType.VARIABLE)
	{
		numnode = new ReferenceNode(FindVariable(num.var, null, types.voidtype));
	}
	else if(num.type == NumberType.ARRAYACCESS)
	{
		numnode = new ArrayAccessNode(FindArrayVariable(num.var,null),GatherNumber(num.args[0]));
	}
	else
	{
		throw new Exception("what is this " ~ to!string(num.type));
	}
	return numnode;
}

Node GatherStatement(Statement stat)
{
	Node statnode = null;
	if(stat.type == StatementType.ASSIGN)
	{
		statnode = new AssignNode(GatherNumber(stat.lvalue),GatherNumber(stat.rvalue));
	}
	else if(stat.type == StatementType.RETURN)
	{
		statnode = new ReturnNode(GatherNumber(stat.rvalue));
	}
	else if(stat.type == StatementType.CONDITIONAL)
	{
		ScopeNode sn = CreateScope(stat.inside);
		statnode = new ConditionNode(GatherNumber(stat.rvalue),sn,stat.loop);
		ExitScope(sn,stat.inside);
	}
	else
	{
		throw new Exception("how do i handle " ~ to!string(stat.type));
	}
	return statnode;
}

ScopeNode CreateScope(Scope inside)
{
	ScopeNode scopenode = new ScopeNode();
	foreach(var; inside.variables)
	{
		FindVariable(var.name, GatherNumber(var.defaultval), var.type);
	}
	return scopenode;
}

void ExitScope(ScopeNode scopenode, Scope inside)
{
	foreach(stat; inside.statements)
	{
		scopenode.AddInput(GatherStatement(stat));
	}
	scopenode.pop();
}

Node GatherVariable(Variable var)
{
	Node varnode = new Node();
	return varnode;
}

Node GatherFunction(Function func)
{
	FunctionNode funcnode = FindFunction(func.name);
	ScopeNode scopenode = CreateScope(func.inside);
	funcnode.AddInput(scopenode);
	foreach(var; func.args)
	{
		FindVariable(var.name, GatherNumber(var.defaultval), var.type);
	}
	ExitScope(scopenode,func.inside);
	return funcnode;
	
}

void Gather(Module mod)
{
	ScopeNode modulescopenode = new ScopeNode();
	foreach(var; mod.variables)
	{
		if(var.elements != 0)
		{
			FindArrayVariable(var.name,var);
		}
		else
		{
			FindVariable(var.name, GatherNumber(var.defaultval), var.type);
		}
	}
	foreach(func; mod.functions)
	{
		GatherFunction(func);
	}
}