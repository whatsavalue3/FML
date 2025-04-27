import app;
import std.stdio;
import std.conv;
import std.algorithm;
import generator;

class Restriction
{
	bool[16] used_registers;
	ubyte result_register;
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
	
	override void Generate()
	{
		writeln("Begin Function ", this.name);
		foreach(input; inputs)
		{
			input.Generate();
		}
		writeln("End Function ", this.name);
	}
}

class ConstantNode : Node
{
	ushort val;
	Type type;
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
		if(this.val < 0x80)
		{
			Emit(cast(ushort)(0xE000 | (res.result_register<<8) | this.val));
		}
		else if(this.val >= 0xff80)
		{
			Emit(cast(ushort)(0xE000 | (res.result_register<<8) | (this.val&0xff)));
		}
		else
		{
			Emit(cast(ushort)((res.result_register<<8)|(this.val&0xff)));
			Emit(cast(ushort)(((res.result_register+1)<<8)|((this.val>>>8)&0xff)));
		}
	}
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
	
	override void Generate()
	{
		writeln("Begin Scope ", index);
		foreach(input; inputs)
		{
			input.Generate();
		}
		writeln("End Scope ", index);
	}
}

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
		if(cast(ConstantNode)this.inputs[0])
		{
			(cast(ConstantNode)this.inputs[0]).type = this.type;
			return this.inputs[0];
		}
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
	ushort val;
	Type type;

	this(ushort val, Type type)
	{
		super();
		this.val = val;
		this.type = type;
	}

	override void Generate(Restriction res)
	{
		writeln("DerefImmNode ", this.val);
		if(type.size == 16)
		{
			Emit(cast(ushort)(0x9012 | ((cast(ushort)res.result_register)<<8)));
		}
		else if(type.size == 8)
		{
			Emit(cast(ushort)(0x9010 | ((cast(ushort)res.result_register)<<8)));
		}
		else
		{
			throw new Exception("bruh type.size = " ~ to!string(type.size));
		}
		Emit(this.val);
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
	
	override Node Optimize()
	{
		super.Optimize();
		if(cast(ConstantNode)this.inputs[0])
		{
			return new ConstantNode((cast(ConstantNode)this.inputs[0]).val, (cast(ConstantNode)this.inputs[0]).type);
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
	this(Node node, UnaryOperationType type)
	{
		this.AddInput(node);
		this.type = type;
	}
	
	override string toString()
	{
		return "Unary " ~ super.toString();
	}
	
	override void Generate()
	{
		super.Generate();
		writeln("Begin Unary ", type);
		foreach(input; inputs)
		{
			input.Generate();
		}
		writeln("End Unary ", type);
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
			return new DerefImmNode((cast(ConstantNode)this.inputs[0]).val,(cast(ConstantNode)this.inputs[0]).type);
		}
		return this;
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
		super.Generate();
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
				return new ConstantNode(cast(ushort)(a+b));
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
	
	override void Generate()
	{
		Restriction res = new Restriction();
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
	
	override void Generate()
	{
		Restriction res = new Restriction();
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
			inputs[1].Generate();
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
	
	override void Generate()
	{
		writeln("Begin Assign");
		Restriction res = new Restriction();
		res.result_register = 0;
		//inputs[0].Generate();
		if(cast(DerefImmNode)inputs[0])
		{
			inputs[1].Generate(res);
			Emit(cast(ushort)(0x9013));
			Emit((cast(DerefImmNode)inputs[0]).val);
		}
		else
		{
			throw new Exception("idfk how to emit this im paid too little");
		}
		writeln("End Assign");
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

Node GatherNumber(Number num)
{
	Node numnode = null;
	if(num.type == NumberType.CALL)
	{
		numnode = new CallNode(FindFunction(num.var));
	}
	else if(num.type == NumberType.IMMEDIATE)
	{
		numnode = new ConstantNode(num.imm);
	}
	else if(num.type == NumberType.BINARY_OPERATION)
	{
		numnode = new BinaryNode(GatherNumber(num.binary_op.a), GatherNumber(num.binary_op.b), num.binary_op.type);
	}
	else if(num.type == NumberType.UNARY_OPERATION)
	{
		numnode = new UnaryNode(GatherNumber(num.unary_op.val),num.unary_op.type);
	}
	else if(num.type == NumberType.VARIABLE)
	{
		numnode = new ReferenceNode(FindVariable(num.var, null, types.u16));
	}
	else
	{
		writeln(num.type);
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
		FindVariable(var.name, GatherNumber(var.defaultval), var.type);
	}
	foreach(func; mod.functions)
	{
		GatherFunction(func);
	}
}