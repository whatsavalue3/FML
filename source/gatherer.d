import app;
import std.stdio;
import std.conv;
import std.algorithm;
import generator;

class Restriction
{
	bool[16] used_registers;
	ubyte res_reg;
	
	Node[8] register_contain;
	Type[16] register_type;
	
	@property ubyte result_register()
	{
		return this.res_reg;
	}
	
	@property ubyte result_register(ubyte val)
	{
		return this.res_reg = val&0xf;
	}
	
	void IncResult(ubyte val)
	{
		this.res_reg = cast(ubyte)(this.res_reg + val)&0xf;
	}
	
	void SetResult(Node n)
	{
		this.register_contain[this.result_register>>1] = n;
		this.register_type[this.result_register] = n.GetType();
	}
	
	Type GetType(ubyte reg)
	{
		return register_type[reg];
	}
	
	void SetRegister(ubyte reg, Node n)
	{
		this.register_contain[reg>>1] = n;
	}
	
	bool ResultContains(Node n)
	{
		return this.register_contain[this.result_register>>1] == n;
	}
}

string tabs = "";

class Node
{
	Node[] inputs;
	Node[] outputs;
	bool printed = false;
	bool deleteme = false;
	
	this()
	{
		//nodes ~= this;
	}
	
	void AddInput(Node other)
	{
		this.inputs ~= other;
		if(countUntil(other.outputs,this) == -1)
		{
			other.outputs ~= this;
		}
	}
	
	bool Unused()
	{
		return this.outputs.length == 0;
	}
	
	override string toString()
	{
		return to!string(typeid(this));
	}
	
	void Generate()
	{
	
	}
	
	bool Reads(Node node)
	{
		return false;
	}
	
	bool Writes(Node node)
	{
		return false;
	}
	
	bool Modifies(Node node)
	{
		return false;
	}
	
	Node GetModifier()
	{
		return null;
	}
	
	void Generate(Restriction res)
	{
		throw new Exception("tried to generate a restricted Node " ~ to!string(typeid(this)));
	}
	
	bool OptimizeOutputs()
	{
		if(this.deleteme)
		{
			writeln(this);
			throw new Exception("f");
		}
		if(this.Unused())
		{
			return false;
		}
		foreach(i, outputA; this.outputs)
		{
			foreach(j, outputB; this.outputs)
			{
				if(i == j)
				{
					continue;
				}
				if(outputA.deleteme)
				{
					writeln(outputA,cast(void*)(outputA));
					throw new Exception("fu");
				}
				if(outputB.deleteme)
				{
					throw new Exception("ck");
				}
				if(outputA.EquivalentTo(outputB))
				{
					if(outputB == this)
					{
						throw new Exception("KILL ASFOI DJSgfi");
					}
					//writeln("MERGING ", typeid(outputA), " ", typeid(outputB));
					this.outputs = this.outputs.remove(j);
					outputA.Merge(outputB);
					return true;
				}
			}
		}
		return false;
	}
	
	
	
	Node Optimize()
	{
		if(deleteme)
		{
			writeln(typeid(this),cast(void*)this);
			throw new Exception("why");
			return this;
		}
		foreach(i, input; inputs)
		{
			Node newnode = input.Optimize();
			if(this.deleteme)
			{
				return newnode;
			}
			if(newnode != input)
			{
				newnode.Steal(input);
//				inputs[i] = newnode;
			}
		}
		while(this.OptimizeOutputs())
		{
		
		}
		return this;
	}
	
	void SmartOptimize()
	{
		if(deleteme)
		{
			throw new Exception("why");
			return;
		}
		foreach(input; inputs)
		{
			writeln(tabs,input, cast(void*)input);
			tabs ~= "    ";
			input.SmartOptimize();
			tabs.length -= 4;
			if(deleteme)
			{
				throw new Exception("why");
				return;
			}
		}
	}
	
	void Merge(Node other)
	{
		if(other == this)
		{
			throw new Exception("KILL");
		}
		foreach(otherout; other.outputs)
		{
			foreach(i, otheroutin; otherout.inputs)
			{
				if(otheroutin == other)
				{
					otherout.inputs[i] = this;
				}
			}
			if(countUntil(this.outputs, otherout) == -1)
			{
				this.outputs ~= otherout;
			}
		}
		
		foreach(otherin; other.inputs)
		{
			foreach_reverse(i, otherinout; otherin.outputs)
			{
				if(otherinout == other)
				{
					if(countUntil(otherin.outputs, this) != -1)
					{
						otherin.outputs = otherin.outputs.remove(i);
					}
					else
					{
						otherin.outputs[i] = this;
					}
				}
			}
		}
		
		other.deleteme = true;
		
		writeln("merging ", other, cast(void*)other, " and ", this, cast(void*)this);
		
		//other.destroy();
	}
	
	void Steal(Node oldnode)
	{
		if(this == oldnode)
		{
			throw new Exception("KILL");
		}
		Node[] oldoutputs = oldnode.outputs;
		Node[] oldinputs = oldnode.inputs;
		foreach(o; oldoutputs)
		{
			foreach(j, elem; o.inputs)
			{
				if(elem == oldnode)
				{
					o.inputs[j] = this;
					writeln(oldnode, " wha ", o);
				}
			}
		}
		
		foreach(oi; oldinputs)
		{
			foreach_reverse(j, elem; oi.outputs)
			{
				if(elem == oldnode)
				{
					oi.outputs = oi.outputs.remove(j);
					//oi.outputs[j] = this;
				}
			}
		}
		
		this.outputs = oldoutputs;
		oldnode.deleteme = true;		
		//writeln("stealing ", oldnode, cast(void*)oldnode);
		//oldnode.destroy();
	}
	
	bool EquivalentTo(Node other)
	{
		return this == other;
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
	Type type;
	this(string name, Type type)
	{
		super();
		this.name = name;
		this.type = type;
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
		foreach(i, arg; inputs[1..$])
		{
			res.SetRegister((cast(ubyte)(i<<1)),arg);
		}
		res.result_register = cast(ubyte)((inputs.length-1)<<1);
		inputs[0].Generate(res);
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
		res.SetResult(this);
	}
	
	override Type GetType()
	{
		if(this.type.ptrdepth > 0)
		{
			return types.u16;
		}
		return this.type;
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
		//writeln("Begin Scope ", index);
		foreach(input; inputs)
		{
			input.Generate(res);
		}
		//writeln("End Scope ", index);
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
		if(value !is null)
		{
			this.AddInput(value);
		}
	}
	
	override string toString()
	{
		return this.name;
	}
	
	override void Generate()
	{
		writeln("Variable " ~ name);
	}
	
	override void Generate(Restriction res)
	{
		writeln("piss");
	}
	
	override Type GetType()
	{
		if(this.type.ptrdepth > 0)
		{
			return types.u16;
		}
		return this.type;
	}
	
	override Node Optimize()
	{
		super.Optimize();
		if(this.inputs.length != 0 && cast(ConstantNode)this.inputs[0] && this.type.isconst)
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

class ArgumentNode : Node
{
	string name;
	ubyte index;
	Type type;
	
	this(string name, ubyte index, Type type)
	{
		this.name = name;
		this.type = type;
		this.index = index;
		scopestack[$-1].AddInput(this);
	}
	
	override string toString()
	{
		return this.name;
	}
	
	override void Generate()
	{
		writeln("Variable " ~ name);
	}
	
	override void Generate(Restriction res)
	{
		res.result_register = cast(ubyte)(this.index<<1);
		res.SetResult(this);
	}
	
	override Type GetType()
	{
		if(this.type.ptrdepth > 0)
		{
			return types.u16;
		}
		return this.type;
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
	
	override Type GetType()
	{
		return this.var.type;
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
	
	override Type GetType()
	{
		return (cast(FunctionNode)this.inputs[0]).type;
	}
	
	override void Generate(Restriction res)
	{
		FunctionNode fn = (cast(FunctionNode)this.inputs[0]);
		res.result_register = 0;
		foreach(input; inputs[1..$])
		{
			input.Generate(res);
			res.IncResult(2);
		}
		
		Emit(cast(ushort)(0xF001));
		LinkFunction(fn);
		ubyte result = res.result_register;
		if(result != 0)
		{
			if(fn.type.size == 16)
			{
				Emit(0xF005 | (result<<8));
			}
			else if(fn.type.size == 8)
			{
				Emit(0x8000 | (result<<8));
			}
			else if(fn.type.size == 0)
			{
				
			}
			else
			{
				throw new Exception("invalid function return type size " ~ to!string(fn.type.size));
			}
		}
		//res.result_register = result;
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
		if(res.ResultContains(this))
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
		res.SetResult(this);
	}
	
	override Type GetType()
	{
		return this.inputs[0].GetType();
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
	
	override void AddInput(Node other)
	{
		this.inputs ~= other;
		other.outputs ~= this;
	}
	
	override Type GetType()
	{
		return this.inputs[0].GetType();
	}
	
	override void Generate()
	{
		writeln("Ref ", (cast(VariableNode)inputs[0]).name);
	}
	
	override void Generate(Restriction res)
	{
		writeln(this.inputs[0],this.outputs);
		if(res.ResultContains(this.inputs[0]))
		{
			return;
		}
		foreach(i, thing; res.register_contain)
		{
			if(thing == this.inputs[0])
			{
				//ubyte size = (cast(VariableNode)this.inputs[0]).type.size;
				res.result_register = cast(ubyte)(i<<1);
				/*
				if(size == 8)
				{
					Emit(cast(ushort)(0x8000 | (res.result_register << 8) | (i << 5)));
				}
				else if(size == 16)
				{
					Emit(cast(ushort)(0xF005 | (res.result_register << 8) | (i << 5)));
				}
				else
				{
					throw new Exception("poo.  cant emit dereference of a variable of size " ~ to!string(size));
				}
				*/
				res.SetResult(this.inputs[0]);
				return;
			}
		}
		Type type = this.inputs[0].GetType();
		if(type.size == 16)
		{
			Emit(cast(ushort)(0x9012 | (res.result_register<<8)));
		}
		else if(type.size == 8)
		{
			Emit(cast(ushort)(0x9010 | (res.result_register<<8)));
		}
		else
		{
			throw new Exception("idk how to emit type.szie = " ~ to!string(type.size));
		}
		LinkVariable(cast(VariableNode)this.inputs[0]);
		res.SetResult(this.inputs[0]);
	}
	
	override Node Optimize()
	{
		super.Optimize();
		if(cast(ConstantNode)this.inputs[0])
		{
			return this.inputs[0];
		}
		return this;
	}
	
	override void SmartOptimize()
	{
		Node[] readers;
		Node[] modifiers;
		foreach(output; outputs)
		{
			if(output.Modifies(this))
			{
				modifiers ~= output.GetModifier();
			}
			else if(output.Reads(this) && countUntil(modifiers,output) == -1)
			{
				readers ~= output;
			}
		}
		
		bool justadds = true;
		foreach(modifier; modifiers)
		{
			if((cast(AddImmNode)modifier) is null)
			{
				justadds = false;
				break;
			}
		}
		
		ubyte howMuchLeftShift = 0x10;
		foreach(reader; readers)
		{
			if(cast(LshImmNode)reader)
			{
				howMuchLeftShift = min(howMuchLeftShift,(cast(LshImmNode)reader).val);
			}
			else if(cast(OperationImmNode)reader)
			{
				
			}
			else if(cast(ArrayAccessNode)reader)
			{
				
			}
			else
			{
				writeln("WHAT  TE ", reader);
				howMuchLeftShift = 0;
				break;
			}
		}
		writeln(readers);
		//writeln(this.inputs[0], ": ", readers, " howMuchLeftShift ", howMuchLeftShift);
		if(justadds && howMuchLeftShift)
		{
			RshImmNode rin = new RshImmNode(this,howMuchLeftShift);
			foreach(modifier; modifiers)
			{
				(cast(AddImmNode)modifier).val <<= howMuchLeftShift;
			}
			foreach(reader; readers)
			{
				if(cast(LshImmNode)reader)
				{
					this.Steal(reader);
				}
				else if(cast(OperationImmNode)reader)
				{
					(cast(OperationImmNode)reader).val <<= howMuchLeftShift;
				}
				else if(cast(ArrayAccessNode)reader)
				{
					reader.inputs[1] = rin;
				}
			}
		}
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
	
	override bool Reads(Node n)
	{
		return n.EquivalentTo(this.inputs[0]);
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
		else if(this.type == UnaryOperationType.NOT)
		{
			Emit(cast(ushort)(0x40FF | (res.result_register << 8)));
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
		writeln("AddImmNode adds ", a, cast(void*)a);
		this.AddInput(a);
		this.val = val;
	}
	
	override bool Reads(Node n)
	{
		return this.inputs[0] == n;
	}
	
	override bool EquivalentTo(Node n)
	{
		AddImmNode other = cast(AddImmNode)(n);
		if(other is null)
		{
			return false;
		}
		return other.val == this.val && other.inputs[0] == this.inputs[0];
	}
	
	override Type GetType()
	{
		return this.inputs[0].GetType();
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
	
	override bool Reads(Node n)
	{
		return this.inputs[0] == n;
	}
	
	override bool EquivalentTo(Node n)
	{
		LshImmNode other = cast(LshImmNode)(n);
		if(other is null)
		{
			return false;
		}
		return other.val == this.val && other.inputs[0].EquivalentTo(this.inputs[0]);
	}
	
	override string toString()
	{
		return "LshImmNode " ~ super.toString();
	}
	
	override Type GetType()
	{
		return this.inputs[0].GetType();
	}
	
	override void Generate(Restriction res)
	{
		if(res.ResultContains(this))
		{
			res.SetResult(this);
			return;
		}
		inputs[0].Generate(res);
		Emit(0x900B | cast(ushort)((res.result_register+1)<<8) | cast(ushort)(this.val<<4));
		Emit(0x900A | cast(ushort)(res.result_register<<8) | cast(ushort)(this.val<<4));
		res.SetResult(this);
	}
}


class RshImmNode : Node
{
	ubyte val;
	this(Node a, ubyte val)
	{
		this.AddInput(a);
		this.val = val;
	}
	
	override bool Reads(Node n)
	{
		return this.inputs[0] == n;
	}
	
	override bool EquivalentTo(Node n)
	{
		RshImmNode other = cast(RshImmNode)(n);
		if(other is null)
		{
			return false;
		}
		return other.val == this.val && other.inputs[0].EquivalentTo(this.inputs[0]);
	}
	
	override Type GetType()
	{
		return this.inputs[0].GetType();
	}
	
	override void Generate(Restriction res)
	{
		if(res.ResultContains(this))
		{
			res.SetResult(this);
			return;
		}
		inputs[0].Generate(res);
		Emit(0x900D | cast(ushort)(res.result_register<<8) | cast(ushort)(this.val<<4));
		Emit(0x900C | cast(ushort)((res.result_register+1)<<8) | cast(ushort)(this.val<<4));
		res.SetResult(this);
	}
}

class OperationImmNode : Node
{
	ushort val;
	this(Node a, ushort val)
	{
		this.AddInput(a);
		this.val = val;
	}
	
	override bool Reads(Node n)
	{
		return this.inputs[0] == n;
	}
	
	override bool EquivalentTo(Node n)
	{
		typeof(this) other = cast(typeof(this))(n);
		if(other is null)
		{
			return false;
		}
		return other.val == this.val && other.inputs[0].EquivalentTo(this.inputs[0]);
	}
	
	override Type GetType()
	{
		return types.boolean;
	}
	
	override void Generate(Restriction res)
	{
		inputs[0].Generate(res);
		Emit(0x7000 | cast(ushort)((res.result_register)<<8) | cast(ushort)(this.val&0xff));
		Emit(0x5000 | cast(ushort)((res.result_register + 1)<<8) | cast(ushort)(this.val>>>8));
		res.SetResult(this);
	}
}

class GreaterImmNode : OperationImmNode
{
	this(Node a, ushort val)
	{
		super(a,val);
	}
}

class LesserImmNode : OperationImmNode
{
	this(Node a, ushort val)
	{
		super(a,val);
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
	
	override Type GetType()
	{
		return this.inputs[0].GetType();
	}
	
	override string toString()
	{
		return "Binary " ~ to!string(type) ~ " " ~ super.toString();
	}
	
	override bool EquivalentTo(Node n)
	{
		BinaryNode other = cast(BinaryNode)(n);
		if(other is null)
		{
			return false;
		}
		return other.type == this.type && other.inputs[1].EquivalentTo(this.inputs[1]) && other.inputs[0].EquivalentTo(this.inputs[0]);
	}
	
	override bool Reads(Node n)
	{
		return n.EquivalentTo(this.inputs[0]) || n.EquivalentTo(this.inputs[1]);
	}
	
	override void Generate(Restriction res)
	{
		foreach(i, result; res.register_contain)
		{
			if(result !is null && result.EquivalentTo(this))
			{
				res.result_register = cast(ubyte)(i<<1);
				return;
			}
		}
		inputs[0].Generate(res);
		ubyte first = res.result_register;
		ubyte firsttype = res.GetType(first).size;
		res.IncResult(2);
		inputs[1].Generate(res);
		ubyte second = res.result_register;
		ubyte secondtype = res.GetType(second).size;
		res.result_register = cast(ubyte)(first);
		if(this.type == BinaryOperationType.ADD)
		{
			if(firsttype == secondtype)
			{
				if(firsttype == 16)
				{
					Emit(0xF006 | cast(ushort)(first<<8) | cast(ushort)((second)<<4));
				}
				else if(firsttype == 8)
				{
					Emit(0x8001 | cast(ushort)(first<<8) | cast(ushort)((second)<<4));
				}
				else
				{
					throw new Exception("firsttype == " ~ to!string(firsttype));
				}
			}
			else
			{
				if(firsttype == 16 && secondtype == 8)
				{
					Emit(0x8001 | cast(ushort)(first<<8) | cast(ushort)((second)<<4));
					Emit(0x6000 | cast(ushort)((first+1)<<8));
				}
				else if(firsttype == 8 && secondtype == 16)
				{
					writeln(first, " ", this.inputs[0], " ", firsttype);
					res.result_register = cast(ubyte)(second);
					Emit(0x8001 | cast(ushort)(second<<8) | cast(ushort)((first)<<4));
					Emit(0x6000 | cast(ushort)((second+1)<<8));
				}
				else
				{
					throw new Exception("firsttype == " ~ to!string(firsttype) ~ " | secondtype = " ~ to!string(secondtype));
				}
			}
		}
		else if(this.type == BinaryOperationType.EQUAL)
		{
			Emit(0xF007 | cast(ushort)(first<<8) | cast(ushort)((second)<<4));
		}
		else if(this.type == BinaryOperationType.NOTEQUAL)
		{
			Emit(0xF007 | cast(ushort)(first<<8) | cast(ushort)((second)<<4));
		}
		else if(this.type == BinaryOperationType.GREATER)
		{
			Emit(0xF007 | cast(ushort)(first<<8) | cast(ushort)((second)<<4));
		}
		else if(this.type == BinaryOperationType.LEFTSHIFT)
		{
			Emit(0x800B | cast(ushort)((first+1)<<8) | cast(ushort)((second + 1)<<4));
			Emit(0x800A | cast(ushort)(first<<8) | cast(ushort)((second)<<4));
		}
		else if(this.type == BinaryOperationType.AND)
		{
			Emit(0x8002 | cast(ushort)(first<<8) | cast(ushort)((second)<<4));
		}
		else if(this.type == BinaryOperationType.SUB)
		{
			Emit(0x8008 | cast(ushort)(first<<8) | cast(ushort)((second)<<4));
			Emit(0x8009 | cast(ushort)((first+1)<<8) | cast(ushort)((second+1)<<4));
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
			if(this.type == BinaryOperationType.GREATER && (a < 0x100))
			{
				return new GreaterImmNode(this.inputs[1],a);
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
			if(this.type == BinaryOperationType.RIGHTSHIFTLOGI && (a < 0x10))
			{
				return new RshImmNode(this.inputs[0],cast(ubyte)a&0xf);
			}
			if(this.type == BinaryOperationType.GREATER && (a < 0x100))
			{
				return new LesserImmNode(this.inputs[0],a);
			}
		}
		return this;
	}
	
	override void SmartOptimize()
	{
		Node[] readers;
		Node[] modifiers;
		foreach(output; outputs)
		{
			if(output.Modifies(this))
			{
				modifiers ~= output.GetModifier();
			}
			else if(output.Reads(this))
			{
				readers ~= output;
			}
		}
		if(readers.length > 1)
		{
			
		}
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
		//writeln("Begin Return");
		//res.result_register = 0;
		foreach(input; inputs)
		{
			input.Generate(res);
		}
		if(res.result_register != 0)
		{
			Emit(0x8000 | cast(ushort)(res.result_register<<4));
		}
		Emit(0xFE1F);
		//writeln("End Return");
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
	
	override bool EquivalentTo(Node n)
	{
		ConditionNode other = cast(ConditionNode)(n);
		if(other is null)
		{
			return false;
		}
		return other.conditiontype == this.conditiontype && other.loop == this.loop && other.inputs[0].EquivalentTo(this.inputs[0]) && other.inputs[1].EquivalentTo(this.inputs[1]);
	}
	
	
	override string toString()
	{
		return "Condition " ~ super.toString();
	}
	
	override void Generate(Restriction res)
	{
		//Restriction res = new Restriction();
		//res.result_register = 0;
		//writeln("Begin Condition");
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
		//writeln("End Condition");
	}
	
	override Node Optimize()
	{
		super.Optimize();
		this.conditiontype = 0xC900;
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
		else if((cast(GreaterImmNode)this.inputs[0]))
		{
			this.conditiontype = 0xC000;
		}
		else if((cast(LesserImmNode)this.inputs[0]))
		{
			this.conditiontype = 0xC100;
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
		return "Assign " ~ to!string(this.inputs[0]) ~ " = " ~ to!string(this.inputs[1]);
	}
	
	override bool Reads(Node n)
	{
		return n.EquivalentTo(this.inputs[1]);
	}
	
	override bool Writes(Node n)
	{
		return n.EquivalentTo(this.inputs[0]);
	}
	
	override bool Modifies(Node n)
	{
		return n.EquivalentTo(this.inputs[0]) && this.inputs[1].inputs.length == 1 && this.inputs[1].Reads(n);
	}
	
	override Node GetModifier()
	{
		return this.inputs[1];
	}
	
	override bool EquivalentTo(Node n)
	{
		AssignNode other = cast(AssignNode)(n);
		if(other is null)
		{
			return false;
		}
		return other.inputs[1].EquivalentTo(this.inputs[1]) && other.inputs[0].EquivalentTo(this.inputs[0]);
	}
	
	override void Generate(Restriction res)
	{
		res.IncResult(2);
		//writeln("Begin Assign");
		//Restriction res = new Restriction();
		//inputs[0].Generate();
		if(cast(DerefImmNode)inputs[0])
		{
			inputs[1].Generate(res);
			Emit(cast(ushort)(0x9013 | (res.result_register<<8)));
			Emit((cast(ConstantNode)inputs[0].inputs[0]).val);
		}
		else if(cast(UnaryNode)inputs[0])
		{
			if((cast(UnaryNode)inputs[0]).type == UnaryOperationType.DEREF)
			{
				inputs[0].inputs[0].Generate(res);
				ubyte innie = res.result_register;
				write(innie);
				res.IncResult(2);
				inputs[1].Generate(res);
				ubyte outie = res.result_register;
				
				if(inputs[0].GetType().size == 16)
				{
					Emit(cast(ushort)(0x9003 | (outie << 8) | (innie << 4)));
				}
				else if(inputs[0].GetType().size == 8)
				{
					Emit(cast(ushort)(0x9001 | (outie << 8) | (innie << 4)));
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
			if(inputs[0].GetType().size == 16)
			{
				Emit(cast(ushort)(0x9013 | (res.result_register<<8)));
			}
			else if(inputs[0].GetType().size == 8)
			{
				Emit(cast(ushort)(0x9011 | (res.result_register<<8)));
			}
			else
			{
				throw new Exception("i HAVE to jump off the carpet... ");
			}
			LinkVariable(cast(VariableNode)(inputs[0].inputs[0]));
		}
		else
		{
			throw new Exception("idfk how to emit this im paid too little " ~ to!string(inputs[0]));
		}
		//writeln("End Assign");
	}
}

class ArrayAccessNode : Node
{
	this(Node a, Node b)
	{
		this.AddInput(a);
		this.AddInput(b);
	}
	
	override bool Reads(Node n)
	{
		return n.EquivalentTo(this.inputs[1]);
	}
	
	override bool EquivalentTo(Node n)
	{
		return n == this || ((cast(ArrayAccessNode)n) && n.inputs[0] == this.inputs[0] && n.inputs[1] == this.inputs[1]);
	}
	
	override Type GetType()
	{
		return this.inputs[0].GetType();
	}
	
	override void Generate(Restriction res)
	{
		if(res.register_contain[((res.result_register + 2)&0xf)>>1] == this)
		{
			res.IncResult(2);
			res.SetResult(this);
			return;
		}
		writeln("NUJ ", res.register_contain[((res.result_register + 2)&0xf)>>1]);
		ArrayVariableNode avn = cast(ArrayVariableNode)inputs[0];
		inputs[1].Generate(res);
		ubyte origresult = res.result_register;
		res.IncResult(2);
		if(avn.var.type.size == 8)
		{
			Emit(cast(ushort)(0x9008 | (origresult << 4) | (res.result_register << 8)));
		}
		else
		{
			throw new Exception("avn.var.type.size = " ~ to!string(avn.var.type.size));
		}
		LinkArrayVariable(avn);
		res.SetResult(this);
	}
	
	override void SmartOptimize()
	{
		Node[] readers;
		Node[] modifiers;
		foreach(output; outputs)
		{
			if(output.Modifies(this))
			{
				modifiers ~= output.GetModifier();
			}
			else if(output.Reads(this))
			{
				readers ~= output;
			}
		}
		//if(readers.length > 1)
		//{
		//	writeln(typeid(this),readers);
		//}
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
	return new FunctionNode(name, types.voidtype);
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

ArgumentNode FindArgument(string name, ubyte index, Type type)
{
	foreach(scop; scopestack)
	{
		foreach(node; scop.inputs)
		{
			if(cast(ArgumentNode)(node) !is null)
			{
				if((cast(ArgumentNode)(node)).name == name)
				{
					return cast(ArgumentNode)node;
				}
			}
		}
	}
	
	return null;
}

ReferenceNode FindReference(VariableNode var)
{
	if(var.outputs.length > 2)
	{
		return cast(ReferenceNode)var.outputs[2];
	}
	return new ReferenceNode(var);
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
		foreach(arg; num.args)
		{
			numnode.AddInput(GatherNumber(arg));
		}
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
		numnode = FindArgument(num.var,0,types.voidtype);
		if(numnode is null)
		{
			numnode = FindReference(FindVariable(num.var, null, types.voidtype));
		}
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
	else if(stat.type == StatementType.CALL)
	{
		statnode = new CallNode(FindFunction(stat.lvalue.var));
		foreach(arg; stat.lvalue.args)
		{
			statnode.AddInput(GatherNumber(arg));
		}
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
		if(var.isarg)
		{
			FindArgument(var.name, var.arg, var.type);
			continue;
		}
		
		if(var.defaultval is null)
		{
			FindVariable(var.name, null, var.type);
		}
		else
		{
			FindVariable(var.name, GatherNumber(var.defaultval), var.type);
		}
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
	foreach(i, var; func.args)
	{
		funcnode.AddInput(new ArgumentNode(var.name, cast(ubyte)i, var.type));
	}
	funcnode.type = func.returntype;
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