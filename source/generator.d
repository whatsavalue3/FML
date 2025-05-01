import gatherer;
import app;
import std.stdio;
import std.conv;

interface Linkable
{
	ushort Get();
}

class L_Imm : Linkable
{
	ushort val;
	this(ushort val)
	{
		this.val = val;
	}
	
	ushort Get()
	{
		return val;
	}
}

class L_FuncLink : Linkable
{
	FunctionNode func;
	this(FunctionNode func)
	{
		this.func = func;
	}
	
	ushort Get()
	{
		return cast(ushort)(functionIndex[this.func]<<1);
	}
}

class L_VarLink : Linkable
{
	VariableNode var;
	this(VariableNode var)
	{
		this.var = var;
	}
	
	ushort Get()
	{
		return cast(ushort)(varIndex[this.var]<<1);
	}
}

class L_ArrayVarLink : Linkable
{
	ArrayVariableNode var;
	this(ArrayVariableNode var)
	{
		this.var = var;
	}
	
	ushort Get()
	{
		return cast(ushort)(arrayIndex[this.var]<<1);
	}
}

class L_FuncOffsetLink : Linkable
{
	ulong label;
	ulong begin;
	ushort val;
	this(ushort val, ulong begin, ulong label)
	{
		this.label = label;
		this.begin = begin;
		this.val = val;
	}
	
	ushort Get()
	{
		return this.val | cast(ubyte)(cast(byte)(labelIndex[this.label] - this.begin) - 1);
	}
}


Linkable[] emitted;
Linkable[] emittedram;
ulong[FunctionNode] functionIndex;
ulong[VariableNode] varIndex;
ulong[ArrayVariableNode] arrayIndex;
ulong[ulong] labelIndex;



void Emit(ushort val)
{
	emitted ~= new L_Imm(val);
}

void LinkFunction(FunctionNode func)
{
	emitted ~= new L_FuncLink(func);
}

void LinkVariable(VariableNode func)
{
	emitted ~= new L_VarLink(func);
}

void LinkArrayVariable(ArrayVariableNode func)
{
	emitted ~= new L_ArrayVarLink(func);
}

void LinkLabelOffset(ushort val, ulong label)
{
	emitted ~= new L_FuncOffsetLink(val,emitted.length,label);
}

void LabelFunction(FunctionNode func)
{
	functionIndex[func] = emitted.length;
}

void LabelVariable(VariableNode var)
{
	if(var.hasloc)
	{
		varIndex[var] = var.location>>1;
	}
	else
	{
		varIndex[var] = emittedram.length+0x4800;
		emittedram ~= new L_Imm(cast(ushort)(emittedram.length+0x4800));
	}
}

void LabelArrayVariable(ArrayVariableNode var)
{
	if(var.var.hasloc)
	{
		arrayIndex[var] = var.var.location>>1;
		return;
	}
	arrayIndex[var] = emitted.length;
	if(var.var.type.size == 8)
	{
		ushort p = 0;
		bool parity = false;
		foreach(value; var.var.defaultelements)
		{
			p >>= 8;
			p |= value<<8;
			if(parity)
			{
				emitted ~= new L_Imm(p);
			}
			parity = !parity;
		}
	}
	else if(var.var.type.size == 16)
	{
		foreach(value; var.var.defaultelements)
		{
			emitted ~= new L_Imm(value);
		}
	}
}

void Label(ulong label)
{
	labelIndex[label] = emitted.length;
}

void Generate()
{
	Emit(0xf000);
	foreach(node; nodes)
	{
		if((cast(FunctionNode)node).name == "main")
		{
			LinkFunction(cast(FunctionNode)node);
			break;
		}
	}
	Emit(0x06);
	foreach(node; nodes)
	{
		node = node.Optimize().Optimize();
		node.SmartOptimize();
		LabelFunction(cast(FunctionNode)node);
		node.Generate(new Restriction());
	}
	foreach(variable, type; globalvariables)
	{
		if(variable.type.isconst)
		{
			if(variable.hasloc)
			{
				LabelVariable(variable);
			}
			continue;
		}
		writeln(variable, ": ", variable.type);
		LabelVariable(variable);
	}
	
	foreach(node, var; globalarrays)
	{
		LabelArrayVariable(var);
	}
	
	writeln(emitted);
	
	ushort[] ushorts;
	ushorts.length = 0x40000;
	foreach(i, linkable; emitted)
	{
		ushorts[i] = linkable.Get();
	}
	
	foreach(i, linkable; emittedram)
	{
		ushorts[i+0x4800] = linkable.Get();
	}
	
	toFile(ushorts,"rom.bin");
}