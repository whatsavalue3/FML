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
	varIndex[var] = emittedram.length+0x4800;
	emittedram ~= new L_Imm(cast(ushort)(emittedram.length+0x4800));
}

void Label(ulong label)
{
	labelIndex[label] = emitted.length;
}

void Generate()
{
	Emit(0xf000);
	Emit(0x06);
	Emit(0x06);
	foreach(node; nodes)
	{
		node = node.Optimize();
		LabelFunction(cast(FunctionNode)node);
		node.Generate(new Restriction());
	}
	foreach(variable, type; globalvariables)
	{
		if(variable.type.isconst)
		{
			continue;
		}
		writeln(variable, ": ", variable.type);
		LabelVariable(variable);
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