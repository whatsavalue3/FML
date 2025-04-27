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
ulong[FunctionNode] functionIndex;
ulong[ulong] labelIndex;



void Emit(ushort val)
{
	emitted ~= new L_Imm(val);
}

void LinkFunction(FunctionNode func)
{
	emitted ~= new L_FuncLink(func);
}

void LinkLabelOffset(ushort val, ulong label)
{
	emitted ~= new L_FuncOffsetLink(val,emitted.length,label);
}

void LabelFunction(FunctionNode func)
{
	functionIndex[func] = emitted.length;
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
		node.Generate();
	}
	
	writeln(emitted);
	
	ushort[] ushorts;
	foreach(linkable; emitted)
	{
		ushorts ~= linkable.Get();
	}
	
	toFile(ushorts,"rom.bin");
}