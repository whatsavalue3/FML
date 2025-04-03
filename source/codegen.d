module codegen;
import app;
import std.stdio;

void writetab(T...)(T args)
{
	writeln(tab,args);
}

string tab = "";

void Stage1Number(Number num)
{
	
	switch(num.type)
	{
		case NumberType.IMMEDIATE:
			writetab(num.imm);
			break;
		case NumberType.UNARY_OPERATION:
			writetab(num.unary_op.type);
			writetab("{");
			tab ~= "\t";
			Stage1Number(num.unary_op.val);
			tab.length--;
			writetab("}");
			break;
		case NumberType.BINARY_OPERATION:
			writetab(num.binary_op.type);
			writetab("{");
			tab ~= "\t";
			Stage1Number(num.binary_op.a);
			Stage1Number(num.binary_op.b);
			tab.length--;
			writetab("}");
			break;
		case NumberType.VARIABLE:
			writetab(num.var);
			break;
		case NumberType.CALL:
			writetab("CALL ",num.var);
			writetab("{");
			tab ~= "\t";
			foreach(arg; num.args)
			{
				Stage1Number(arg);
			}
			tab.length--;
			writetab("}");
			break;
		default:
			break;
	}
	
}

void Stage1Assign(Number a, Number b)
{
	writetab("ASSIGN");
	writetab("{");
	tab ~= "\t";
	Stage1Number(a);
	Stage1Number(b);
	tab.length--;
	writetab("}");
}

void Stage1Conditional(Number a, Scope sco)
{
	writetab("CONDITIONAL");
	writetab("{");
	tab ~= "\t";
	Stage1Number(a);
	Stage1Scope(sco);
	tab.length--;
	writetab("}");
}

void Stage1Return(Number a)
{
	writetab("RETURN");
	writetab("{");
	tab ~= "\t";
	Stage1Number(a);
	tab.length--;
	writetab("}");
}

void Stage1Scope(Scope sco)
{
	writetab("SCOPE");
	writetab("{");
	tab ~= "\t";
	foreach(stat; sco.statements)
	{
		switch(stat.type)
		{
			case StatementType.ASSIGN:
				Stage1Assign(stat.lvalue,stat.rvalue);
				break;
			case StatementType.CONDITIONAL:
				Stage1Conditional(stat.rvalue,stat.inside);
				break;
			case StatementType.RETURN:
				Stage1Return(stat.rvalue);
				break;
			default:
				throw new Exception("invalid statement type");
				return;
		}
	}
	tab.length--;
	writetab("}");
}

void Stage1Function(Function func)
{
	writetab("FUNCTION ", func.name);
	writetab("{");
	tab ~= "\t";
	foreach(arg; func.args)
	{
		writetab(arg.type," ",arg.name);
	}
	Stage1Scope(func.inside);
	tab.length--;
	writetab("}");
}

void Stage1Module(Module mod)
{
	writetab("MODULE ", mod.name);
	writetab("{");
	tab ~= "\t";
	foreach(func; mod.functions)
	{
		Stage1Function(func);
	}
	tab.length--;
	writetab("}");
}