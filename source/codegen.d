module codegen;
import app;
import std.stdio;

void Stage1Number(Number num)
{
	switch(num.type)
	{
		case NumberType.IMMEDIATE:
			writeln(num.imm);
			break;
		case NumberType.UNARY_OPERATION:
			writeln(num.unary_op.type);
			Stage1Number(num.unary_op.val);
			break;
		case NumberType.BINARY_OPERATION:
			writeln(num.binary_op);
			break;
		case NumberType.VARIABLE:
			writeln(num.var);
			break;
		default:
			break;
	}
}

void Stage1Assign(Number a, Number b)
{
	Stage1Number(a);
}

void Stage1Conditional(Number a, Scope sco)
{

}

void Stage1Return(Number a)
{

}

void Stage1Scope(Scope sco)
{
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
}

void Stage1Function(Function func)
{
	Stage1Scope(func.inside);
}

void Stage1Module(Module mod)
{
	foreach(func; mod.functions)
	{
		Stage1Function(func);
	}
}