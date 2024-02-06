#include "nullary.h"
#include "unary.h"
#include "binary.h"
#include <math.h>



namespace sym 
{
	
	bool NegOp::is_neg() const { return true;}

	__expr_t* NegOp::eval(const var_map_t& vars) const { 
		__expr_t* dummy = operand->eval(vars);
		if(dummy->is_nullary() && (dynamic_cast<const __nullary_op_t*>(dummy))->is_con()){
			Const* result = new Const(-(dynamic_cast<const Const*>(dummy)->get_value()));
			delete dummy;
			return result;	
		}
		else{  
			delete dummy;
			return new NegOp(operand->eval(vars));
		}
		
	}

	__expr_t* NegOp::diff(const std::string& v) const { 
		return NegOp(operand->diff(v)).eval();
	}

	std::ostream& NegOp::operator<< (std::ostream &out) const { 
		if(operand->is_nullary()){
			out << "-" << *operand;
		}
		else{
			out << "-" << "(" << *operand << ")";
		}
		return out;
	}

	bool NegOp::operator==(const __expr_t& other_) const { 
		if(other_.is_unary() && dynamic_cast<const __unary_op_t&>(other_).is_neg()){
			return ((*operand) == *(dynamic_cast<const NegOp&>(other_).operand));
		}
		return false;
	}
}

namespace sym 
{
	bool ExpOp::is_exp() const { return true;}

	__expr_t* ExpOp::eval(const var_map_t& vars) const { 
		__expr_t* dummy = operand->eval(vars);
		if(dummy->is_nullary() && (dynamic_cast<const __nullary_op_t*>(dummy))->is_con()){
			Const* result = new Const(std::exp(dynamic_cast<const Const*>(dummy)->get_value()));
			delete dummy;
			return result;	
		}
		else{ 
			return new ExpOp(dummy);
		}
	}

	__expr_t* ExpOp::diff(const std::string& v) const { 
		return MulOp(operand->diff(v), new ExpOp(operand->eval())).eval();
	}

	std::ostream& ExpOp::operator<< (std::ostream &out) const { 
		if(operand->is_nullary()){
			out << "e^" << *operand;
		}
		else{
			out << "e^" << "(" << *operand << ")";
		}
		return out;
	}

	bool ExpOp::operator==(const __expr_t& other_) const { 
		if(other_.is_unary() && dynamic_cast<const __unary_op_t&>(other_).is_exp()){
			return ((*operand) == *(dynamic_cast<const ExpOp&>(other_).operand));
		}
		return false;
	}
}
