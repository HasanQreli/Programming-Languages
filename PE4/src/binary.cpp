#include "binary.h"
#include "nullary.h"
#include "type.h"
#include <math.h>



namespace sym 
{
	bool AddOp::is_add() const {return true; }

	__expr_t* AddOp::eval(const var_map_t& vars) const { 
		__expr_t *l = lhs_->eval(vars);
		__expr_t *r = rhs_->eval(vars);
		const Const *leftptr = dynamic_cast<const Const*>(l);
		const Const *rightptr = dynamic_cast<const Const*>(r);
		if(leftptr != NULL && rightptr != NULL){
			int value = leftptr->get_value() + rightptr->get_value();
			delete l;
			delete r;
			return new Const(value);
		} 
		
		else if(leftptr != NULL && (leftptr->get_value() == 0)){
			delete l;
			return r;
		}
		else if(rightptr != NULL && (rightptr->get_value() == 0)){
			delete r;
			return l;
		}
		else{
			return new AddOp(l, r);
		}
		
	}

	__expr_t* AddOp::diff(const std::string& v) const { 
		return AddOp(lhs_->diff(v), rhs_->diff(v)).eval();
	}

	std::ostream& AddOp::operator<< (std::ostream &out) const { 
		if(lhs_->is_nullary()){
			out << *lhs_;
		}
		else{
			out << "(" << *lhs_ << ")";
		}
		out << " + ";
		if(rhs_->is_nullary()){
			out << *rhs_;
		}
		else{
			out << "(" << *rhs_ << ")";
		}
		return out;
	}

	bool AddOp::operator==(const __expr_t& other_) const { 
		if(other_.is_binary() && dynamic_cast<const __binary_op_t&>(other_).is_add()){
			return ((*rhs_ == *dynamic_cast<const AddOp&>(other_).rhs_) && 
				   (*lhs_ == *dynamic_cast<const AddOp&>(other_).lhs_)) ||
				   ((*lhs_ == *dynamic_cast<const AddOp&>(other_).rhs_) && 
				   (*rhs_ == *dynamic_cast<const AddOp&>(other_).lhs_));
		}
	}
}

namespace sym 
{
	bool MulOp::is_mul() const { return true;}

	__expr_t* MulOp::eval(const var_map_t& vars) const { 
		__expr_t *l = lhs_->eval(vars);
		__expr_t *r = rhs_->eval(vars);
		const Const *leftptr = dynamic_cast<const Const*>(l);
		const Const *rightptr = dynamic_cast<const Const*>(r);
		if((leftptr != NULL && (leftptr->get_value() == 0)) || (rightptr != NULL && (rightptr->get_value() == 0))){
			delete l;
			delete r;
			return new Const(0);
		}
		else if(leftptr != NULL && rightptr != NULL){
			int value = dynamic_cast<const Const*>(l)->get_value() * dynamic_cast<const Const*>(r)->get_value();
			delete l;
			delete r;
			return new Const(value);
		}
		else if(leftptr != NULL && (leftptr->get_value() == 1)){
			delete l ;
			return (r);
		}
		else if(rightptr != NULL && (rightptr->get_value() == 1)){
			delete  r;
			return (l);
		}
		else{
			return new MulOp(l,r);
		}
	
	}

	__expr_t* MulOp::diff(const std::string& v) const { 
		return AddOp(MulOp(lhs_->diff(v),rhs_->eval()).eval(), MulOp(lhs_->eval(), rhs_->diff(v)).eval()).eval();
	}

	std::ostream& MulOp::operator<< (std::ostream &out) const { 
		if(lhs_->is_nullary()){
			out << *lhs_;
		}
		else{
			out << "(" << *lhs_ << ")";
		}
		out << " * ";
		if(rhs_->is_nullary()){
			out << *rhs_;
		}
		else{
			out << "(" << *rhs_ << ")";
		}
		return out;
	}

	bool MulOp::operator==(const __expr_t& other_) const { 
		if(other_.is_binary() && dynamic_cast<const __binary_op_t&>(other_).is_mul()){
			return ((*rhs_ == *dynamic_cast<const MulOp&>(other_).rhs_) && 
				   (*lhs_ == *dynamic_cast<const MulOp&>(other_).lhs_)) ||
				   ((*lhs_ == *dynamic_cast<const MulOp&>(other_).rhs_) && 
				   (*rhs_ == *dynamic_cast<const MulOp&>(other_).lhs_));
		}
	}
}
