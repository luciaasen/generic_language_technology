module Typecheck

import Prelude;
import Abstract;
import Load;

alias TENV = tuple[ map[PicoId, TYPE] symbols, list[tuple[loc l, str msg]] errors]; 

TENV addError(TENV env, loc l, str msg) = env[errors = env.errors + <l, msg>];      

str required(TYPE t, str got) = "Required <getName(t)>, got <got>";                 
str required(TYPE t1, TYPE t2) = required(t1, getName(t2));

// compile Expressions.

TENV checkExp(exp:natCon(int N), TYPE req, TENV env) =                              
  req == natural() ? env : addError(env, exp@location, required(req, "natural"));

TENV checkExp(exp:strCon(str S), TYPE req, TENV env) =
 req == string() ? env : addError(env, exp@location, required(req, "string"));

// HM2_1.1 Add the boolean constant
TENV checkExp(exp:boolCon(bool B), TYPE req, TENV env) =
 req == boolean() ? env : addError(env, exp@location, required(req, "boolean"));

TENV checkExp(exp:id(PicoId Id), TYPE req, TENV env) {                              
  if(!env.symbols[Id]?)
     return addError(env, exp@location, "Undeclared variable <Id>");
  tpid = env.symbols[Id];
  return req == tpid ? env : addError(env, exp@location, required(req, tpid));
}

TENV checkExp(exp:add(EXP E1, EXP E2), TYPE req, TENV env) =                        
  req == natural() ? checkExp(E1, natural(), checkExp(E2, natural(), env))
                   : addError(env, exp@location, required(req, "natural"));
  
TENV checkExp(exp:sub(EXP E1, EXP E2), TYPE req, TENV env) =                      
  req == natural() ? checkExp(E1, natural(), checkExp(E2, natural(), env))
                   : addError(env, exp@location, required(req, "natural"));

TENV checkExp(exp:conc(EXP E1, EXP E2), TYPE req, TENV env) =                    
  req == string() ? checkExp(E1, string(), checkExp(E2, string(), env))
                   : addError(env, exp@location, required(req, "string"));

// HW2_1.1 Add operations	-- start -- 
TENV checkExp(exp:mul(EXP E1, EXP E2), TYPE req, TENV env) =                      
  req == natural() ? checkExp(E1, natural(), checkExp(E2, natural(), env))
                   : addError(env, exp@location, required(req, "natural"));

TENV checkExp(exp:div(EXP E1, EXP E2), TYPE req, TENV env) =                      
  req == natural() ? checkExp(E1, natural(), checkExp(E2, natural(), env))
                   : addError(env, exp@location, required(req, "natural"));
                  
TENV checkExp(exp:eq(EXP E1, EXP E2), TYPE req, TENV env){	
	try{
		E1Type = getExpType(E1, env);
		return checkExp(E1, E1Type, checkExp(E2, E1Type, env));
	}
	catch NoSuchKey(k): 
		return addError(env, exp@location, "Undeclared variable <k>");
}
                                      
TENV checkExp(exp:neq(EXP E1, EXP E2), TYPE req, TENV env) {
	try{
		E1Type = getExpType(E1, env);
		return checkExp(E1, E1Type, checkExp(E2, E1Type, env));
	}
	catch NoSuchKey(k): 
		return addError(env, exp@location, "Undeclared variable <k>");
}
//                      --  end  -- 

// HW2_1.1 Add getExpType	-- start -- 
TYPE getExpType(exp:natCon(int n), TENV env) = natural();
TYPE getExpType(exp:strCon(str s), TENV env) = string();
TYPE getExpType(exp:boolCon(bool b), TENV env) = boolean();

TYPE getExpType(exp:add(EXP E1, EXP E2), TENV env) = natural();
TYPE getExpType(exp:sub(EXP E1, EXP E2), TENV env) = natural();
TYPE getExpType(exp:mul(EXP E1, EXP E2), TENV env) = natural();
TYPE getExpType(exp:div(EXP E1, EXP E2), TENV env) = natural();
TYPE getExpType(exp:conc(EXP E1, EXP E2), TENV env) = string();
TYPE getExpType(exp:eq(EXP E1, EXP E2), TENV env) = boolean();
TYPE getExpType(exp:neq(EXP E1, EXP E2), TENV env) = boolean();

TYPE getExpType(exp:id(PicoId Id), TENV env) = env.symbols[Id];
//                      --  end  -- 

// check a statement

TENV checkStat(stat:asgStat(PicoId Id, EXP Exp), TENV env) {                        
  if(!env.symbols[Id]?)
     return addError(env, stat@location, "Undeclared variable <Id>");
  tpid = env.symbols[Id];
  return checkExp(Exp, tpid, env);
}

TENV checkStat(stat:ifElseStat(EXP Exp,                                             
                              list[STATEMENT] Stats1,
                              list[STATEMENT] Stats2),
               TENV env){
    env0 = checkExp(Exp, natural(), env);
    env1 = checkStats(Stats1, env0);
    env2 = checkStats(Stats2, env1);
    return env2;
}

TENV checkStat(stat:whileStat(EXP Exp, 
                             list[STATEMENT] Stats1),
                 TENV env) {
    env0 = checkExp(Exp, natural(), env);
    env1 = checkStats(Stats1, env0);
    return env1;
}

// HW2_1.2 Add checkStat for FOR loop  -- start --
TENV checkStat(stat:forStat(list[STATEMENT] Stats1,
							EXP Exp,
							list[STATEMENT] Stats2, 
                            list[STATEMENT] Stats3),
                 TENV env) {
                 
    env0 = checkStats(Stats1, env);
    env1 = checkExp(Exp, boolean(), env0);
    env2 = checkStats(Stats2, env1);
    env3 = checkStats(Stats3, env2);
    return env3;
}
//								   --  end  --

// check a list of statements
TENV checkStats(list[STATEMENT] Stats1, TENV env) {                                 
  for(S <- Stats1){
      env = checkStat(S, env);
  }
  return env;
}
  
// check declarations

TENV checkDecls(list[DECL] Decls) =                                                 
    <( Id : tp  | decl(PicoId Id, TYPE tp) <- Decls), []>;

// check a Pico program

public TENV checkProgram(PROGRAM P){                                                
  if(program(list[DECL] Decls, list[STATEMENT] Series) := P){
     TENV env = checkDecls(Decls);
     return checkStats(Series, env);
  } else
    throw "Cannot happen";
}
                                                                                    
public list[tuple[loc l, str msg]] checkProgram(str txt) = checkProgram(load(txt)).errors;