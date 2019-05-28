module Abstract

public data TYPE = natural() | string() | boolean();    
	  
public alias PicoId = str;                  
	  
public data PROGRAM =                       
  program(list[DECL] decls, list[STATEMENT] stats);

public data DECL =
  decl(PicoId name, TYPE tp);

public data EXP = 
       id(PicoId name)
     | natCon(int iVal)
     | strCon(str sVal)
     | boolCon(str bVal)
     | add(EXP left, EXP right)
     | sub(EXP left, EXP right)
     | mul(EXP left, EXP right)
     | div(EXP left, EXP right)
     | eq(EXP left, EXP right)
     | neq(EXP left, EXP right)
     | conc(EXP left, EXP right)
     ;
    
public data STATEMENT =
       asgStat(PicoId name, EXP exp)
     | ifElseStat(EXP exp, list[STATEMENT] thenpart, list[STATEMENT] elsepart)
     | whileStat(EXP exp, list[STATEMENT] body)
     | forStat(STATEMENT ini, EXP exp, STATEMENT inc, list[STATEMENT] body)
     ;

anno loc TYPE@location;                   
anno loc PROGRAM@location;
anno loc DECL@location;
anno loc EXP@location;
anno loc STATEMENT@location;

public alias Occurrence = tuple[loc location, PicoId name, STATEMENT stat];