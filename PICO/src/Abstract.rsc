module Abstract

// HW2 Add boolean type values							-- START --
public data TYPE = natural() | string() | boolean(); // --  END  --
	  
public alias PicoId = str;                  
	  
public data PROGRAM =                       
  program(list[DECL] decls, list[STATEMENT] stats);

public data DECL =
  decl(PicoId name, TYPE tp);

public data EXP = 
       id(PicoId name)
     | natCon(int iVal)
     | strCon(str sVal)
     // HW2_1.1 Add boolean constant      -- START --
     | boolCon(bool bVal) 		    //    --  END  --
     // HW2_1.1 Add operators 		   -- START --
     | mul(EXP left, EXP right)
     | div(EXP left, EXP right)
     | eq(EXP left, EXP right)
     | neq(EXP left, EXP right)     // --  END  --
     | add(EXP left, EXP right)
     | sub(EXP left, EXP right)
     | conc(EXP left, EXP right)
     ;
    
public data STATEMENT =
       asgStat(PicoId name, EXP exp)
     | ifElseStat(EXP exp, list[STATEMENT] thenpart, list[STATEMENT] elsepart)
     | whileStat(EXP exp, list[STATEMENT] body)
     // HW2_1.2 Add FOR loop statement:
     | forStat(list[STATEMENT] vari, EXP cond, list[STATEMENT] oper, list[STATEMENT] body)
     ;

anno loc TYPE@location;                   
anno loc PROGRAM@location;
anno loc DECL@location;
anno loc EXP@location;
anno loc STATEMENT@location;

public alias Occurrence = tuple[loc location, PicoId name, STATEMENT stat];