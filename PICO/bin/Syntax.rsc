module Syntax

import Prelude;

lexical Id  = ([a-z][a-z0-9]* !>> [a-z0-9]) \ PicoKeywords;
lexical Natural = [0-9]+ ;
lexical String = "\"" ![\"]*  "\"";
lexical Boolean = "true" | "false";	// HW1_3.3 Add boolean type values

keyword PicoKeywords = "begin" | "end" | 
                       "declare" | "true" | "false" |
                       "if" | "then" | "else" | "fi" | 
                       "while" | "do" | "od" |
                       "true" | "false" | // HW1_3.2 Boolean keywords
                       "for" | // HW1_3.4 "for" loop keyword
                       "break" // HW1_3.4 extra termination condition keyword
                       ;

layout Layout = WhitespaceAndComment* !>> [\ \t\n\r%];

lexical WhitespaceAndComment 
   = [\ \t\n\r]
   | @category="Comment" "%" ![%]+ "%"
   | @category="Comment" "%%" ![\n]* $
   ;

start syntax Program 
   = program: "begin" Declarations decls {Statement  ";"}* body "end" ;

syntax Declarations 
   = "declare" {Declaration ","}* decls ";" ;  
 
syntax Declaration = decl: Id id ":" Type tp;

syntax Type 
   = natural:"natural" 
   | string :"string" 
   | boolean :"boolean"
   ;

syntax Statement 
   = asgStat: Id var ":="  Expression val 
   | ifElseStat: "if" Expression cond "then" {Statement ";"}*  thenPart "else" {Statement ";"}* elsePart "fi"
   | whileStat: "while" Expression cond "do" {Statement ";"}* body "od"
   // HW1_3.4 "for" loop statement with 3 parameters. Expression is mandatory.
   | forStat: "for" {Statement ","}* vari ";" Expression cond ";" {Statement ","}* oper "do" {Statement ";"}* body "od"
   | breakStat: "break" ";" 	// HW1_3.4 extra termination condition statement
  ;  
     
syntax Expression 
   = id: Id name
   | strCon: String string
   | natCon: Natural natcon
   | boolCon: Boolean boolcon
   | bracket "(" Expression e ")"
   
   > left conc: Expression lhs "||" Expression rhs
   // HW1_3.3 Numerical operators for multiplication and division
   > left ( mul: Expression lhs "*" Expression rhs
          | div: Expression lhs "/" Expression rhs
          )
   > left ( add: Expression lhs "+" Expression rhs
          | sub: Expression lhs "-" Expression rhs
          )
   // HW1_3.2 Comparison operators "==" and "!="
   > left ( eq: Expression lhs "==" Expression rhs
   		  | neq: Expression lhs "!=" Expression rhs 
   		  )
  ;

public start[Program] program(str s) {
  return parse(#start[Program], s);
}

public start[Program] program(str s, loc l) {
  return parse(#start[Program], s, l);
}