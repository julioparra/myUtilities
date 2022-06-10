(* ::Package:: *)

BeginPackage["myUtilities`"]


timing::usage = 
		"Executes its argument and prints out the AbsoluteTiming time taken"
timingc::usage =
		"timingc[n] is a version of timing that constrains evaluation to n seconds"

timingM::usage = 
		"timingM[expr] works like timing, but also prints a temporary cell 
		to track the elapsed time during evaluation"
		
writeToFileList::usage = 
		"Writes the element of a list to a file."

replace::usage = 
		"replace[x] is an opperator that applies the replacement rules x to an expression using /."

replaceAll::usage = 
		"replaceAll[x] is an opperator that applies the replacement rules x to an expression using //."

zip::usage = 
		"zip[{a1, a2, ...},{b1, b2, ...}] = {{a1,b1},{a2,b2}, ...}"


Begin["Private`"]


ClearAll[timing]
SetAttributes[timing,{HoldFirst,SequenceHold}]
timing[expr_]:=Module[{res=AbsoluteTiming[expr]},Print[Row@{"Timing (line "<>ToString@$Line<>")  :  ", First@res, " seconds"}];res[[2]] ]

ClearAll[timingc]
SetAttributes[timingc,Attributes@timing]
timingc[args___]:=Function[Null,timing@TimeConstrained[#,args],{HoldAll,SequenceHold}]

ClearAll[timingM]
(* monitored timing *)
(* from https://mathematica.stackexchange.com/a/169921/11035 
	and (even moreso) https://mathematica.stackexchange.com/a/145607/11035 *)
SetAttributes[timingM, HoldAll];
Options[timingM] = { 
	"updateInterval" -> 0.1 (* measured in seconds *),
	"roundQ" -> True
}
timingM[ expr_, OptionsPattern[] ] := With[
	{
		start = AbsoluteTime[]
		(*,formatFcn = \
			If[ TrueQ@OptionValue@"roundQ", 
				Round[#, OptionValue@"updateInterval"]&,
				Identity
			]*)
	},
   	(*Monitor[expr, DateDifference[start, Now, "Second"] ];*)
   	Row[{
      		StringForm["Running timing (line ``)  :  ", $Line],
(* 
      		Dynamic[ AbsoluteTime[] - start // formatFcn, 
       			UpdateInterval -> OptionValue@"updateInterval" 
			],
*)
			Dynamic @ Clock[{0, Infinity, OptionValue@"updateInterval"}],
			" seconds"
      	}] // PrintTemporary;
   	expr
   ] // timing


writeToFileList[list_,listName_,fileName_]:= Module[{file},
file=OpenAppend[fileName];
Do[
WriteString[file,listName<>"["<>ToString[i]<>"] = "];
Write[file,list[[i]]];
WriteString[file,"\n"];
,{i,1,Length[list]}];
Close[file]]


replace[rule_][exp_] := (exp) /. rule
replaceAll[rule_][exp_] := (exp) //. rule


zip[a_List,b_List,f_]:= Module[{},Table[f[a[[i]],b[[j]]],{i,1,Length[a]},{j,1,Length[b]}]]


End[]


EndPackage[]
