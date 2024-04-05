(* ::Package:: *)

(* ::Package:: *)
(* :Title: WordVec *)
(* :Context: WordVec` *)
(* :Authors: FB, MD, YH, MR *)



BeginPackage["WordVec`"];


Main::usage = "Main[] main routine of the WordVec package";


Begin["`Private`"];


Main[text_]:= isValidText[text]


(* Auxiliary functions *)


(* Plots two vector in a n-dimensional space: TODO *)

(* Checks whether an expression is a number *)
isANumber[expr_] := NumericQ[expr]

(* Check whether a string is a valid word *)
isValidWord[str_, lang_: "English"] := DictionaryWordQ[str, Language -> lang]

(* Check wether a piece of text is valid *)
isValidText[text_String, lang_: "English"] := AllTrue[DeleteStopwords@TextWords[text], isValidWord[#, lang] &]


End[];


EndPackage[];
