(* ::Package:: *)

(* ::Package:: *)
(* :Title: WordVec *)
(* :Context: WordVec` *)
(* :Authors: FB, MD, YH, MR *)



BeginPackage["WordVec`"];


Main::usage = "Main[] main routine of the WordVec package";


Begin["`Private`"];


Main[]:= NumberLinePlot[{1,2,3}]


MiniMap[loc_] := GeoGraphics[toLocation[loc], ImageSize->Small]


toLocation[loc_] := Interpreter["Location"][loc]


End[];


EndPackage[];
