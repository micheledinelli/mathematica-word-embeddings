(* ::Package:: *)

BeginPackage["WordVec`"];


MiniMap::usage = "Minimap[loc] makes a tiny map of a location";


Begin["`Private`"];


MiniMap[loc_] := GeoGraphics[toLocation[loc], ImageSize->Small]


toLocation[loc_] := Interpreter["Location"][loc]


End[];


EndPackage[];
