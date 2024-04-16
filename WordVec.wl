(* ::Package:: *)

(* ::Package:: *)
(* :Title: WordVec *)
(* :Context: WordVec` *)
(* :Author: BF DM HY RM *)
(* :Summary: An interactive visualizer of word embeddings *)
(* :Copyright: BF DM HY RM 2024 *)
(* :Package Version: 1 *)
(* :Mathematica Version: 13 *)
(* :History: last modified 27/3/2023 *)
(* :Keywords: embeddings, vectors, word2vec *)
(* :Limitations: this is for educational purposes only.  *)
(* :Requirements: *)
(* :Warning: package Context is not defined *)


BeginPackage["WordVec`"];


Main::usage = "Main[] main routine of the WordVec package";


Begin["`Private`"];


Main[]:= startUI[]


(* Auxiliary functions *)


startUI[]:= Module[{x}, Manipulate[x^2, {{x, 1, "x value"}, 1, 10, 1}]]


End[];


EndPackage[];
