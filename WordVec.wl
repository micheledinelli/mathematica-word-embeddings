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


BeginPackage["WordVec`"];


Main::usage = "Main[] main routine of the WordVec package";


Begin["`Private`"];


(* Global variables *)

(*Load the model network*)
$net = NetModel["ConceptNet Numberbatch Word Vectors V17.06 (Raw Model)"];


Main[]:= visualizeWordVectors2D[$net, {"hello","world","apple","banana"}]


(* Generates a random word given a seed *)
(* IN: seed *)
randomWord[seed_] := (
    (* BlockRandom temporarily changes the random number generation within its scope *)
    BlockRandom[
        (* SeedRandom sets the seed for random number generation *)
        SeedRandom[seed];
        (* RandomChoice selects a random word from the WordList[] *)
        RandomChoice[WordList[]]
    ]
)


(* Displays the list of input words in a three-dimensional space using PCA *)
(* IN: embedding network, list of words *)
visualizeWordVectors3D[network_, words_List] := Module[
    {vectors, pca3D, colors},
    (* Get vectors for the given words *)
    vectors = network[words];
    (* Perform PCA for dimensionality reduction *)
    pca3D = PrincipalComponents[vectors][[All, 1 ;; 3]];
    (* Define colors for each word *)
    colors = ColorData[97] /@ Range[Length[words]];
    (* Plot the vectors in a 3D space with swagger *)
    Graphics3D[
        {
            MapThread[{Thick, #1, Arrow[{{0, 0, 0}, #2}]} & , {colors, pca3D}],
            MapThread[Text[Style[#1, 14], #2 + 0.1 Normalize[#2]] & , {words, pca3D}]
        },
        Axes -> True,
        BoxRatios -> {1, 1, 1},
        AxesStyle -> Directive[Black, Bold],
        AxesLabel -> {"PC1", "PC2", "PC3"},
        ViewPoint -> {1.3, -2.4, 2},
        ImageSize -> Large,
        PlotRange -> All
    ]
]


(* Displays the list of input words in a bi-dimensional space using PCA *)
(* IN: embedding network, list of words *)
visualizeWordVectors2D[network_, words_List] := Module[
    {vectors, pca2D, colors},
    (* Get vectors for the given words *)
    vectors = network[words];
    (* Perform PCA for dimensionality reduction *)
    pca2D = PrincipalComponents[vectors][[All, 1 ;; 2]];
    (* Define colors for each word *)
    colors = ColorData[97] /@ Range[Length[words]];
    (* Plot the vectors in a 2D space with swagger *)
    Graphics[
        {
            MapThread[{Thick, #1, Arrow[{{0, 0}, #2}]} & , {colors, pca2D}],
            MapThread[Text[Style[#1, 14], #2 + 0.1 Normalize[#2]] & , {words, pca2D}]
        },
        Axes -> True,
        AspectRatio -> 1,
        PlotRange -> All,
        AxesStyle -> Directive[Black, Bold],
        AxesLabel -> {"PC1", "PC2"},
        ImageSize -> Large
    ]
]


End[];


EndPackage[];
