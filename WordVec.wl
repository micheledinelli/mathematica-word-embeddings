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


Main[]:= visualizeWordVectors3D[$net, {"asgsdfgdsgfdsg", "michele", "ciao"}]


(* Generates a random word given a seed *)
(* IN: seed *)
randomWord[seed_] := Module[
    {random},

    (* BlockRandom temporarily changes the random number generation within its scope *)
    BlockRandom[
        (* SeedRandom sets the seed for random number generation *)
        SeedRandom[seed];
        (* RandomChoice selects a random word from the WordList[] *)
        random = RandomChoice[WordList[]];
    ];

    (* Convert the randomly chosen word to lowercase and return it *)
    ToLowerCase[random]
]


(* Displays the list of input words in a three-dimensional space using PCA *)
(* IN: embedding network, list of words *)
visualizeWordVectors3D[net_, words_] := Module[
    {vectors, pca3D, colors, wordsWithEmbeddings},
    
    (* Filter out words for which the network cannot produce embeddings *)
    wordsWithEmbeddings = Select[ToLowerCase[words], checkWordNetQ];
    
    (* Check if there are words with embeddings *)
    If[Length[wordsWithEmbeddings] == 0,
        Print["No embeddings found for any of the input words."];
        Return[]
    ];
    
    (* Get vectors for the words with embeddings *)
    vectors = net[wordsWithEmbeddings];
    
    (* Perform PCA for dimensionality reduction *)
    pca3D = PrincipalComponents[vectors][[All, 1 ;; 3]];
    
    (* Define colors for each word *)
    colors = ColorData[97] /@ Range[Length[wordsWithEmbeddings]];
    (* Plot the vectors in a 3D space with swagger *)
    Graphics3D[
        {
            MapThread[{Thick, #1, Arrow[{{0, 0, 0}, #2}]} & , {colors, pca3D}],
            MapThread[Text[Style[#1, 14], #2 + 0.1 Normalize[#2]] & , {wordsWithEmbeddings, pca3D}]
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
visualizeWordVectors2D[net_, words_] := Module[
    {vectors, pca2D, colors, wordsWithEmbeddings},
    
    (* Filter out words for which the network cannot produce embeddings *)
    wordsWithEmbeddings = Select[ToLowerCase[words], checkWordNetQ];
    
    (* Check if there are words with embeddings *)
    If[Length[wordsWithEmbeddings] == 0,
        Print["No embeddings found for any of the input words."];
        Return[]
    ];
    
    (* Get vectors for the words with embeddings *)
    vectors = net[wordsWithEmbeddings];
    
    (* Perform PCA for dimensionality reduction *)
    pca2D = PrincipalComponents[vectors][[All, 1 ;; 2]];
    
    (* Define colors for each word *)
    colors = ColorData[97] /@ Range[Length[wordsWithEmbeddings]];
    
    (* Plot the vectors in a 2D space with swagger *)
    Graphics[
        {
            MapThread[{Thick, #1, Arrow[{{0, 0}, #2}]} & , {colors, pca2D}],
            MapThread[Text[Style[#1, 14], #2 + 0.1 Normalize[#2]] & , {wordsWithEmbeddings, pca2D}]
        },
        Axes -> True,
        AspectRatio -> 1,
        PlotRange -> All,
        AxesStyle -> Directive[Black, Bold],
        AxesLabel -> {"PC1", "PC2"},
        ImageSize -> Large
    ]
]


(* Check whether the net can produce embeddings for a given word *)
(* IN: word *)
checkWordNetQ[word_] := Module[
    {wordLower, vectors, exists},

    (* Convert the input word to lowercase *)
    wordLower = ToLowerCase[word];

    (* Try querying the network *)
    Quiet[Check[
        vectors = $net[wordLower];
        exists = True,
        exists = False
    ]];

    (* Return whether the vectors exist *)
    exists
]


End[];


EndPackage[];
