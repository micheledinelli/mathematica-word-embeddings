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
Distance::usage = "Distance[word1, word2] returns the euclidian distance between the embeddings of the two words";


Begin["`Private`"];


(* Global variables *)

(*Load the model network*)
$net = NetModel["ConceptNet Numberbatch Word Vectors V17.06 (Raw Model)"];


Main[]:= Module[{words, embeddings}, 
	words = {"hello", "how", "are", "goodbye", "cow", "chicken"};
	embeddings = getEmbedding /@ words;
	visualizeWordVectors3D[words, embeddings]]


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


visualizeWordVectors3D[words_, embeddings_] := Module[
    {pca3D, colors, wordsWithEmbeddings, distanceVector},

    (* Perform PCA for dimensionality reduction *)
    pca3D = PrincipalComponents[embeddings][[All, 1 ;; 3]];

    (* Define colors for each word *)
    colors = ColorData[97] /@ Range[Length[words]];

    (* Plot the vectors in a 3D space with arrows representing the embeddings *)
    graphics = Graphics3D[
        {
            MapThread[{Thick, #1, Arrow[{{0, 0, 0}, #2}]} &, {colors, pca3D}],
            MapThread[Text[Style[#1, 14], #2 + 0.1 Normalize[#2]] &, {words, pca3D}]
        },
        Axes -> True,
        BoxRatios -> {1, 1, 1},
        AxesStyle -> Directive[Black, Bold],
        AxesLabel -> {"PC1", "PC2", "PC3"},
        ViewPoint -> {1.3, -2.4, 2},
        ImageSize -> Large,
        PlotRange -> All
    ];
    
    (* Return the graphics *)
    graphics
]


visualizeWordVectors2D[words_, embeddings_] := Module[
    {pca2D, colors, distanceVector},

    (* Perform PCA for dimensionality reduction *)
    pca2D = PrincipalComponents[embeddings][[All, 1 ;; 2]];

    (* Define colors for each word *)
    colors = ColorData[97] /@ Range[Length[words]];

    (* Plot the vectors in a 2D space with arrows representing the embeddings *)
    graphics = Graphics[
        {
            MapThread[{Thick, #1, Arrow[{{0, 0}, #2}]} &, {colors, pca2D}],
            MapThread[Text[Style[#1, 14], #2 + 0.1 Normalize[#2]] &, {words, pca2D}]
        },
        Axes -> True,
        AspectRatio -> 1,
        AxesStyle -> Directive[Black, Bold],
        AxesLabel -> {"PC1", "PC2"},
        ImageSize -> Large
    ];
    (* Return the graphics *)
    graphics
]


(* Calculate distance between two words based on their embeddings *)
Distance[word1_, word2_] := Module[
    {embOne, embTwo},

    (* Get embeddings for the words *)
    embOne = getEmbedding[word1];
    embTwo = getEmbedding[word2];
    
    visualizeWordVectors2D[{word1, word2}, {embOne, embTwo}, PlotDistance -> True]

    (* Check if embeddings are valid *)
    If[embOne === None || embTwo === None,
        Return[None], (* Return None if either embedding is None *)
        Return[Norm[embOne - embTwo]] (* Calculate Euclidean distance *)
    ]
]


(* Get embedding for a given word *)
(* IN: word *)
getEmbedding[word_] := Module[
    {wordLower, vector},

    (* Convert the input word to lowercase *)
    wordLower = ToLowerCase[word];

    (* Try querying the network *)
    Quiet[
        Check[
            vector = $net[wordLower];
            Return[vector], (* Return the vector if it exists *)
            Return[None]    (* Return None if an error occurs or vector doesn't exist *)
        ]
    ]
]


(* Check whether the net can produce embedding for a given word *)
(* IN: word *)
checkWordNetQ[word_] := Module[
    {wordLower, vector, exists},

    (* Convert the input word to lowercase *)
    wordLower = ToLowerCase[word];

    (* Try querying the network *)
    Quiet[Check[
        vector = $net[wordLower];
        exists = True,
        exists = False
    ]];

    (* Return whether the vectors exist *)
    exists
]


End[];


EndPackage[];
