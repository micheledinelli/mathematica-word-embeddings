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
(* :Limitations: this is for educational purposes only *)
(* :Requirements: *)


BeginPackage["WordVec`"];


Main::usage = "Main[] main routine of the WordVec package";


Begin["`Private`"];


(* Global variables *)
$net = NetModel["ConceptNet Numberbatch Word Vectors V17.06 (Raw Model)"];
$words = NetExtract[$net,"Input"][["Labels"]];
$vecs = Normal@NetExtract[$net, "Weights"];
$word2vec = AssociationThread[$words -> $vecs];


Main[] := createDemo[]


createDemo[] := DynamicModule[
    {words = Table[generateRandomWord[seed], {seed, 5}], 
    wordInput, 
    exerciseWord, 
    exerciseMode = False,
    hints = {}},

	infoAction := (
        MessageDialog[
            "This is a demo interface for exploring word embeddings.\n\n" <>
            "1. Type words in the input field and click 'Add' to add them to your list.\n\n" <>
            "2. Click 'Hints' to get hints for the current exercise word.\n\n" <>
            "3. Click 'Generate exercise' to select a random word from your list as the exercise word.\n\n" <>
            "4. Click 'Show solution' to display the solution (not implemented).\n\n" <>
            "5. Click 'Reset' to clear the word list and reset the interface."
        ]
    );

    (* User interface *)
    Panel[
        Column[{
            Row[{Button["Info", infoAction, BaseStyle -> {FontSize -> 14}]}],
            Spacer[20],
            Row[{Style["Enter a word to view its representation: ", 14]}, Alignment -> Center],
            Row[{
                InputField[Dynamic[wordInput], String, ContinuousAction -> True],
                Spacer[{5, 5}],
                Button["Add word", 
                    If[checkWord[wordInput],
                        AppendTo[words, wordInput];
                    ]; 
                    wordInput = ""]
            }],
            Spacer[10],
            Row[{
                Dynamic[Button["Hints", 
                    hints = getTopNNearest[exerciseWord, 3];
                    words = Join[words, hints],
                    Enabled -> exerciseMode === True]],
                Dynamic[Button["Generate exercise", 
                    exerciseWord = randomWordFromArray[words];
                    exerciseMode = True,
                    Enabled -> Length[words] > 0 && exerciseMode === False
                ]],
                Dynamic[Button["Show solution", 
                    Print[exerciseWord],
                    Enabled -> exerciseMode === True
                ]],
                Button["Reset", 
                    words = {};
                    exerciseWord = {};
                    hints = {};
                    exerciseMode = False;
                ]
            }],
            Spacer[10],
            Row[{
			  Style["Words: ", Bold, 12, Darker@Blue],
			  Dynamic[
			   Grid[Partition[
			     Tooltip[
			        Style[#, Bold, 12, "Hyperlink", Black],
			        WordData[#, "Definitions"]
			     ] & /@ words, 5, 5, {1, -1}, {}],
			     Spacings -> {1, 1}
			   ]
			  ]
			}],
            Spacer[10],
            Row[{Style["Hints: ", Bold, 12, Darker@Green], 
                Dynamic[Style[Row[hints, ", "], Bold, 12]]}],
            Dynamic@visualizeWordVectors3D[words, exerciseWord, exerciseMode]
        }]
    ]
]


visualizeWordVectors3D[words_, exerciseWord_, exerciseMode_: False] := Module[
    {pca3D, colors, embeddings, graphics},
	
	If[exerciseMode === False, 
		Return[showStandardPlot[words]],
		Return[showExercisePlot[words, exerciseWord]]
	];
]


showStandardPlot[words_] := Module[
    {pca3D, colors, embeddings, graphics},
    
    If[Length[words] == 0,
        Return[Graphics3D[{}, ImageSize -> Large]]
    ];
    
    (* Define colors for each word *)
    colors = ColorData[97] /@ Range[Length[words]];
    
    (* Get the embeddings *)
    embeddings = $word2vec /@ words;
    
    (* Perform PCA for dimensionality reduction to 3D *)
    pca3D = PrincipalComponents[embeddings][[All, 1 ;; 3]];
    
    (* Plot the vectors in a 3D space with arrows representing the embeddings *)
    graphics = Graphics3D[
        {
            MapThread[{Thick, #1, Arrow[{{0, 0, 0}, #2}]} &, {colors, pca3D}],
            MapThread[Text[Style[#1, 14], #2 + 0.1 Normalize[#2]] &, {words, pca3D}]
        },
        Axes -> True,
        AxesLabel -> {"PC1", "PC2", "PC3"},
        AxesStyle -> Directive[Black, Bold],
        ImageSize -> Large
    ];
    
    (* Return the graphics *)
    Return[graphics]
]


showExercisePlot[words_, exerciseWord_] := Module[
    {embeddings, exerciseEmbedding, wordLabels, exercisePoint, pca3D, index, 
    pca3DWithoutExWord, wordLabelsWithoutExWord, graphics},
    
    (* Get the embeddings for all words *)
    colors = ColorData[97] /@ Range[Length[words]];
    embeddings = Map[$word2vec, words];
    
    (* Get the embedding for the exercise word *)
    exerciseEmbedding = $word2vec[exerciseWord];
    
    (* Combine the embeddings with the exercise embedding *)
    embeddings = Append[embeddings, exerciseEmbedding];
    
    (* Perform PCA for dimensionality reduction to 3D *)
    pca3D = PrincipalComponents[embeddings][[All, 1 ;; 3]];
    
    (* Extract the word labels *)
    wordLabels = Append[words, exerciseWord];
    
    (* Find the index of the exerciseWord in wordLabels *)
    index = Position[wordLabels, exerciseWord][[1, 1]];
    
    (* Remove the corresponding element from pca3D *)
    pca3DWithoutExWord = Delete[pca3D, index];
    
    (* Remove the exerciseWord from wordLabels *)
    wordLabelsWithoutExWord = Delete[wordLabels, index];
    
    (* Plot the PCA-transformed embeddings and their corresponding words *)
    graphics = Graphics3D[{
        MapThread[{Thick, #1, Arrow[{{0, 0, 0}, #2}]} &, {colors, pca3DWithoutExWord}],
        MapThread[Text[Style[#1, 18], #2 + 0.1 Normalize[#2]] &, {wordLabelsWithoutExWord, pca3DWithoutExWord}],
        {Red, PointSize[Large], Point[pca3D[[index]]]}
        }, 
        Axes -> True, 
        AxesLabel -> {"PC1", "PC2", "PC3"}, 
        AxesStyle -> Directive[Black, Bold], 
        ImageSize -> Large
    ];
    graphics
]


(* Generates a random word that could be embedded, given a seed *)
(* IN: seed *)
generateRandomWord[seed_] := Module[
    {randomWord,randomList,n},

    (* BlockRandom temporarily changes the random number generation within its scope *)
    BlockRandom[
        (* SeedRandom sets the seed for random number generation *)
        SeedRandom[seed];
        
        (* RandomChoice selects a random word from the WordList[] *)
        (* + Check on embeddable words *)
        n=1;
        While[checkWord[randomWord, Verbose -> False]==False,randomList=RandomChoice[WordList["Noun"],n];randomWord=Part[randomList,n]];
    ];

    (* Convert the randomly chosen word to lowercase and return it *)
    ToLowerCase[randomWord]
]


(* Get a random word starting from an array with n words *)
(* IN: array *)
randomWordFromArray[array_] := Module[
   {seed, myWord},
   
   BlockRandom[
	   (* Creating the random seed by vector hashing *)
	   seed = Hash[array, "SHA256"];
	   SeedRandom[seed];
	   
	   (* Generate a word from the hash of the array *)
	   myWord = generateRandomWord[seed];
   ];
   
   (* Return the generated word *)
   myWord
]


(* Get the embedding of the random word generated from the word array *)
(* IN: array *)
embeddedWordFromArray[array_] := Module[
	{randomWordToEmbed},
	
	(* Create random word *)
	randomWordToEmbed = randomWordFromArray[array];
	
	(* Return embedded random word *)
	getEmbedding[randomWordToEmbed]
]


(* Get the vector representation or embedding for a given word *)
getEmbedding[word_] := Module[
    {wordLower, vector},

	(* Convert the word to lower case *)
	wordLower = ToLowerCase[word];
	
    (* Check if the word is suitable for being embedded then return the embedding o/w none *)
	If[checkWord[word], Return[$word2vec[wordLower]]; Return[None]];
]


checkWord[word_, OptionsPattern[{Verbose -> True}]] := Module[
    {wordLower, embeddingExists, verbose},

    (* Extract the value of the Verbose option *)
    verbose = OptionValue[Verbose];

    (* Check if the word is an empty string *)
    If[word === "", 
        If[verbose, MessageDialog["Please enter a non-empty word."]]; 
        Return[False]
    ]; 

    (* Convert the word to lower case *)
    wordLower = ToLowerCase[word];

    (* Check if the word has a valid embedding representation *)
    Quiet[
        Check[
            vector = $net[wordLower];
            embeddingExists = True,
            embeddingExists = False
        ]
    ];

    (* If the word does not have a vector representation, show a message and return false *)
    If[!embeddingExists, 
        If[verbose, MessageDialog["Please enter a common English word."]]; 
        Return[False]
    ];

    (* If the word is not in the built-in word list, show a message and return false *)
    (* WordList default call is a list of common English words *)
    If[!MemberQ[WordList["Noun"], wordLower], 
        If[verbose, MessageDialog["Please enter a common English word."]]; 
        Return[False]
    ];

    Return[True];
]


getTopNNearest[word_, n_] := Module[
    {wordLower, nNearest = {}},
    (* Converts a word to lower case *)
    wordLower = ToLowerCase[word];
    (* Check if the provided word is valid then return the n nearest words *)
    If[checkWord[word],
        nNearest = DeleteCases[Nearest[$word2vec, $word2vec[wordLower], n + 1], wordLower]; 
        (* Remove the input word if it's included *)
        Return[nNearest],
        None
    ]
]


End[];


EndPackage[];
