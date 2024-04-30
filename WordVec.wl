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


Main::usage = "Main[seed] main routine of the WordVec package.";


Begin["`Private`"];


(* Global variables *)
$net = NetModel["ConceptNet Numberbatch Word Vectors V17.06 (Raw Model)"];
$words = NetExtract[$net,"Input"][["Labels"]];
$vecs = Normal@NetExtract[$net, "Weights"];
$word2vec = AssociationThread[$words -> $vecs];


Main[] := createDemo[]


createDemo[] := DynamicModule[
    {
	    words = Table[generateRandomWord[seed], {seed, 42, 46}], 
	    wordInput,  
	    exerciseMode = False,
	    targetWord,
	    hints = {},
	    numberOfHints
    },

	infoAction := (
        MessageDialog[
            "This is a demo interface for exploring word embeddings.\n" <>
            "You'll see some words in the space, you can interact with the plot and see words are represented as vectors.\n" <>
			"You can add new words to the plot. Choose carefully because only common English nouns are accepted.\n\n" <>
            "- ADD A WORD TO PLOT: Type the word you would like to view in the first input field and then click ADD TO PLOT.\n\n" <>
            "- VIEW WORD MEANINGS: Under the plot you can find a list of words, hover a given word to discover its meaning.\n\n" <>
            "- GENERATE AN EXERCISE: When you are ready to test yourself click GENEREATE EXERCISE BUTTON.\n" <>
            "A red dot will appear. Try to guess which word is close to that point!\n\n" <>
            "- SHOW HINTS: You can view hints that will help you guessing the word.\n\n" <>
            "- RESTART: Click the restart button to restart the interface from where you started.\n\n" <>
            "- RESET: Clear all the words in the interface, this will end an ongoing exercise.\n"
        ]
    );
    
    (* User interface *)
    Panel[
        Grid[{
	        {Style["WORD EMBEDDINGS", FontSize -> 18], SpanFromLeft},
	        {Row[{Button["READ A SMALL GUIDE", infoAction], ""}], SpanFromLeft},
	        {
		        Row[{
	                InputField[Dynamic[wordInput], String, ContinuousAction -> True, FieldHint -> "Type a word", Enabled -> exerciseMode == False],
	                Dynamic[Button["ADD TO PLOT", 
	                    If[checkWord[wordInput],
						    If[!MemberQ[words, ToLowerCase[wordInput]],
						        AppendTo[words, ToLowerCase[wordInput]],
						        MessageDialog["The word is already in the list."]
						    ]
						];
	                    wordInput = "",
	                    Enabled -> exerciseMode == False
	                ]]
	            }], SpanFromLeft
	        },
	        {Dynamic@drawPlot[words, targetWord, exerciseMode], SpanFromLeft},
	        {Style["YOUR WORDS (HOVER FOR DEFINITIONS)", FontSize -> 12, Underlined], SpanFromLeft},
	        {Dynamic[Row[Tooltip[Style[ToUpperCase[#], Black, Bold, 14, "Hyperlink"], WordData[#, "Definitions"]] & /@ words, ", "]], SpanFromLeft},
	        {
		        Dynamic[Row[{
	                InputField[Dynamic[wordInput], String, ContinuousAction -> True, FieldHint -> "Try to guess", Enabled -> exerciseMode == True],
	                Dynamic[Button["GUESS", 
	                    If[checkWord[wordInput],
						    If[!MemberQ[words, ToLowerCase[wordInput]],
						        AppendTo[words, ToLowerCase[wordInput]];
						        checkGuess[targetWord, wordInput],
						        MessageDialog["The word is already in the list."]
						    ]
						]; 
	                    wordInput = "",
	                    Enabled -> exerciseMode == True
	                ]]
	            }]], SpanFromLeft
	        },
	        {
	            Grid[{
	                {
	                    Dynamic[Button["GENERATE EXERCISE", 
	                        targetWord = generateExercise[words];
	                        exerciseMode = True,
	                        Enabled -> Length[words] > 0 && exerciseMode == False
	                    ]],
	                    Dynamic[Button["SHOW HINTS", 
	                        hints = generateHints[targetWord, 2];
	                        words = Join[words, hints],
	                        Enabled -> exerciseMode == True
	                    ]]
	                },
					{Style["YOUR HINTS (HOVER FOR DEFINITIONS)", FontSize -> 12, Underlined], SpanFromLeft},
	                {Dynamic[Row[Tooltip[Style[ToUpperCase[#], Black, Bold, 14, "Hyperlink"], WordData[#, "Definitions"]] & /@ hints, ", "]], SpanFromLeft},						
	                {
	                    Dynamic[Button["SHOW SOLUTION", 
	                        MessageDialog["SOLUTION WAS ", targetWord];
	                        AppendTo[words, targetWord],
	                        Enabled -> exerciseMode == True
	                    ]], SpanFromLeft
	                },
	                {
	                    Dynamic[Button["RESTART", 
	                        words = Table[generateRandomWord[seed], {seed, 42, 46}]; 
		                    exerciseMode = False; 
		                    hints = {}; 
		                    targetWord,
		                    BaseStyle -> {Background -> LightRed, FontSize -> 12}
	                    ]], 
	                    Button["CLEAR ALL",
		                    words = {}; 
		                    exerciseMode = False; 
		                    hints = {}; 
		                    targetWord,
		                    BaseStyle -> {Background -> LightRed, FontSize -> 12}
		                ]
	                },
	                {
	                    Button["EXPORT PLOT", 
	                        Export["embeddings.jpg", plotEmbeddings[words]],
	                        Enabled -> Length[words] > 0], 
	                    SpanFromLeft
	                }
	            },
	            Spacings -> {Automatic, 2}
	            ], SpanFromLeft
	        }
        },
        ItemSize -> 30,
        Alignment -> Center,
        Spacings -> {Automatic, 2}
        ]
    ]
]


drawPlot[words_, targetWord_, exerciseMode_: False] :=
	If[exerciseMode === False, 
		Return[plotEmbeddings[words]],
		Return[plotExercise[words, targetWord]]
	];


plotEmbeddings[words_] := Module[
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


generateExercise[words_] := randomWordFromArray[words]


generateHints[targetWord_, n_] := getTopNNearest[targetWord, n]


checkGuess[targetWord_, wordIn_] := Module[
    {targetLower, wordInLower, dst, dstMessage},
    targetLower = ToLowerCase[targetWord];
    wordInLower = ToLowerCase[wordIn];
    
    dst = EuclideanDistance[$word2vec[targetWord], $word2vec[wordIn]];
    dstMessage = StringTemplate["`a` is `dst` far from the target using Euclidean Distance"][<|"a" -> wordIn, "dst" -> dst|>];
    
    Which[
        dst < 0.5, MessageDialog["Hurray! You WON!\n" <> dstMessage],
        0.5 <= dst <= 1, MessageDialog["You're close!\n" <> dstMessage],
        1 <= dst <= 1.5, MessageDialog["You're getting there!\n" <> dstMessage],
        dst > 2, MessageDialog["You can do better.\n" <> dstMessage],
        True, MessageDialog["The distance is undefined."]
    ]
]


(* Generates a random word that could be embedded, given a seed *)
(* IN: seed *)
generateRandomWord[seed_] := Module[
    {randomWord, randomList, n},

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


randomWordFromWords[words_, n_] := RandomSample[words, n]


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


plotExercise[words_, exerciseWord_] := Module[
	{embeddings, exerciseEmbedding, wordLabels, exercisePoint, pca3D, index, 
    pca3DWithoutExWord, wordLabelsWithoutExWord, graphics, colors},
  
	  (* Get the embeddings for all words *)
	  colors = ColorData[97] /@ Range[Length[words]];
	  embeddings = Map[$word2vec, words];
	  
	  (* Get the embedding for the exercise word *)
	  exerciseEmbedding = $word2vec[exerciseWord];
	  
	  (* Combine the embeddings with the exercise embedding *)
	  embeddings = Append[embeddings, exerciseEmbedding];
	  
	  (* Perform PCA for dimensionality reduction *)
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
	      MapThread[Text[Style[#1, 14], #2 + 0.1 Normalize[#2]] &, {wordLabelsWithoutExWord, pca3DWithoutExWord}],
	      {Red, PointSize[0.025], Point[pca3D[[index]]]}
	    }, 
	    Axes -> True, 
	    AxesLabel -> {"PC1", "PC2", "PC3"}, 
	    BoxRatios -> {1, 1, 1}, 
	    AxesStyle -> Directive[Black, Bold], 
	    ImageSize -> Large
	  ];
	  graphics
]


End[];


EndPackage[];
