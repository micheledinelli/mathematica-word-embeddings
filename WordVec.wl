(* ::Package:: *)

(* :Title: WordVec *)
(* :Context: WordVec` *)
(* :Author: BF DM HY RM *)
(* :Summary: An interactive visualizer of word embeddings *)
(* :Copyright: BF DM HY RM 2024 *)
(* :Package Version: 1 *)
(* :Mathematica Version: 13 *)
(* :History: last modified 1/05/2024 *)
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
        spawnSeed = 42,
	    words = Table[generateRandomWord[seed], {seed, 42, 42 + 4}], 
	    wordInput,  
	    exerciseMode = False,
	    targetWord,
	    hints = {},
	    numHints = 0,
	    exerciseFinished = False
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
	        {
		        Row[{Button["READ A SMALL GUIDE", infoAction], "" }],
	            SpanFromLeft
	        },
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
	        {Dynamic@drawPlot[words, targetWord, exerciseMode, exerciseFinished], SpanFromLeft},
	        {Style["YOUR WORDS (HOVER FOR DEFINITIONS)", FontSize -> 12, Underlined], SpanFromLeft},
	        {Dynamic[Row[Tooltip[Style[ToUpperCase[#], Black, Bold, 14, "Hyperlink"], WordData[#, "Definitions"]] & /@ words, ", "]], SpanFromLeft},
	        {
		        Dynamic[Row[{
	                InputField[Dynamic[wordInput], String, ContinuousAction -> True, FieldHint -> "Try to guess", Enabled -> exerciseMode == True],
	                Dynamic[Button["GUESS", 
	                    If[checkWord[wordInput],
						    If[!MemberQ[words, ToLowerCase[wordInput]],
						        AppendTo[words, ToLowerCase[wordInput]];
						        (* Updating game state *)
						        exerciseFinished = checkGuess[targetWord, wordInput],
						        MessageDialog["The word is already in the list."]
						    ]
						]; 
	                    wordInput = "",
	                    Enabled -> exerciseMode == True && !exerciseFinished
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
	                    ]], SpanFromLeft
	                },					
					{
						Style["NUMBER OF HINTS: ", FontSize -> 12], SpanFromLeft
					},
					{
						Column[{
							Dynamic[SetterBar[Dynamic[numHints, (numHints = #;
							        hints = generateHints[targetWord, numHints];
							        (* Joining words and hints toghether in two steps*)
							        words = Complement[words, hints];
							        words = Join[words, hints]) &],
							        {0, 1, 2, 3}, 
							        Enabled -> exerciseMode == True && !exerciseFinished]]
						}], SpanFromLeft
					},
					{
						Style["YOUR HINTS (HOVER FOR DEFINITIONS)", FontSize -> 12, Underlined], SpanFromLeft
					},
					{
						Dynamic[Row[Tooltip[Style[ToUpperCase[#], Black, Bold, 14, "Hyperlink"], WordData[#, "Definitions"]] & /@ hints, ", "]], SpanFromLeft
					},
	                {
	                    Dynamic[Button["SHOW SOLUTION", 
	                        MessageDialog["SOLUTION WAS ", targetWord];
	                        AppendTo[words, targetWord];
	                        exerciseFinished = True;
	                        MessageDialog["Solution was " <> targetWord],
	                        Enabled -> exerciseMode == True && !exerciseFinished
	                    ]], SpanFromLeft
	                },
	                {
	                    Dynamic[Button["RESTART", 
	                        spawnSeed = RandomInteger[100];
	                        words = Table[generateRandomWord[seed], {seed, spawnSeed, spawnSeed + 4}]; 
		                    exerciseMode = False; 
		                    exerciseFinished = False;
		                    hints = {}; 
		                    targetWord,
		                    BaseStyle -> {Background -> LightGreen, FontSize -> 12}
	                    ]], 
	                    Button["CLEAR ALL",
		                    words = {}; 
		                    exerciseMode = False; 
		                    hints = {}; 
		                    exerciseFinished = False;
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


(* 
   Chooses to draw either a standard plot of embeddings or an exercise plot,
   depending on the value of the flag exerciseMode. 
   IN:
	   - words: a list of words
	   - targetWord: a word to guess
	   - exerciseMode: a flag stating if the current mode is exercise or standard mode
   OUT:
       - a graphic representing word embeddings
*)
drawPlot[words_, targetWord_, exerciseMode_: False, exerciseFinished_: False] := Module[
    (* Define local variable to determine which plot to draw *)
    {plotFlag},
    
    (* Calculate the plotFlag based on exerciseMode and exerciseFinished *)
    plotFlag = (!exerciseMode && !exerciseFinished) || (exerciseMode && exerciseFinished);
    
    (* Check the value of plotFlag to determine which plot to draw *)
    If[plotFlag, 
        (* If plotFlag is True, draw standard embeddings plot *)
        plotEmbeddings[words],
        (* If plotFlag is False, draw exercise plot *)
        plotExercise[words, targetWord]
    ]
]


(*
   Plots the embeddings of a list of words in a 3D space.
   It uses PCA for dimensionality reduction to visualize the embeddings in 3D.
   IN:
	   - words: a list of words
*)
plotEmbeddings[words_] := Module[
    {pca3D, colors, embeddings, graphics},
    
    (* If there are no words, return an empty 3D graphics *)
    If[Length[words] == 0,
        Return[Graphics3D[{}, ImageSize -> Large]]
    ];
    
    (* Define colors for each word *)
    colors = ColorData[97] /@ Range[Length[words]];
    
    (* Get the embeddings for the words *)
    embeddings = $word2vec /@ words;
    
    (* Perform PCA for dimensionality reduction to 3D *)
    pca3D = PrincipalComponents[embeddings][[All, 1 ;; 3]];
    
    (* Plot the vectors in a 3D space with arrows representing the embeddings *)
    graphics = Graphics3D[
        {
            MapThread[{Thick, #1, Arrow[{{0, 0, 0}, #2}]} &, {colors, pca3D}], (* Plot arrows for each embedding *)
            MapThread[Text[Style[#1, 14], #2 + 0.5 Normalize[#2]] &, {words, pca3D}] (* Label each embedding *)
        },
        Axes -> True, (* Show axes *)
        AxesLabel -> {"PC1", "PC2", "PC3"}, (* Label axes *)
        AxesStyle -> Directive[Black, Bold], (* Style the axes *)
        ImageSize -> Large (* Set image size *)
    ];
    
    (* Return the graphics *)
    Return[graphics]
]



generateExercise[words_] := randomWordFromArray[words]


generateHints[targetWord_, n_] := getTopNNearest[targetWord, n]


(* 
   Checks if a user guess is close enough to the target word 
   and produces a message giving feedback on the attempt.
   Returns if the user won checking if the distance is less than a fixed amount (0.5).
   
   IN:
	   - targetWord: the target word
	   - wordIn: user's word guess
*)
checkGuess[targetWord_, wordIn_] := Module[
    {targetLower, wordInLower, dst, dstMessage},
    
    (* Convert both the target word and the user input to lowercase *)
    targetLower = ToLowerCase[targetWord];
    wordInLower = ToLowerCase[wordIn];
    
    (* Calculate the Euclidean distance between the target word's vector 
       and the user input word's vector *)
    dst = EuclideanDistance[$word2vec[targetWord], $word2vec[wordIn]];
    
    (* Create a message template to display the distance *)
    dstMessage = StringTemplate["`a` is `dst` far from the target using Euclidean Distance"][<|"a" -> wordIn, "dst" -> dst|>];
    
    (* Based on the distance, show an appropriate feedback message *)
    Which[
        dst < 0.5, MessageDialog["Hurray! You WON!\n" <> dstMessage],
        0.5 <= dst <= 1, MessageDialog["You're close!\n" <> dstMessage],
        1 <= dst <= 1.5, MessageDialog["You're getting there!\n" <> dstMessage],
        dst > 2, MessageDialog["You can do better.\n" <> dstMessage],
        True, MessageDialog["The distance is undefined."]
    ];
    
    Return[dst < 0.5];
]



(* 
	Generates a random word that could be embedded, given a seed 
	IN:
	    - seed: a number to start a pseudorandomic state
*)
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


(* 
	Get a random word given an array 
	IN:
	    - array: a list of words
*)
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


(* 
	Get the embedding of the random word generated from the word array 
	IN:
		- array: a list of words
*)
embeddedWordFromArray[array_] := Module[
	{randomWordToEmbed},
	
	(* Create random word *)
	randomWordToEmbed = randomWordFromArray[array];
	
	(* Return embedded random word *)
	getEmbedding[randomWordToEmbed]
]


(* 
	Get the vector representation or embedding for a given word 
	IN:
		- word: a word
*)
getEmbedding[word_] := Module[
    {wordLower, vector},

	(* Convert the word to lower case *)
	wordLower = ToLowerCase[word];
	
    (* Check if the word is suitable for being embedded then return the embedding o/w none *)
	If[checkWord[word], Return[$word2vec[wordLower]]; Return[None]];
]


(* 
	Checks if a given word is valid given the package business logic. 
	Accepts verbose as an option to eventually produce messages
	IN:
		- word: a word
		- options: {Verbose: Boolean}
*)
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


(* 
	Return the n nearest words for a given word 
	IN:
	    - word: a word
	    - n: number of neighbours to produce
*)
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


(* 
	Plot word embedding and an exercise word represented as a anonymous point in the plot 
	IN:
		- pca3D: matrix representing a dimensionality reduction
		- labels: list of words representing labels for points
		- minDistance: a minimum distance to keep labels away from each others
*)
adjustLabelPositions[pca3D_, labels_, minDistance_] := Module[{labelOffsets},
    labelOffsets = ConstantArray[{0, 0, 0}, Length[pca3D]];
    Do[
        If[Norm[pca3D[[i]] - pca3D[[j]]] < minDistance,
            labelOffsets[[i]] += Normalize[pca3D[[i]] - pca3D[[j]]] * minDistance;
        ],
        {i, Length[pca3D]},
        {j, i + 1, Length[pca3D]}
    ];
    Transpose[{labels, pca3D + labelOffsets}]
]

(* 
	Plots the words in input using their embeddings, and plots the exerciseWord 
	as a red dot labelling it with the first and last word of the word it represents. 
	IN:
		- words: a list of words
		- exerciseWord: a word to be represented as a red dot (to be guessed by the user)
*)
plotExercise[words_, exerciseWord_] := Module[
    {embeddings, exerciseEmbedding, wordLabels, exercisePoint, pca3D, index, 
    pca3DWithoutExWord, wordLabelsWithoutExWord, graphics, colors, label, adjustedLabels},
  
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
    
    (* Generate a label for the exercise word keeping the last two chars and replacing the middle ones with dots *)
    label = StringTake[exerciseWord, {1, 1}] <> StringRepeat[".", Max[0, StringLength[exerciseWord] - 2]] <> StringTake[exerciseWord, {-1, -1}];
    
    (* Adjust label positions to prevent overlap *)
    adjustedLabels = adjustLabelPositions[pca3DWithoutExWord, wordLabelsWithoutExWord, 0.1];
    
    (* Plot the PCA-transformed embeddings and their corresponding words *)
    graphics = Graphics3D[{
        MapThread[{Thick, #1, Arrow[{{0, 0, 0}, #2}]} &, {colors, pca3DWithoutExWord}],
        MapThread[Text[Style[#1, 14], #2] &, {adjustedLabels[[All, 1]], adjustedLabels[[All, 2]]}],
        {Red, PointSize[0.025], Point[pca3D[[index]]], Text[Style[label, 22, Bold], pca3D[[index]] + {0, 0, 0.2}, {0, -1}]}
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
