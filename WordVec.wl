(* ::Package:: *)

(* :Title: WordVec *)
(* :Context: WordVec` *)
(* :Author: BF DM HY RM *)
(* :Summary: An interactive visualizer of word embeddings *)
(* :Copyright: BF DM HY RM 2024 *)
(* :Package Version: 1 *)
(* :Mathematica Version: 14.0.0.0 *)
(* :History: last modified 11/05/2024 *)
(* :Keywords: embeddings, vectors, word2vec *)
(* :Limitations: this is for educational purposes only *)
(* :Requirements: *)


BeginPackage["WordVec`"];


Main::usage = "Main[] main routine of the WordVec package.";


Begin["`Private`"];


(* Global variables *)
net = NetModel["ConceptNet Numberbatch Word Vectors V17.06 (Raw Model)"];
words = NetExtract[net,"Input"][["Labels"]];
vecs = Normal@NetExtract[net, "Weights"];
word2vec = AssociationThread[words -> vecs]; 
fontSize = 12;
randomSeed = 83;
viewPoint = {1,1,1}


Main[] := createDemo[]


createDemo[] := DynamicModule[
    {
	    words = Table[generateRandomWord[seed], {seed, randomSeed, randomSeed + 4}], 
	    wordInput,  
	    exerciseMode = False,
	    targetWord,
	    hints = {},
	    numHints = 0,
	    exerciseFinished = False
    },

	infoAction := MessageDialog[Text[
            "This interface allows you to experiment with word embeddings.\n" <>
            "You will notice various words displayed in the space. You can interact with the plot to observe how these words are represented as vectors.\n" <>
			"You can add new words to the plot. Choose carefully because only common English nouns are accepted.\n\n" <>
            "- ADD A WORD TO PLOT: Type the word you would like to view in the first input field and then click ADD TO PLOT.\n\n" <>
            "- VIEW WORD MEANINGS: Below the plot, you will find a list of words. Hover over a word to discover its meaning.\n\n" <>
            "- GENERATE AN EXERCISE: When you are ready to test, click the GENERATE EXERCISE button.\n" <>
            "A red dot will appear. Try to guess which word is close to that point!\n\n" <>
            "- SHOW HINTS: You can view hints that will help you guess the word.\n\n" <>
            "- RESTART: Click the restart button to reset the interface to its initial state.\n\n" <>
            "- RESET: Clear all the words from the interface. This will end any ongoing exercise.\n", 
            BaseStyle->{FontSize->14}]
        ];
    
    (* User interface *)
    
    (* Buttons and user actions will be disabled until they can be triggered according to the game's business logic *)
    Panel[
        (* The graphic elements are placed on a Grid *)
        Grid[{
            (* Title *)
	        {Style["WORD EMBEDDINGS", FontSize -> fontSize + 6], SpanFromLeft}, 
	        (* Tutorial button *)
	        {
		        Row[{Button["READ A SMALL GUIDE", infoAction], "" }],
	            SpanFromLeft
	        },
	        (* Input word row *)
	        {
		        Row[{
		            (* The user can use this InputField to insert a new word, which must be a noun according to WordList["Noun"] *)
	                InputField[Dynamic[wordInput], String, ContinuousAction -> True, FieldHint -> "Type a word", Enabled -> exerciseMode == False],
	                (* Button that allows to add the embedded word to the plot *)
	                Dynamic[Button["ADD TO PLOT", 
	                    (* If the user's input word is valid and not already in the list of words, it will be added to the list *)
	                    (* Otherwise a message is produced *)
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
	        (* Plot cell *)
	        {Dynamic@drawPlot[words, targetWord, exerciseMode, exerciseFinished], SpanFromLeft},
	        {Style["YOUR WORDS (HOVER FOR DEFINITIONS)", FontSize -> fontSize, Underlined], SpanFromLeft},
	        (* Generate tooltip for the word list using WordData *)
	        {Dynamic[Row[Tooltip[Style[ToUpperCase[#], Black, Bold, 14, "Hyperlink"], WordData[#, "Definitions"]] & /@ words, ", "]], SpanFromLeft},
	        
	        {
	            (* The graphical components are positioned within a grid layout *)
	            Grid[{
	                {
	                    (* Button that allows you to create an exercise using the input words *)
	                    Dynamic[Button["GENERATE EXERCISE", 
	                        (* The input words are linked to a word that needs to be guessed *)
	                        targetWord = generateExercise[words];
	                        exerciseMode = True,
	                        Enabled -> Length[words] > 0 && exerciseMode == False
	                    ]], SpanFromLeft
	                },
	                {
		        Dynamic[Row[{
	                (* The user can use this InputField to attempt to guess the target word *)
	                InputField[Dynamic[wordInput], String, ContinuousAction -> True, FieldHint -> "Try to guess", Enabled -> exerciseMode == True],
	                Dynamic[Button["GUESS", 
	                    If[checkWord[wordInput],
						    If[!MemberQ[words, ToLowerCase[wordInput]],
						        AppendTo[words, ToLowerCase[wordInput]];
						        (* Updating game state: checkGuess returns whether the user has won or not *)
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
						Style["NUMBER OF HINTS: ", FontSize -> fontSize], SpanFromLeft
					},
					{
					    (* Hints are generated starting from the target word *)
					    (* The number of hints displayed dynamically changes based on which button the user clicks (1, 2, 3) *)
						Column[{
							Dynamic[SetterBar[Dynamic[numHints, (numHints = #;
							        hints = generateHints[targetWord, numHints];
							        (* Combining words and hints together in two steps.*)
							        words = Complement[words, hints];
							        words = Join[words, hints]) &],
							        {0, 1, 2, 3}, 
							        Enabled -> exerciseMode == True && !exerciseFinished]]
						}], SpanFromLeft
					},
					{
						Style["YOUR HINTS (HOVER FOR DEFINITIONS)", FontSize -> fontSize, Underlined], SpanFromLeft
					},
					{
					    (* Tooltip that shows the hints generated *)
						Dynamic[Row[Tooltip[Style[ToUpperCase[#], Black, Bold, 14, "Hyperlink"], WordData[#, "Definitions"]] & /@ hints, ", "]], SpanFromLeft
					},
	                {
	                    (* Button to display a message dialog with the solution (the word to guess) *)
	                    Dynamic[Button["SHOW SOLUTION", 
	                        MessageDialog["SOLUTION WAS ", targetWord];
	                        AppendTo[words, targetWord];
	                        exerciseFinished = True;
	                        MessageDialog["Solution was " <> targetWord],
	                        Enabled -> exerciseMode == True && !exerciseFinished
	                    ]], SpanFromLeft
	                },
	                {
	                    (* Restart the excercise *)
	                    Dynamic[Button["RESTART", 
	                        spawnSeed = RandomInteger[100]; (* This variable cannot be both global and parameterized; otherwise, the restart generation won't work correctly. The same words would always be generated.*)
	                        words = Table[generateRandomWord[seed], {seed, spawnSeed, spawnSeed + 4}]; 
		                    exerciseMode = False; 
		                    exerciseFinished = False;
		                    numHints = 0;
		                    hints = {}; 
		                    targetWord,
		                    BaseStyle -> {Background -> LightGreen, FontSize -> fontSize}
	                    ]], 
	                    (* Clear the entire interface and the plot *)
	                    Button["CLEAR ALL",
		                    words = {}; 
		                    exerciseMode = False; 
		                    hints = {};
		                    numHints = 0; 
		                    exerciseFinished = False;
		                    targetWord,
		                    BaseStyle -> {Background -> LightRed, FontSize -> fontSize}
		                ]
	                },
	                {
	                    (* This button exports the plot image along with the ordered list of inserted words *)
	                    Button["EXPORT PLOT", 
	                        Export["embeddings.jpg", plotEmbeddings[words, ExportMode -> True]],
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
   This function decides whether to draw an exercise plot or a standard plot of embeddings,
    depending on the value of the exerciseMode flag
   IN:
	   - words: a list of words
	   - targetWord: a word to guess
	   - exerciseMode: a flag indicating whether the current mode is exercise or standard
   OUT:
       - A plot depicting word embeddings graphically
*)
drawPlot[words_, targetWord_, exerciseMode_: False, exerciseFinished_: False] := Module[
    (* Define a local variable to determine which plot to draw. *)
    {plotFlag},
    
    (* Calculate the plotFlag based on exerciseMode and exerciseFinished status *)
    plotFlag = (!exerciseMode && !exerciseFinished) || (exerciseMode && exerciseFinished);
    
    (* Check the value of plotFlag to determine which plot to draw based on its current setting *)
    If[plotFlag, 
        (* If plotFlag is True, draw the standard embeddings plot *)
        plotEmbeddings[words],
        (* If plotFlag is False, draw the exercise plot *)
        plotExercise[words, targetWord]
    ]
]


(*
   The plot visualizes word embeddings in a 3D space using PCA for dimensionality reduction
   IN:
	   - words: a list of words
*)
plotEmbeddings[words_, OptionsPattern[{ExportMode -> False}]] := Module[
    {pca3D, colors, embeddings, graphics, exportMode, wordRows, textRows, graphicsWithText},
    
    exportMode = OptionValue[ExportMode];
    
    (* If there are no words, return an empty 3D graphic *)
    If[Length[words] == 0,
        Return[Graphics3D[{}, ImageSize -> Large]]
    ];
    
    (* Define colors for each word *)
    colors = ColorData[97] /@ Range[Length[words]];
    
    (* Retrieve the embeddings for the words *)
    embeddings = word2vec /@ words;
    
    (* Perform PCA for dimensionality reduction to achieve a 3D representation *)
    pca3D = PrincipalComponents[embeddings][[All, 1 ;; 3]];
    
    (* The plot visualizes vectors in a 3D space using arrows to represent the embeddings *)
    graphics = Graphics3D[
        {
            MapThread[{Thick, #1, Arrow[{{0, 0, 0}, #2}]} &, {colors, pca3D}], (* Plot arrows for each embedding *)
            MapThread[Text[Style[#1, 14], #2 + 0.5 Normalize[#2]] &, {words, pca3D}] (* Label each embedding *)
        },
        Axes -> True, (* Show axes *)
        AxesLabel -> {"PC1", "PC2", "PC3"}, (* Label axes *)
        AxesStyle -> Directive[Black, Bold], (* Style the axes *)
        ViewPoint -> Dynamic[viewPoint],
        ImageSize -> Large (* Set image size *)
    ];
    
	If[exportMode,
	    (* Split the list of words into rows, each containing up to 5 words *)
	    wordRows = Partition[words, 5];
	
	    (* Create rows to place them beneath the 3D plot *)
	    textRows = Map[GraphicsRow[Text[Style[#, 8]] & /@ #] &, wordRows];
	    
	    (* Integrate the 3D plot with the text rows *)
	    graphicsWithText = GraphicsColumn[{graphics, Sequence @@ textRows}];
	    Return[graphicsWithText]
	];

	graphics = Show[graphics, ImagePadding -> {{50, 50}, {50, 50}}];
    (* Return the graphics *)
    Return[graphics]
]


generateExercise[words_] := randomWordFromArray[words]


generateHints[targetWord_, n_] := getTopNNearest[targetWord, n]


(* 
   Checks if a user's guess is close enough to the target word and provides feedback on the attempt. 
   Returns whether the user has won by checking if the distance is less than a fixed amount (0.5)
   
   IN:
	   - targetWord: the target word
	   - wordIn: user's guessed word
*)
checkGuess[targetWord_, wordIn_] := Module[
    {targetLower, wordInLower, dst, dstMessage},
    
    (* Convert both the target word and the user input to lowercase letters *)
    targetLower = ToLowerCase[targetWord];
    wordInLower = ToLowerCase[wordIn];
    
    (* Compute the Euclidean distance between the vector of the target word and the vector of the user input word *)
    dst = EuclideanDistance[word2vec[targetWord], word2vec[wordIn]];
    
    (* Generate a message template to show the calculated distance *)
    dstMessage = StringTemplate["`a` is `dst` far from the target using Euclidean Distance"][<|"a" -> wordIn, "dst" -> dst|>];
    
    (* Display an appropriate feedback message based on the calculated distance *)
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
	Generate a random word suitable for embedding based on a given seed
	IN:
	    - seed: a number used to initialize a pseudorandom state
*)
generateRandomWord[seed_] := Module[
    {randomWord, randomList, n},

    (* BlockRandom temporarily modifies the random number generation behavior within its scope *)
    BlockRandom[
        (* SeedRandom initializes the seed for random number generation *)
        SeedRandom[seed];
        
        (* RandomChoice selects a random word from a specified WordList[] *)
        (* + Verify which words are suitable for embedding *)
        n=1;
        While[checkWord[randomWord, Verbose -> False]==False,randomList=RandomChoice[WordList["Noun"],n];randomWord=Part[randomList,n]];
    ];

    (* Convert the randomly chosen word to lowercase and then return it *)
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
	   (* Generate a random seed using vector hashing *)
	   seed = Hash[array, "SHA256"];
	   SeedRandom[seed];
	   
	   (* Generate a word using the hash of the array *)
	   myWord = generateRandomWord[seed];
   ];
   
   (* Return the generated word *)
   myWord
]


(* 
	Retrieve the embedding of the randomly generated word from the word array 
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
	Retrieve the vector representation or embedding of a specified word
	IN:
		- word: a word
*)
getEmbedding[word_] := Module[
    {wordLower, vector},

	(* Convert the word to lower case *)
	wordLower = ToLowerCase[word];
	
    (* Verify if the word is suitable for embedding; if yes, return its embedding; otherwise, return 'None' *)
	If[checkWord[word], Return[word2vec[wordLower]]; Return[None]];
]


(* 
	Validate whether a given word is valid according to the package's business logic.
	 Optionally, enable verbosity to generate messages
	IN:
		- word: a word
		- options: {Verbose: Boolean}
*)
checkWord[word_, OptionsPattern[{Verbose -> True}]] := Module[
    {wordLower, embeddingExists, verbose},

    (* Extract the value of the Verbose option *)
    verbose = OptionValue[Verbose];

   \.14 (* Check if the word is an empty string *)
    If[!embeddingExists && verbose, MessageDialog["Please enter a common English Noun"]; Return[False]];

    (* Convert the word to lower case *)
    wordLower = ToLowerCase[word];

    (* Check if the word has a valid embedding representation *)
    Quiet[
        Check[
            vector = net[wordLower];
            embeddingExists = True,
            embeddingExists = False
        ]
    ];

    (* If the word does not have a vector representation, display a message and return false *)
    If[!embeddingExists && verbose, MessageDialog["Please enter a common English Noun"]; Return[False]];

    (* If the word is not found in the built-in word list, display a message and return false *)
    (* WordList default call is a list of common English words *)
    (* 
       When using 'Noun' as the type, adjectives and verbs may also be included, as some words, such as 'good', 
       can operate as both an adjective and a noun ('my goods'), highlighting the issue of homonyms.
    *)
    If[!MemberQ[WordList["Noun"], wordLower], 
        If[verbose, MessageDialog["Please enter a common English Noun"]]; 
        Return[False]
    ];

    Return[True];
]


(* 
	Retrieve the n nearest words to a given word based on their vector similarities 
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
		(* Remove the input word if it's included *)
        nNearest = DeleteCases[Nearest[word2vec, word2vec[wordLower], n + 1], wordLower]; 
        Return[nNearest],
        None
    ]
]


(* 
	Visualize word embeddings and an exercise word represented as an anonymous point on the plot
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
    
    (* Transpose the list of labels with the corresponding PCA-transformed embeddings *)
    (* This operation is performed to create a list where each element consists of a label paired with its corresponding adjusted position in the 3D space *)
    Transpose[{labels, pca3D + labelOffsets}]
]

(* 
	Plot the input words using their embeddings and represent the exerciseWord as a red dot,
	 labeling it with the first and last words of the word it represents 
	IN:
		- words: a list of words
		- exerciseWord: a word to be represented as a red dot (to be guessed by the user)
*)
plotExercise[words_, exerciseWord_] := Module[
    {embeddings, exerciseEmbedding, wordLabels, exercisePoint, pca3D, index, 
    pca3DWithoutExWord, wordLabelsWithoutExWord, graphics, colors, label, adjustedLabels},
  
    (* Retrieve the embeddings for all words *)
    colors = ColorData[97] /@ Range[Length[words]];
    embeddings = Map[word2vec, words];
    
    (* Retrieve the embedding for the exercise word *)
    exerciseEmbedding = word2vec[exerciseWord];
    
    (* Merge the embeddings with the exercise embedding *)
    embeddings = Append[embeddings, exerciseEmbedding];
    
    (* Perform PCA for dimensionality reduction *)
    pca3D = PrincipalComponents[embeddings][[All, 1 ;; 3]];
    
    (* Retrieve the word labels *)
    wordLabels = Append[words, exerciseWord];
    
    (* Determine the index of the exerciseWord in the wordLabels list *)
    index = Position[wordLabels, exerciseWord][[1, 1]];
    
    (* Remove the corresponding element from pca3D *)
    pca3DWithoutExWord = Delete[pca3D, index];
    
    (* Remove the exerciseWord from wordLabels *)
    wordLabelsWithoutExWord = Delete[wordLabels, index];
    
    (* Create a label for the exercise word by retaining the last two characters and replacing the middle characters with dots *)
    label = StringTake[exerciseWord, {1, 1}] <> StringRepeat[".", Max[0, StringLength[exerciseWord] - 2]] <> StringTake[exerciseWord, {-1, -1}];
    
    (* Modify label positions to avoid overlapping *)
    adjustedLabels = adjustLabelPositions[pca3DWithoutExWord, wordLabelsWithoutExWord, 0.1];
    
    (* Visualize the PCA-transformed embeddings along with their corresponding words on a plot *)
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
    
    graphics = Show[graphics, ImagePadding -> {{50, 50}, {50, 50}}];
    graphics
]


End[];


EndPackage[];
