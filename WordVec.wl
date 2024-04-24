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


Main[] := Print["HELLO"];


createDemo[] := DynamicModule[
	{words = {}, embeddings = {}, wordInput, addWordAndEmbedding},
	
	addWordAndEmbedding[word_] := DynamicModule[
		{embedding}, 
		embedding = getEmbedding[word]; 
			If[embedding =!= None,
				AppendTo[words, word];
				AppendTo[embeddings, embedding];
			];
	];
	
	Grid[{
	  {
	    Column[{
	      "Choose a word",
	      Row[{
	        InputField[Dynamic[wordInput], String, ContinuousAction -> True],
	        Button["Add word", addWordAndEmbedding[wordInput]; wordInput = ""]
	      }]
	    }],
	    Column[{
	      "Actions",
	      Row[{
	        Button["Generate exercise", addWordAndEmbedding[wordInput]; wordInput = ""],
	        Button["Clear", addWordAndEmbedding[wordInput]; wordInput = ""]
	        Button["Show solution", addWordAndEmbedding[wordInput]; wordInput = ""]
	      }]
	    }]
	  },
	  {
	    Row[{
		  "Your words: ",
		  Dynamic[words]
	    }], 
	    SpanFromLeft
	  },
	  {
	    Dynamic[visualizeWordVectors2D[words, embeddings]], 
	    SpanFromLeft
	  }
	}, 
	Frame -> True, 
	ItemSize -> Automatic,
	Dividers -> All,
	Spacings -> {10, 2}
	]
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
        While[checkWord[randomWord]==False,randomList=RandomChoice[WordList[],n];randomWord=Part[randomList,n]];
    ];

    (* Convert the randomly chosen word to lowercase and return it *)
    ToLowerCase[randomWord]
]


visualizeWordVectors2D[words_, embeddings_] := Module[
  {pca2D, colors, graphics},

  If[Length[words] == 0 || Length[embeddings] == 0,
    Return[Graphics[{}, ImageSize -> Large]]
  ];

  (* Perform PCA for dimensionality reduction *)
  pca2D = PrincipalComponents[embeddings][[All, 1 ;; 2]];

  (* Define colors for each word *)
  colors = ColorData[97] /@ Range[Length[words]];

  (* Plot the word vectors in a 2D space *)
  graphics = Graphics[
    {
      PointSize[0.02],
      Transpose[{colors, Point /@ pca2D}],
      MapThread[Text[Style[#1, 14, #2], #3] &, {words, colors, pca2D}]
    },
    Axes -> True,
    AspectRatio -> 1,
    Frame -> True,
    FrameLabel -> {"PC1", "PC2"},
    ImageSize -> Large
  ];

  (* Return the graphics *)
  Return[graphics]
];


(* Calculate distance between two words based on their embeddings *)
distance[word1_, word2_] := Module[
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
            $net[wordLower],
            MessageDialog["The word \"" <> word <> "\" does not have a representation."];
            Return[None]
        ]
    ]
]


(* Get a random word starting from an array with n words *)
(* IN: array *)
randomWordFromArray[array_List] := Module[
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
embeddedWordFromArray[array_List] := Module[
	{randomWordToEmbed},
	
	(* Create random word *)
	randomWordToEmbed = randomWordFromArray[array];
	
	(* Return embedded random word *)
	getEmbedding[randomWordToEmbed]
]


(* Checks if a word is a common word in English and has valid embedding representation using Word2Vec *)
checkWord[word_]:= Module[
	{wordLower, embeddingExists},

	(*Check if the word is an empty string*)
	If[word==="", MessageDialog["Please enter a non-empty word."]; Return[False]]; 
	
	(* Convert the word to lower case *)
	wordLower = ToLowerCase[word];
	
	(* Check if the word as a valid embedding representation *)
	Quiet[Check[
		vector=net[wordLower];
		embeddingExists=True,
		embeddingExists=False
	]];
	
	(* If the word has not a vector representation shows a message and returns false *)
	If[!embeddingExists, MessageDialog["Please enter a common english word."]; Return[False]];
	
	(* If the word is not in the word list built-in shows a message and returns false *)
	(* WordList default call is a list of common english words *)
	If[!MemberQ[WordList[], wordLower], MessageDialog["Please enter a common english word."]; Return[False]];
	
	Return[True];
]


End[];


EndPackage[];
