(* ::Package:: *)

(* ::Package:: *)
(* :Title: WordVec *)
(* :Context: WordVec` *)
(* :Authors: FB, MD, YH, MR *)


BeginPackage["WordVec`"];


Main::usage = "Main[] main routine of the WordVec package";


Begin["`Private`"];


Main[]:= loadModel[];


(* Auxiliary functions *)


(* Load the Word2Vec model *)
loadModel[] = Module[{net, words, vecs, word2vec},
    	net = NetModel["GloVe 50-Dimensional Word Vectors Trained on Wikipedia and Gigaword 5 Data"];
    	words = NetExtract[net, "Input"][["Tokens"]];
    	vecs = Normal@NetExtract[net, "Weights"][[1 ;; -2]];
    	word2vec = AssociationThread[words -> vecs];
        (*Nearest[word2vec, word2vec["king"], 8]*)
    ];

(* Check whether a string is a valid word *)
isValidWord[str_, lang_: "English"] := DictionaryWordQ[str, Language -> lang];

(* Check wether a piece of text is valid *)
isValidText[text_String, lang_: "English"] := AllTrue[DeleteStopwords@TextWords[text], isValidWord[#, lang] &];


End[];


EndPackage[];
