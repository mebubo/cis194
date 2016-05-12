module Main where

import Editor
import JoinList
import Scrabble
import Sized
import Buffer

jl :: JoinList (Score, Size) String
jl = fromString $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]

main = runEditor editor jl
