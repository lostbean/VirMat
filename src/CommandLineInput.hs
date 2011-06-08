
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module CommandLineInput (
GrainDistributionInput (targetNumber
                        , targetMeanVolume
                        , targetVariance
                        , anisotropyShapeRatio
                        , seed ),
GrainDistOutputFile (grainDistroOutFile),
ShowIn3D(..),
RandomSeed(..),
parseArgs
) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Perm
import Text.Parsec.Prim hiding (try)
import System.Posix
import Data.List
import Control.Monad.Trans

-- Data definition
data GrainDistributionInput = GrainSimpleDist { targetNumber::Int
                                                , targetMeanVolume::Double
                                                , targetVariance::Double
                                                , anisotropyShapeRatio::(Double,Double,Double)
                                                , seed::RandomSeed } deriving (Show)
data GrainDistOutputFile = GrainDistOutputFile { grainDistroOutFile::FilePath } deriving (Show)
data ShowIn3D = All | ShowOnly Int deriving (Show)
data RandomSeed = Seed Int | None deriving (Show)
-- >>>>>>>>>>>>>>>>>>>>>>>>  HELP  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
help = "\nVirMat help!\n\
\Usage: <distribution type> <distribution parameters> <outpu file>\n\
\    distribution type:\n\
\        \"-simple\": number of grains, grain size, variance*, anisotropy shape.\n\
\    \n\
\    distribution parameters:\n\
\        \"n=\": numbers of grains (upper limit)\n\
\        \"vol=\", \"radius=\" or \"diameter=\": grain size average\n\
\            \"s2=\": square variance; default = 1.0\n\
\        \"shape=\": anisotropy shape relation. Use \"shape=a:b:c\".\n\
\    \n\
\    output file:\n\
\        \"-o filename\" \n\
\ "
----------------------------------------------------------------------------------"

-- >>>>>>>>>>>>>>>>>>>>>>>>>   PARSING ARGUMENTS   <<<<<<<<<<<<<<<<<<<<<<<<<<<<
-- Insert the char 29 "GS" as a separator between items from args list
mark = '\29'
markStr = mark:[]
sep = char mark <?> help      -- error string == ""
maybeSep = skipMany sep

-- | Parse the string list from command line
parseArgs::[String] -> (GrainDistributionInput, GrainDistOutputFile, ShowIn3D)
parseArgs s =
    case parsed of
        Right x -> x
        Left err  -> error  (show err ++ help)
    where   s' = intercalate markStr s
            parsed = parse parseAll "arguments" s'


-- Build the top parser getting the using sub parsers
parseAll = do permute (func   <$$> parseSimpleGrainDistri
                              <||> (parseOutFile >>= (return.GrainDistOutputFile))
                              <|?> (All, parseShowOnly))
              where func p i s = (p, i, s)

-- Aplly permutation to sub parses and combine them under the "-plot" command
-- The users must to splicite the type of plot, others are optional
parseSimpleGrainDistri = do  try (do {maybeSep; string "-simple";})
                             permute
                                (func
                                <$$> parseNGrains
                                <||> parseGrainsSize
                                <|?> (1.0, parseVariance)
                                <|?> ((1,1,1), parseAnisotropyShape)
                                <|?> (None, parseSeed)
                                )
            where
                    func targetNumber targetMeanVolume targetVariance anisotropyShapeRatio seed = GrainSimpleDist {..}

-- >>>>>>>>>>>>>>>>>>>>>>>>>>> SubParser <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
-- All sub parsers from permutation can't consume any tokens if it fails, otherwise it will get stuked
-- on that paser (waiting to complete that parser): Solution = add try to the keyword that "do {maybeSep; string "hyst" ....
-- and leave the rest. In that way it will wait to complete only if the keyword was macth.

-- Parse and cast the fields
parseNGrains::Parser Int
parseNGrains = do try ( do {maybeSep; string "n="; g <- parseNum; return g;} )
                 <|> pzero

-- Parse and cast the fields
parseGrainsSize::Parser Double
parseGrainsSize = do try ( do {maybeSep; string "vol="; g <- parseNum; return g;} )
                 <|> try ( do {maybeSep; string "radius="; g <- parseNum; return g;} )
                 <|> try ( do {maybeSep; string "diameter="; g <- parseNum; return g;} )
                 <|> pzero


parseVariance::Parser Double
parseVariance = do try ( do {maybeSep; string "s2=";g <- parseNum; return g;}) -- wrap try on first setence including sep or spaces
               <|> pzero

parseAnisotropyShape::Parser (Double, Double, Double)
parseAnisotropyShape = do try (do {maybeSep; string "shape=";})
                          maybeSep; l1 <- parseNum
                          char ':'; l2 <- parseNum
                          char ':'; l3 <- parseNum
                          return (l1, l2, l3)


parseShowOnly::Parser ShowIn3D
parseShowOnly = do try ( do {maybeSep; string "showonly="; g <- parseNum; return $ ShowOnly g;} )
                   <|> pzero

-- Parse and cast the fields
parseSeed::Parser RandomSeed
parseSeed = do try ( do {maybeSep; string "seed="; n <- parseNum; return $ Seed n;} )
               <|> pzero

-- TODO fix file test
parseInFile::Parser FilePath
parseInFile = do try ( do {maybeSep; string "-i"})
                 infile <- many1 $ noneOf markStr
                 let testFile isOk = if isOk then return infile else pzero
                 --isOk <- (liftIO $ fileAccess infile True True False)
                 testFile True


parseOutFile::Parser FilePath
parseOutFile = do try ( do {maybeSep; string "-o"})
                  maybeSep
                  outfile <- many1 $ noneOf markStr
                  -- "fileAccess name read write exec" checks if the file (or other file system object)
                  -- name can be accessed for reading, writing and/or executing. To check a permission
                  -- set the corresponding argument to True.
                  --isOk <- fileAccess outfile True True False
                  if True then return outfile else pzero


parseOutDir::Parser FilePath
parseOutDir = do try ( do {maybeSep; string "-o"})
                 maybeSep
                 dir <- many1 $ noneOf markStr
                 --isOk <- fileAccess dir True True False
                 if True then return dir else pzero


-- >>>>>>>>>>>>> Fields of Others
parseNum::(Read a) => Parser a            
parseNum = try $ do maybeSep
                    n <- many1 (digit <|> char '-' <|> char '.')
                    return (read n)

parseHelp::Parser Bool
parseHelp = do  try ( do {maybeSep; string "--help"})
                return True
