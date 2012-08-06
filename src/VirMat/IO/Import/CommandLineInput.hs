{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module VirMat.IO.Import.CommandLineInput
( parseArgs
) where

import Control.Monad.Trans
import Control.Monad (liftM)
import Data.List
import Text.Parsec.Prim hiding (try)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Perm

import VirMat.IO.Import.Types
                      

-- >>>>>>>>>>>>>>>>>>>>>>>>  HELP  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
help = "\nVirMat help!\n\
\Usage: <distribution type> <distribution parameters> <outpu file> <shozOutput>\n\
\    distribution type:\n\
\        \"-voronoi\": \n\
\    \n\
\    dimension:\n\
\        \"3d\": for 3 dimensions microstructures.\n\
\        \"2d\": for 3 dimensions microstructures.\n\
\    \n\
\    distribution parameters:\n\
\        generqtor type: \"random\", \"packed\"  \n\
\        \"n=\": numbers of grains (upper limit) \n\
\        \"gsDist:\": compose a grain size distribution\n\
\            \"logNorm{scale:mean:mode:offset}\" \n\
\    \n\
\    output file:\n\
\        \"-o <outputInfo> filename\" \n\
\            outputinfo:\n\ 
\                \"v\" -> Volume\n\
\                \"a\" -> Area\n\
\                \"n\" -> Number of Grains\n\
\    \n\
\    showOutput:\n\
\        \"showall\" <showtype>\n\
\             showtype:\n\
\                 \"v\" - VoronoiGrain3D\n\
\                 \"b\" - Box3D\n\
\                 \"h\" - Hull3D\n\
\                 \"p\" - Points3D\n\
\                 \"s\" - Simplex3D\n\
\ "
----------------------------------------------------------------------------------"

-- >>>>>>>>>>>>>>>>>>>>>>>>>   PARSING ARGUMENTS   <<<<<<<<<<<<<<<<<<<<<<<<<<<<
-- Insert the char 29 "GS" as a separator between items from args list
mark = '\29'
markStr = mark:[]
sep = char mark <?> help      -- error string == ""
maybeSep = skipMany sep

-- | Parse the string list from command line
parseArgs::[String] -> JobRequest
parseArgs s =
    case parsed of
        Right x -> x
        Left err  -> error  (show err ++ help)
    where   s' = intercalate markStr s
            parsed = parse parseAll "arguments" s'


-- Build the top parser getting the using sub parsers
parseAll = parseJobRequest

-- Aplly permutation to sub parses and combine them under the "-plot" command
-- The users must to splicite the type of plot, others are optional
parseJobRequest::Parser JobRequest
parseJobRequest = do try (do {maybeSep; string "-voronoi";})
                     permute
                       (func
                        <$$> parseDimension
                        <||> parseDistributionType
                        <||> parseNGrains
                        <||> parseDistribution
                        <|?> (NoSeed, parseSeed)
                        <|?> (NoOutput,parseOutFile)
                        <|?> (NoShow, parseShowOnly)
                       )
  where
    func dimension distrType targetNumber gsDist seed outputFile showResults = VoronoiJob {..}

-- >>>>>>>>>>>>>>>>>>>>>>>>>>> SubParser <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
-- All sub parsers from permutation can't consume any tokens if it fails, otherwise it will get stuked
-- on that paser (waiting to complete that parser): Solution = add try to the keyword that "do {maybeSep; string "hyst" ....
-- and leave the rest. In that way it will wait to complete only if the keyword was macth.

-- Parse and cast the fields
parseDimension::Parser Dimension
parseDimension = do try ( do {maybeSep; string "2d"; return Dimension2D;} )
                       <|> try ( do {maybeSep; string "3d"; return Dimension3D;} )

-- Parse and cast the fields
parseDistributionType::Parser DistributionType
parseDistributionType = do try ( do {maybeSep; string "random"; return RandomDistribution;} )
                       <|> try ( do {maybeSep; string "packed"; return PackedDistribution;} )

-- Parse and cast the fields
parseNGrains::Parser Int
parseNGrains = let
  getit = try $ do
    maybeSep
    string "n="
    parseNum
  in getit <|> pzero

-- Parse and cast the fields
parseGrainsSize::Parser Double
parseGrainsSize = do try ( do {maybeSep; string "vol="; g <- parseNum; return g;} )
                 <|> try ( do {maybeSep; string "radius="; g <- parseNum; return g;} )
                 <|> try ( do {maybeSep; string "diameter="; g <- parseNum; return g;} )
                 <|> pzero


parseVariance::Parser Double
parseVariance = let
  getit = try $ do
    maybeSep
    string "s2="
    parseNum
  in getit <|> pzero

parseAnisotropyShape::Parser (Double, Double, Double)
parseAnisotropyShape = try $ do
  maybeSep; string "shape="
  maybeSep; l1 <- parseNum
  char ':'; l2 <- parseNum
  char ':'; l3 <- parseNum
  return (l1, l2, l3)

parseDistribution::Parser [CombDist]
parseDistribution = let
  getit = try $ do
    maybeSep; string "gsDist:"
    many1 (choice [parseLogNormal, parseNormal, parseUniform])
  in getit <|> pzero

parseLogNormal::Parser CombDist
parseLogNormal = let 
  getit = try $ do 
    maybeSep; string "logNorm"
    maybeSep; char '{'
    k <- parseNum
    char ':'; mean   <- parseNum
    char ':'; mode   <- parseNum
    char ':'; offset <- parseNum
    char '}'
    return $ CombDist $ LogNormal k mean mode offset
  in getit <|> pzero
     
     
parseNormal :: Parser CombDist
parseNormal = let 
  getit = try $ do 
    maybeSep; string "norm"
    maybeSep; char '{'
    k <- parseNum
    char ':'; mean <- parseNum
    char ':'; var  <- parseNum
    char '}'
    return $ CombDist $ Normal k mean var
  in getit <|> pzero

parseUniform :: Parser CombDist
parseUniform = let 
  getit = try $ do 
    maybeSep; string "uniform"
    maybeSep; char '{'
    k <- parseNum
    char ':'; mean <- parseNum
    char ':'; var  <- parseNum
    char '}'
    return $ CombDist $ Uniform k mean var
  in getit <|> pzero
                 
parseShow3DType::Parser ShowType
parseShow3DType = try ( do {maybeSep; string "v"; return ShowVoronoiGrains;} )
              <|> try ( do {maybeSep; string "b"; return ShowBox;} )
              <|> try ( do {maybeSep; string "h"; return ShowHull;} )
              <|> try ( do {maybeSep; string "p"; return ShowPoints;} )
              <|> try ( do {maybeSep; string "s"; return ShowSimplex;} )
                     
parseShowOnly::Parser ShowResults
parseShowOnly = try ( do maybeSep
                         string "showall"
                         t <- many parseShow3DType
                         return $ ShowAll t
                    )
            <|> pzero

-- Parse and cast the fields
parseSeed::Parser RandomSeed
parseSeed = let
  getit = try $ do
    maybeSep
    string "seed="
    liftM Seed parseNum
  in getit <|> pzero

-- TODO fix file test
parseInFile::Parser FilePath
parseInFile = do try ( do {maybeSep; string "-i"})
                 infile <- many1 $ noneOf markStr
                 let testFile isOk = if isOk then return infile else pzero
                 --isOk <- (liftIO $ fileAccess infile True True False)
                 testFile True

-- Parse and cast the fields
parseOutpuInfoType::Parser OutputInfoType
parseOutpuInfoType = try ( do {maybeSep; string "v" ; return GrainVolume;} )
                 <|> try ( do {maybeSep; string "a"; return GrainArea;} )
                 <|> try ( do {maybeSep; string "n"; return GrainNumber;} )


parseOutFile::Parser OutputFile
parseOutFile = do try ( do {maybeSep; string "-o"})
                  maybeSep
                  infoType <- many1 parseOutpuInfoType
                  maybeSep
                  outfile <- many1 $ noneOf markStr
                  -- "fileAccess name read write exec" checks if the file (or other file system object)
                  -- name can be accessed for reading, writing and/or executing. To check a permission
                  -- set the corresponding argument to True.
                  --isOk <- fileAccess outfile True True False
                  if True then return (OutputFile outfile infoType) else pzero
               <|> pzero

parseOutDir::Parser FilePath
parseOutDir = do try ( do {maybeSep; string "-o"})
                 maybeSep
                 dir <- many1 $ noneOf markStr
                 --isOk <- fileAccess dir True True False
                 if True then return dir else pzero


-- >>>>>>>>>>>>> Fields of Others
parseNum::(Read a) => Parser a
parseNum = try $ do 
  maybeSep
  n <- many1 (digit <|> char '-' <|> char '.')
  return (read n)

parseHelp::Parser Bool
parseHelp = do
  try ( do {maybeSep; string "--help"})
  return True
