
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module CommandLineInput
( JobRequest (..)
, DistributionType (..)
, OutputFile (..)
, OutputInfoType   (..)
, ShowIn3D   (..)
, RandomSeed (..)
, Show3DType (..)
, parseArgs
) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Perm
import Text.Parsec.Prim hiding (try)
import System.Posix
import Data.List
import Control.Monad.Trans

-- Data definition
data JobRequest =
  VoronoiGrainDistribution
    { distrType            ::DistributionType
    , targetNumber         ::Int
    , targetMeanVolume     ::Double
    , targetVariance       ::Double
    , anisotropyShapeRatio ::(Double,Double,Double)
    , seed                 ::RandomSeed
    , outputFile           ::OutputFile 
    , showIn3D             ::ShowIn3D
    } deriving (Show)

data DistributionType =
    FullDistribution
  | InBoxDistribution  
  | OnionDistribution 
    deriving (Show, Eq)
             
data OutputFile =
    NoOutput
  | OutputFile 
    { outputFilePath ::FilePath
    , outputInfo     ::[OutputInfoType]
    } deriving (Show)
                             
data OutputInfoType =
    GrainVolume
  | GrainArea  
  | GrainNumber 
    deriving (Show, Eq)
                      
data ShowIn3D =
    ShowAll [Show3DType]
  | NoShow
  | ShowOnly [Show3DType] Int 
    deriving (Show)

data Show3DType =
    VoronoiGrain3D
  | Box3D
  | Hull3D 
  | Points3D
  | Simplex3D
    deriving (Show, Eq)

data RandomSeed =
    Seed Int
  | NoSeed
    deriving (Show)


-- >>>>>>>>>>>>>>>>>>>>>>>>  HELP  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
help = "\nVirMat help!\n\
\Usage: <distribution type> <distribution parameters> <outpu file> <shozOutput>\n\
\    distribution type:\n\
\        \"-voronoi\": \n\
\    \n\
\    distribution parameters:\n\
\        generqtor type: \"full\", \"inbox\", \"onion\" \n\
\        \"n=\": numbers of grains (upper limit)\n\
\        \"vol=\", \"radius=\" or \"diameter=\": grain size average\n\
\            \"s2=\": square variance; default = 1.0\n\
\        \"shape=\": anisotropy shape relation. Use \"shape=a:b:c\".\n\
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
                        <$$> parseDistributionType
                        <||> parseNGrains
                        <||> parseGrainsSize
                        <|?> (1.0, parseVariance)
                        <|?> ((1,1,1), parseAnisotropyShape)
                        <|?> (NoSeed, parseSeed)
                        <|?> (NoOutput,parseOutFile)
                        <|?> (NoShow, parseShowOnly)
                       )
  where
    func distrType targetNumber targetMeanVolume targetVariance anisotropyShapeRatio seed outputFile showIn3D = VoronoiGrainDistribution {..}

-- >>>>>>>>>>>>>>>>>>>>>>>>>>> SubParser <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
-- All sub parsers from permutation can't consume any tokens if it fails, otherwise it will get stuked
-- on that paser (waiting to complete that parser): Solution = add try to the keyword that "do {maybeSep; string "hyst" ....
-- and leave the rest. In that way it will wait to complete only if the keyword was macth.


-- Parse and cast the fields
parseDistributionType::Parser DistributionType
parseDistributionType = do try ( do {maybeSep; string "full" ; return FullDistribution ;} )
                       <|> try ( do {maybeSep; string "inbox"; return InBoxDistribution;} )
                       <|> try ( do {maybeSep; string "onion"; return OnionDistribution;} )

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

parseShow3DType::Parser Show3DType
parseShow3DType = try ( do {maybeSep; string "v"; return VoronoiGrain3D;} )
              <|> try ( do {maybeSep; string "b"; return Box3D;} )
              <|> try ( do {maybeSep; string "h"; return Hull3D;} )
              <|> try ( do {maybeSep; string "p"; return Points3D;} )
              <|> try ( do {maybeSep; string "s"; return Simplex3D;} )
                     
parseShowOnly::Parser ShowIn3D
parseShowOnly = try ( do maybeSep
                         string "showonly="
                         g <- parseNum
                         t <- many parseShow3DType
                         return $ ShowOnly t g
                    )
            <|> try ( do maybeSep
                         string "showall"
                         t <- many parseShow3DType
                         return $ ShowAll t
                    )
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
parseNum = try $ do maybeSep
                    n <- many1 (digit <|> char '-' <|> char '.')
                    return (read n)

parseHelp::Parser Bool
parseHelp = do  try ( do {maybeSep; string "--help"})
                return True
