{-# LANGUAGE NamedFieldPuns #-}

module Douane.Import.MTF.MTMDiscreteODFReader
( parseMTMDiscODF
)where


import Control.Monad (liftM)
import Data.Char
import Data.Vector (Vector, (!))
import Text.ParserCombinators.Parsec
import qualified Data.Vector as V

import Distributions.Texture.DiscreteODF

-- >>>>>>>>>>>>>>>>>>>>>>>>>   PARSING SECTION   <<<<<<<<<<<<<<<<<<<<<<<<<<<<
-- | Parse just one input file. Rise an error mesage in case of error.
parseMTMDiscODF::String -> IO DiscODF
parseMTMDiscODF fileName = do 
  parsed <- parseFromFile parseFile fileName
  case (parsed) of
    Left err  -> error (">> Error reading file " ++ fileName ++ "\n" ++ show err)
    Right xs  -> return xs

-- >>>>>>>>>>>>>>>>>>>>>>   SubParsers   <<<<<<<<<<<<<<<<<<<<<<<<<<<
parseFile::Parser DiscODF
parseFile = do
  name  <- do { x <- manyTill anyChar eol; return x;} 
  step  <- do { x <- parseNum; spaces; string "Distance between grid points"; eol; return x;} 
  nPHI1 <- do { x <- parseNum; spaces; string "Number of PHI1-values"; eol; return x; }
  sPHI1 <- do { x <- parseNum; spaces; string "First value of PHI1"; eol; return x; }
  nPHI  <- do { x <- parseNum; spaces; string "Number of PHI2-values"; eol; return x; }
  sPHI  <- do { x <- parseNum; spaces; string "First value of PHI2"; eol; return x; }
  nPHI2 <- do { x <- parseNum; spaces; string "Number of PHI-values"; eol; return x; }
  sPHI2 <- do { x <- parseNum; spaces; string "First value of PHI"; eol; return x; }
  newArr  <- gridParse nPHI1 nPHI nPHI2
  return $ DiscODF { step, nPHI1, nPHI, nPHI2, sPHI1, sPHI, sPHI2, odf = newArr }

gridParse::Int -> Int -> Int -> Parser (Vector Double)
gridParse nPHI1 nPHI nPHI2 = do 
  grid <- liftM V.fromList $ many1 parseNum
  if V.length ls == V.length grid
    then return $ newArr grid
    else error $ "MTM Discrete ODF Reader: Unexpected number of points." ++ show ( V.length ls, V.length grid)
  where
    newArr x = V.update (V.replicate (V.length ns) 0) (V.zip ns x)
    ns = V.map (getLinPos' nPHI1 nPHI) ls
    ls = V.fromList [(i,j,k) | j <- [0..nPHI-1], k <- [0..nPHI2-1], i <- [0..nPHI1-1] ]

-- >>>>>>>>>>>>> Fields of Others
parseNum::(Read a) => Parser a
parseNum = try $ do 
  spaces
  n <- many1 (digit <|> char '-' <|> char '.')
  -- correct double represantion when -1 < n < 1 (.xxx or -.xxx)
  let n' = case n of
        ('.':_) -> '0':n
        ('-':'.':xs) -> '-':'0':'.':xs
        _ -> n  
  return (read n')
      
-- | Define eol (End Of Line) as CR-LF or LF
eol::Parser String
eol = do try (string "\r\n") <|> (string "\n")

