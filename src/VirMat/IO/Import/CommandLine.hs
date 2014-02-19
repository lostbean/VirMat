{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module VirMat.IO.Import.CommandLine
       ( getJob
       ) where

import Options.Applicative

import VirMat.IO.Import.Types


parseJob :: Parser JobRequest
parseJob = VoronoiJob
  <$> parseDimension
  <*> parseDistType
  <*> parseNGrains
  <*> parseDistribution
  <*> parseSeed
  <*> parseOutFile

getJob :: IO JobRequest
getJob = execParser opts
  where
    opts = info (helper <*> parseJob)
      ( fullDesc
      <> progDesc "Virtual microstructure generator in 3D/2D."
      <> header "Virmat"
      <> footer "Legend: k = scaling factor; mu = average; s = variance; o = offset"
      )

parseDimension :: Parser Dimension
parseDimension = let
  d3 = flag' Dimension3D (long "3d" <> help "Generate 3D microstructures (default).")
  d2 = flag' Dimension2D (long "2d" <> help "Generate 2D microstructures.")
  in d3 <|> d2 <|> pure Dimension3D

parseDistType :: Parser DistributionType
parseDistType = let
  pn = (PackedDistribution . max 0) <$> option
       ( long    "packed-n" <>
         metavar "INT"      <>
         help    "Packing grain placement with a given number of iterations." )
  r  = flag' RandomDistribution
       ( long "random" <> help "Random grain placement." )
  p  = flag' (PackedDistribution 60)
       ( long "packed" <> help "Packing grain placement with 60 iterations (default)." )
  in pn <|> p <|> r <|> pure (PackedDistribution 60)

parseNGrains :: Parser Int
parseNGrains =
  option ( long    "grains" <>
           short   'n'      <>
           metavar "INT"    <>
           help    "Number of grains." )

parseDistribution::Parser [CombDist]
parseDistribution = many $ parseLogNormal <|> parseNormal <|> parseUniform

parseNormal :: Parser CombDist
parseNormal = let
  func (a,b,c) = CombDist $ Normal a b c
  p =  option ( long    "norm"                <>
                metavar "(k, mu, s)"          <>
                help    "Normal distribution." )
  in func <$> p

parseLogNormal :: Parser CombDist
parseLogNormal = let
  func (a,b,c,d) = CombDist $ LogNormal a b c d
  p =  option ( long    "lnorm"                   <>
                metavar "(k, mu, mode, o)"   <>
                help    "Log Normal distribution." )
  in func <$> p

parseUniform :: Parser CombDist
parseUniform = let
  func (a,b,c) = CombDist $ Uniform a b c
  p =  option ( long    "uniform"              <>
                metavar "(k, mu, s)"           <>
                help    "Uniform distribution." )
  in func <$> p

parseSeed :: Parser RandomSeed
parseSeed = let
  p = optional $ option ( long    "seed"        <>
                          metavar "INT"         <>
                          help    "Random seed." )
  in maybe NoSeed Seed <$> p

parseOutFile :: Parser Output
parseOutFile = Output
  <$> strOption
      (  long    "dir"      <>
         short   'd'           <>
         metavar "FILEPATH"    <>
         help    "Output directory." )
  <*> strOption
      (  long    "sample"      <>
         short   's'           <>
         metavar "STR"    <>
         help    "Sample name." )
  <*> parseShow

parseShow :: Parser [ShowType]
parseShow = let
  v = flag' ShowVoronoi (long "showvoronoi" <> help "Show Voronoi grains.")
  b = flag' ShowBox     (long "showbox"     <> help "Show enclosing box.")
  h = flag' ShowHull    (long "showhull"    <> help "Show convex hull.")
  p = flag' ShowPoints  (long "showpoints"  <> help "Show weighted points.")
  s = flag' ShowSimplex (long "showsimplex" <> help "Show Delaunay Triangulation.")
  f = flag' ShowForces  (long "showforces"  <> help "Show forces in packing.")
  in many (v <|> b <|> h <|> p <|> s <|> f)
