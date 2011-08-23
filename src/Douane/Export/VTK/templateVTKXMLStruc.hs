{-# LANGUAGE OverloadedStrings #-}
module Douane.Export.VTK.TemplateVTKXMLStruc
       ( renderVTKDoc
       , renderScalarPointData 
       , renderScalarCellData ) where

import Data.List (intersperse)
import Data.Text (Text, pack, append)
import Data.Vec hiding (pack, map, append)
import Data.XML.Types
import qualified Data.Text as T

toTxt::(Show a)=>a -> Text
toTxt = pack.show

type Range    = (Int, Int, Int ,Int, Int, Int)
type Origin   = (Double, Double, Double)
type StepSize = (Double, Double, Double)


renderVTKDoc::Range -> Origin -> StepSize -> [Element] -> [Element] -> Document
renderVTKDoc range origin stepsize pointData cellData =
  Document {
    documentPrologue = Prologue {prologueBefore = [], prologueDoctype = Nothing, prologueAfter = []}, 
    documentRoot = Element {
      elementName = Name {nameLocalName = "VTKFile", nameNamespace = Nothing, namePrefix = Nothing}, 
      elementAttributes = [(Name {nameLocalName = "type", nameNamespace = Nothing, namePrefix = Nothing},[ContentText "ImageData"])], 
      elementNodes = [ 
        NodeContent (ContentText "\n"),
        NodeElement (
          Element {
             elementName = Name {nameLocalName = "ImageData", nameNamespace = Nothing, namePrefix = Nothing},
             elementAttributes = [(Name {nameLocalName = "WholeExtent", nameNamespace = Nothing, namePrefix = Nothing},[ContentText $ mapToText [x1,x2,y1,y2,z1,z2] ]),
                                  (Name {nameLocalName = "Origin", nameNamespace = Nothing, namePrefix = Nothing},[ContentText $ mapToText [x0,y0,z0] ]),
                                  (Name {nameLocalName = "Spacing", nameNamespace = Nothing, namePrefix = Nothing},[ContentText $ mapToText [dx,dy,dz] ]) ], 
             elementNodes = [
               NodeContent (ContentText "\n"),
               NodeElement (
                 Element {
                    elementName = Name {nameLocalName = "Piece", nameNamespace = Nothing, namePrefix = Nothing},
                    elementAttributes = [(Name {nameLocalName = "Extent", nameNamespace = Nothing, namePrefix = Nothing},[ContentText $ mapToText [x1,x2,y1,y2,z1,z2] ])],
                    elementNodes =
                      -- Insert Point data
                      map NodeElement pointData
                      ++
                      -- Insert Cell Data
                      map NodeElement cellData }),
               NodeContent (ContentText "\n\n")]}),
        NodeContent (ContentText "\n")																
        ]}, documentEpilogue = []}
  where
    mapToText::(Show a)=> [a] -> Text
    mapToText = T.unwords . map toTxt 
    (x1, x2, y1, y2, z1, z2) = range
    (x0, y0, z0) = origin
    (dx, dy, dz) = stepsize
  

renderScalarPointData::String -> [Double] -> Element
renderScalarPointData name scalarData =
  Element {
    elementName = Name {nameLocalName = "PointData", nameNamespace = Nothing, namePrefix = Nothing},
    elementAttributes = [(Name {nameLocalName = "Scalars", nameNamespace = Nothing, namePrefix = Nothing},[ContentText $ pack name])],
    elementNodes = [
      NodeContent (ContentText "\n"),
      NodeElement (
        Element {
           elementName = Name {nameLocalName = "DataArray", nameNamespace = Nothing, namePrefix = Nothing},
           elementAttributes = [
             (Name {nameLocalName = "type", nameNamespace = Nothing, namePrefix = Nothing}, [ContentText "Float32"]),
             (Name {nameLocalName = "Name", nameNamespace = Nothing, namePrefix = Nothing},[ContentText $ pack name]),
             (Name {nameLocalName = "format", nameNamespace = Nothing, namePrefix = Nothing},[ContentText "ascii"])],
           elementNodes = [
             NodeContent (ContentText "\n\t\t"),
             NodeContent (ContentText $ T.unwords $ map (pack.show) scalarData)]}),
      NodeContent (ContentText "\n")]}
  
renderScalarCellData::String -> [Double] -> Element
renderScalarCellData name scalarData = 
  Element {
    elementName = Name {nameLocalName = "CellData", nameNamespace = Nothing, namePrefix = Nothing},
    elementAttributes = [(Name {nameLocalName = "Scalars", nameNamespace = Nothing, namePrefix = Nothing},[ContentText $ pack name])],
    elementNodes = [
      NodeContent (ContentText "\n"),
      NodeElement (
        Element {
           elementName = Name {nameLocalName = "DataArray", nameNamespace = Nothing, namePrefix = Nothing},
           elementAttributes = [
             (Name {nameLocalName = "type", nameNamespace = Nothing, namePrefix = Nothing},[ContentText "Float32"]),
             (Name {nameLocalName = "Name", nameNamespace = Nothing, namePrefix = Nothing},[ContentText $ pack name]),
             (Name {nameLocalName = "format", nameNamespace = Nothing, namePrefix = Nothing},[ContentText "ascii"])],
           elementNodes = [
             NodeContent (ContentText "\n\t\t"),
             NodeContent (ContentText $ T.unwords $ map (pack.show) scalarData) ]}),
      NodeContent (ContentText "\n")]}