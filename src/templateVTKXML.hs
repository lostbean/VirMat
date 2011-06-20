{-# LANGUAGE OverloadedStrings #-}
module TemplateVTKxml
       ( renderVTKDoc
       , renderScalarPointData
       , renderScalarCellData 
       , renderPoints
       , renderCells ) where

import Data.XML.Types
import qualified Data.Text as T
import Data.Text (Text, pack, append)
import Data.Vec hiding (pack, map, append)

toTxt::(Show a)=>a -> Text
toTxt = pack.show

renderVTKDoc::Int -> Int -> [Element] -> [Element] -> Element -> Element -> Document
renderVTKDoc nPoints nCells pointData cellData points cells =
  Document {
    documentPrologue = Prologue {prologueBefore = [], prologueDoctype = Nothing, prologueAfter = []}, 
    documentRoot = Element {
      elementName = Name {nameLocalName = "VTKFile", nameNamespace = Nothing, namePrefix = Nothing}, 
      elementAttributes = [(Name {nameLocalName = "type", nameNamespace = Nothing, namePrefix = Nothing},[ContentText "UnstructuredGrid"])], 
      elementNodes = [ 
        NodeContent (ContentText "\n"),
        NodeElement (
          Element {
             elementName = Name {nameLocalName = "UnstructuredGrid", nameNamespace = Nothing, namePrefix = Nothing},
             elementAttributes = [], 
             elementNodes = [
               NodeContent (ContentText "\n"),
               NodeElement (
                 Element {
                    elementName = Name {nameLocalName = "Piece", nameNamespace = Nothing, namePrefix = Nothing},
                    elementAttributes = [
                      -- Number of points and cells
                      (Name {nameLocalName = "NumberOfPoints", nameNamespace = Nothing, namePrefix = Nothing},[ContentText $ toTxt nPoints]),
                      (Name {nameLocalName = "NumberOfCells", nameNamespace = Nothing, namePrefix = Nothing},[ContentText $ toTxt nCells])], 
                    elementNodes =
                      -- Insert Point data
                      map NodeElement pointData
                      ++
                      -- Insert Cell Data
                      map NodeElement cellData
                      ++
                      -- Insert points
                      [NodeElement ( points ),
                      -- Insert cells
                      NodeElement ( cells )] }),
               NodeContent (ContentText "\n\n")]}),
        NodeContent (ContentText "\n")																
        ]}, documentEpilogue = []}


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
             (Name {nameLocalName = "Name", nameNamespace = Nothing, namePrefix = Nothing},[ContentText "my_scalars"]),
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
             (Name {nameLocalName = "Name", nameNamespace = Nothing, namePrefix = Nothing},[ContentText "scalars"]),
             (Name {nameLocalName = "format", nameNamespace = Nothing, namePrefix = Nothing},[ContentText "ascii"])],
           elementNodes = [
             NodeContent (ContentText "\n\t\t"),
             NodeContent (ContentText $ T.unwords $ map (pack.show) scalarData) ]}),
      NodeContent (ContentText "\n")]}

renderPoints::[Maybe Vec3D] -> Element
renderPoints points = 
  Element {
    elementName = Name {nameLocalName = "Points", nameNamespace = Nothing, namePrefix = Nothing},
    elementAttributes = [],
    elementNodes = [
      NodeContent (ContentText "\n"),
      NodeElement (
        Element {
           elementName = Name {nameLocalName = "DataArray", nameNamespace = Nothing, namePrefix = Nothing},
           elementAttributes = [
             (Name {nameLocalName = "type", nameNamespace = Nothing, namePrefix = Nothing},[ContentText "Float32"]),
             (Name {nameLocalName = "Name", nameNamespace = Nothing, namePrefix = Nothing},[ContentText "Points"]),
             (Name {nameLocalName = "NumberOfComponents", nameNamespace = Nothing, namePrefix = Nothing},[ContentText "3"]),
             (Name {nameLocalName = "format", nameNamespace = Nothing, namePrefix = Nothing},[ContentText "ascii"])],
           elementNodes = [
             NodeContent (ContentText $ T.unwords $ map showVec points)] }),
      NodeContent (ContentText "\n")]}
  where
    showVec x = case x of
      Just (Vec3D x y z) -> T.unwords [toTxt x, toTxt y, toTxt z]
      Nothing            -> pack "0.0 0.0 0.0"
  
renderCells::[Int] -> [Int] -> [Int] -> [Int] -> [Int] -> Element
renderCells cellConn cellOffsets cellTypes faces faceOffsets = 
  Element {
    elementName = Name {nameLocalName = "Cells", nameNamespace = Nothing, namePrefix = Nothing},
    elementAttributes = [],
    elementNodes = [
      NodeContent (ContentText "\n"),
      NodeElement (
        Element {
           elementName = Name {nameLocalName = "DataArray", nameNamespace = Nothing, namePrefix = Nothing},
           elementAttributes = [
             (Name {nameLocalName = "type", nameNamespace = Nothing, namePrefix = Nothing},[ContentText "Int64"]),
             (Name {nameLocalName = "Name", nameNamespace = Nothing, namePrefix = Nothing},[ContentText "connectivity"]),
             (Name {nameLocalName = "format", nameNamespace = Nothing, namePrefix = Nothing},[ContentText "ascii"])],
           elementNodes = [
             NodeContent (ContentText $ T.unwords $ map toTxt cellConn)] }),
      NodeContent (ContentText "\n"),
      NodeElement (
        Element {
           elementName = Name {nameLocalName = "DataArray", nameNamespace = Nothing, namePrefix = Nothing},
           elementAttributes = [
             (Name {nameLocalName = "type", nameNamespace = Nothing, namePrefix = Nothing},[ContentText "Int64"]),
             (Name {nameLocalName = "Name", nameNamespace = Nothing, namePrefix = Nothing},[ContentText "offsets"]),
             (Name {nameLocalName = "format", nameNamespace = Nothing, namePrefix = Nothing},[ContentText "ascii"])],
           elementNodes = [
             NodeContent (ContentText $ T.unwords $ map toTxt cellOffsets)] }),
      NodeContent (ContentText "\n"),
      NodeElement (
        Element {
           elementName = Name {nameLocalName = "DataArray", nameNamespace = Nothing, namePrefix = Nothing},
           elementAttributes = [
             (Name {nameLocalName = "type", nameNamespace = Nothing, namePrefix = Nothing},[ContentText "UInt8"]),
             (Name {nameLocalName = "Name", nameNamespace = Nothing, namePrefix = Nothing},[ContentText "types"]),
             (Name {nameLocalName = "format", nameNamespace = Nothing, namePrefix = Nothing},[ContentText "ascii"])],
           elementNodes = [
             NodeContent (ContentText $ T.unwords $ map toTxt cellTypes)] }),
      NodeContent (ContentText "\n"),
      NodeElement (
        Element {
           elementName = Name {nameLocalName = "DataArray", nameNamespace = Nothing, namePrefix = Nothing},
           elementAttributes = [
             (Name {nameLocalName = "type", nameNamespace = Nothing, namePrefix = Nothing},[ContentText "Int64"]),
             (Name {nameLocalName = "Name", nameNamespace = Nothing, namePrefix = Nothing},[ContentText "faces"]),
             (Name {nameLocalName = "format", nameNamespace = Nothing, namePrefix = Nothing},[ContentText "ascii"])],
           elementNodes = [
             NodeContent (ContentText  $ T.unwords $ map toTxt faces)] }),
      NodeContent (ContentText "\n"),
      NodeElement (
        Element {
           elementName = Name {nameLocalName = "DataArray", nameNamespace = Nothing, namePrefix = Nothing},
           elementAttributes = [
             (Name {nameLocalName = "type", nameNamespace = Nothing, namePrefix = Nothing},[ContentText "Int64"]),
             (Name {nameLocalName = "Name", nameNamespace = Nothing, namePrefix = Nothing},[ContentText "faceoffsets"]),
             (Name {nameLocalName = "format", nameNamespace = Nothing, namePrefix = Nothing},[ContentText "ascii"])],
           elementNodes = [
             NodeContent (ContentText  $ T.unwords $ map toTxt faceOffsets)]}),
      NodeContent (ContentText "\n")]}