module Generate exposing (..)

import BinarySource exposing (BinarySource)
import Color exposing (Color)
import List
import List.Extra
import Point3d exposing (Point3d)
import Result.Extra
import Transforms
import Types exposing (..)
import Utils exposing (..)
import Vector3 exposing (Vector3)
import Vector3d


blankColoringTemplate : ColoringTemplate
blankColoringTemplate =
    ColoringTemplate
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)


blankStructureTemplate : StructureTemplate
blankStructureTemplate =
    StructureTemplate
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)


blankEyeTemplate : EyeTemplate
blankEyeTemplate =
    Err NotYetSet


consumeColoringToTemplate : BinarySource -> ( BinarySource, ColoringTemplate )
consumeColoringToTemplate fullSource =
    let
        trfunc : (BinarySource -> ColoringTemplate -> ( BinarySource, ColoringTemplate )) -> ( BinarySource, ColoringTemplate ) -> ( BinarySource, ColoringTemplate )
        trfunc tr ( s, te ) =
            tr s te

        remainingSourceAndTemplate =
            List.foldl
                trfunc
                ( fullSource, blankColoringTemplate )
                Transforms.coloringTransforms
    in
    remainingSourceAndTemplate


consumeCoreStructureToTemplate : BinarySource -> ( BinarySource, StructureTemplate )
consumeCoreStructureToTemplate fullSource =
    let
        trfunc : (BinarySource -> StructureTemplate -> ( BinarySource, StructureTemplate )) -> ( BinarySource, StructureTemplate ) -> ( BinarySource, StructureTemplate )
        trfunc tr ( s, te ) =
            tr s te
    in
    List.foldl
        trfunc
        ( fullSource, blankStructureTemplate )
        Transforms.coreStructureTransforms


consumeEyeToTemplate : BinarySource -> ( BinarySource, EyeTemplate )
consumeEyeToTemplate source =
    ( source, blankEyeTemplate )
