import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Svg 
import Svg.Attributes 

graphWidth : Float
graphWidth = 1000.0 

graphHeight : Float
graphHeight = 1000.0

intervalSize : Float
intervalSize = 2

radius : Float
radius = 1

type alias AbstractCircle =
  {
      cx : Float
    , cy : Float
    , r : Float
  }


main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL
type alias Model = 
  {
    innerScalar : Float,
    outerScalar : Float,
    func : (Float -> Float)
  }

init : Model
init =
  {
    innerScalar = 0.0,
    outerScalar = 0.0,
    func = sin
  }


-- UPDATE
type Msg = 
    ChangeInner String
  | ChangeOuter String
  | ChangeFunc String

update : Msg -> Model -> Model
update msg model =
  case msg of
    ChangeInner changeData ->
      {
        model | innerScalar =
          case String.toFloat changeData of
            Nothing ->
              0
            Just val ->
              val
      }

    ChangeOuter changeData ->
      {
        model | outerScalar =
          case String.toFloat changeData of
            Nothing ->
              0
            Just val ->
              val
      }

    ChangeFunc changeData ->
      if changeData == "sin" then
        {model | func = sin}
      else if changeData == "cos" then
        {model | func = cos}
      else if changeData == "tan" then
        {model | func = tan}
      else
        model

-- VIEW
view : Model -> Html Msg
view model =
  div []
    [
      div []
      [
        div [] [Html.text ("Inner: " ++ String.fromFloat model.innerScalar)],
        Html.input [placeholder "", value (String.fromFloat model.innerScalar), onInput ChangeInner] [],

        div [] [Html.text ("Outer: " ++ String.fromFloat model.outerScalar)],
        Html.input [placeholder "", value (String.fromFloat model.outerScalar), onInput ChangeOuter] [],

        div [] [],

        button [onClick (ChangeFunc "sin")] [Html.text "sin"],
        button [onClick (ChangeFunc "cos")] [Html.text "cos"],
        button [onClick (ChangeFunc "tan")] [Html.text "tan"],

        div [] [Html.text (String.fromFloat model.outerScalar ++ "(sin/cos/tan)" ++ "(" ++ String.fromFloat model.innerScalar ++ "x)")]
      ],
      Svg.svg
      [
        Svg.Attributes.width (String.fromFloat graphWidth),
        Svg.Attributes.height (String.fromFloat graphHeight)
      ]
      (
        List.map makeSvgCircle <| graphWithCircles (\n -> model.outerScalar*(model.func (degrees (model.innerScalar)*n)) + graphHeight/2)
      )
    ]


-- AUXILiARY FUNCTIONS
makeSvgCircle : AbstractCircle -> Svg.Svg msg
makeSvgCircle circleData =
  Svg.circle
    [
      Svg.Attributes.cx (String.fromFloat circleData.cx),
      Svg.Attributes.cy (String.fromFloat circleData.cy),
      Svg.Attributes.r (String.fromFloat circleData.r) 
    ]
    []


graphWithCircles : (Float -> Float) -> List AbstractCircle
graphWithCircles graphFunc =
  let
    mapList = (List.map (\n -> toFloat n) (List.range 0 (truncate (graphWidth/intervalSize))))
    mapFunction = (\n -> AbstractCircle (n*intervalSize) (graphHeight - (graphFunc n)) radius)
  in
    List.map mapFunction mapList