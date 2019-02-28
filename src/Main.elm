import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Svg
import Svg.Attributes  exposing (x1, y1, x2, y2)
import Animation exposing (px)

graphWidth : Float
graphWidth = 1000.0

graphHeight : Float
graphHeight = 1000.0

intervalSize : Float
intervalSize = 1

radius : Float
radius = 0.5

defaultLineAttributes : List (Svg.Attribute msg)
defaultLineAttributes =
  [
    Svg.Attributes.stroke "#D80707" ,
    Svg.Attributes.strokeWidth "1",
    Svg.Attributes.strokeMiterlimit "10",
    Svg.Attributes.fill "none"
  ]

type alias AbstractCircle =
  {
      cx : Float
    , cy : Float
    , r : Float
  }

type alias Point =
  {
    x : Float,
    y : Float
  }


main =
  Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


-- MODEL
type alias Model = 
  { innerScalar : Float
  , outerScalar : Float
  , func : (Float -> Float)
  , style : Animation.State
  }

init : () -> (Model, Cmd Msg)
init _ =
  let 
    model = {
      innerScalar = 1.0,
      outerScalar = 100.0,
      func = sin,
      style = Animation.style [Animation.points ((\n -> graphHeight/2) |> functionToPoints |> pointsToTuple)]
      }
  in
    (model, Cmd.none)


-- UPDATE
type Msg
  = ChangeInner String
  | ChangeOuter String
  | ChangeFunc String
  | FadeLines
  | Animate Animation.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeInner changeData ->
      ({
        model | innerScalar =
          case String.toFloat changeData of
            Nothing ->
              0
            Just val ->
              val
      },
      Cmd.none)

    ChangeOuter changeData ->
      ({
        model | outerScalar =
          case String.toFloat changeData of
            Nothing ->
              0
            Just val ->
              val
      },
      Cmd.none)

    ChangeFunc changeData ->
      let
        newFunc = if changeData == "sin" then sin else if changeData == "cos" then cos else tan
        funcWithScalars = addScalarsToFunction { model | func = newFunc }
        newStyle = 
          Animation.interrupt
            [ Animation.to [Animation.points (funcWithScalars |> functionToPoints |> pointsToTuple)] ]
            model.style
      in
        (
          { model | style = newStyle, func = newFunc},
          Cmd.none
        )
        {--
        if changeData == "sin" then
          ({model | func = sin}, Cmd.none)
        else if changeData == "cos" then
          ({model | func = cos}, Cmd.none)
        else if changeData == "tan" then
          ({model | func = tan}, Cmd.none)
        else
          (model, Cmd.none)
        --}

    FadeLines ->
      ( { model
        | style =
            Animation.interrupt [Animation.to [Animation.opacity 0]] model.style
      }
      , Cmd.none
      )

    Animate animMsg ->
      ({ model
        | style = Animation.update animMsg model.style
      },
      Cmd.none)

-- SUPSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate [ model.style ]


-- VIEW
view : Model -> Html Msg
view model =
  let 
    --circleList = graphWithCircles (\n -> model.outerScalar*(model.func (degrees (model.innerScalar)*n)) + graphHeight/2)
    points = model |> addScalarsToFunction |> functionToPoints
  in 
    div [] [
      div [] [
        div [] [Html.text ("Inner: " ++ String.fromFloat model.innerScalar)],
        Html.input [placeholder "", value (String.fromFloat model.innerScalar), onInput ChangeInner] [],

        div [] [Html.text ("Outer: " ++ String.fromFloat model.outerScalar)],
        Html.input [placeholder "", value (String.fromFloat model.outerScalar), onInput ChangeOuter] [],

        div [] [],

        button [onClick (ChangeFunc "sin")] [Html.text "sin"],
        button [onClick (ChangeFunc "cos")] [Html.text "cos"],
        button [onClick (ChangeFunc "tan")] [Html.text "tan"],

        div [] [],

        button [onClick FadeLines ] [text "Fade Out Lines"],

        div [] [Html.text (String.fromFloat model.outerScalar ++ "(sin/cos/tan)" ++ "(" ++ String.fromFloat model.innerScalar ++ "x)")],

        div [] [text (String.left 200 (model |> addScalarsToFunction |> functionToPoints |> pointsToString))],
        div [] [ ]
      ],

      Svg.svg [
        Svg.Attributes.width (String.fromFloat graphWidth),
        Svg.Attributes.height (String.fromFloat graphHeight)
        ]
        --((List.map makeSvgCircle circleList) ++ (makeLinesFromCircles circleList))
        [
          Svg.polyline ([ Svg.Attributes.stroke "black", Svg.Attributes.fill "none", Svg.Attributes.strokeWidth "1" ] ++ Animation.render model.style)
            []
      ]
    ]


-- AUXILiARY FUNCTIONS
makeSvgCircle : AbstractCircle -> Svg.Svg msg
makeSvgCircle circleData =
  Svg.circle [
    Svg.Attributes.cx (String.fromFloat circleData.cx),
    Svg.Attributes.cy (String.fromFloat circleData.cy),
    Svg.Attributes.r (String.fromFloat circleData.r) 
  ]
  []

functionToPoints : (Float -> Float) -> List Point
functionToPoints graphFunc =  
  let 
    mapList = List.map (\n -> toFloat n) (List.range 0 (truncate (graphWidth/intervalSize)))
    mapFunction = (\n -> Point (n*intervalSize) (graphFunc (n*intervalSize)))
  in
    List.map mapFunction mapList


pointsToString : List Point -> String
pointsToString points =
  case points of
    [] -> 
      ""
    [a] -> 
      String.fromFloat a.x ++ "," ++ String.fromFloat a.y
    a::_ -> 
      String.fromFloat a.x ++ "," ++ String.fromFloat a.y ++ " " ++ pointsToString (List.drop 1 points)

pointsToPathCommand : List Point -> List Animation.PathStep
pointsToPathCommand points = 
  List.map (\n -> Animation.line n.x n.y) points

pointsToTuple : List Point -> List (Float, Float)
pointsToTuple points = 
  List.map (\n -> (n.x, n.y) ) points

addScalarsToFunction : Model -> (Float -> Float)
addScalarsToFunction model =
  (\n -> clamp -10 (graphHeight+10) (model.outerScalar*(model.func (degrees (model.innerScalar)*n)) + graphHeight/2))

graphWithCircles : (Float -> Float) -> List AbstractCircle
graphWithCircles funcToGraph =
  let
    mapList = (List.map (\n -> toFloat n) (List.range 0 (truncate (graphWidth/intervalSize))))
    mapFunction = (\n -> AbstractCircle (n*intervalSize) (graphHeight - (funcToGraph n)) radius)
  in
    List.map mapFunction mapList

makeLinesFromCircles : List AbstractCircle -> List (Svg.Svg msg)
makeLinesFromCircles circleList =
  case circleList of
    [] -> []
    [a] -> []
    [a,b] ->
      [Svg.line ([
        x1 (String.fromFloat a.cx),
        y1 (String.fromFloat a.cy),
        x2 (String.fromFloat b.cx),
        y2 (String.fromFloat b.cy)
        ]++defaultLineAttributes) []
      ]
    a::b::_ ->
      (Svg.line ([
        x1 (String.fromFloat a.cx),
        y1 (String.fromFloat a.cy),
        x2 (String.fromFloat b.cx),
        y2 (String.fromFloat b.cy)
        ]++defaultLineAttributes) []
      ) :: makeLinesFromCircles (List.drop 1 circleList)