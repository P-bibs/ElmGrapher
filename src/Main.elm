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
intervalSize = 0.5

defaultLineAttributes : List (Svg.Attribute msg)
defaultLineAttributes =
  [
    Svg.Attributes.stroke "black" ,
    Svg.Attributes.strokeWidth "2",
    Svg.Attributes.fill "none"
  ]

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
      style = Animation.style [Animation.path ((\n -> graphHeight/2) |> functionToPoints |> pointsToPathCommand)]
      }
  in
    (model, Cmd.none)

-- UPDATE
type Msg
  = ChangeInner String
  | ChangeOuter String
  | ChangeFunc String
  | Animate Animation.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeInner changeData ->
      let
        newScalar = Maybe.withDefault 0.0 (String.toFloat changeData)
        funcWithNewScalar = addScalarsToFunction { model | innerScalar = newScalar}
        newStyle = 
          Animation.interrupt
            [ Animation.to [Animation.path (funcWithNewScalar |> functionToPoints |> pointsToPathCommand)] ]
            model.style
      in
        (
          { model | style = newStyle, innerScalar = newScalar},
          Cmd.none
        )

    ChangeOuter changeData ->
      let
        newScalar = Maybe.withDefault 0.0 (String.toFloat changeData)
        funcWithNewScalar = addScalarsToFunction { model | outerScalar = newScalar}
        newStyle = 
          Animation.interrupt
            [ Animation.to [Animation.path (funcWithNewScalar |> functionToPoints |> pointsToPathCommand)] ]
            model.style
      in
        (
          { model | style = newStyle, outerScalar = newScalar},
          Cmd.none
        )

    ChangeFunc changeData ->
      let
        newFunc = if changeData == "sin" then sin else if changeData == "cos" then cos else tan
        funcWithScalars = addScalarsToFunction { model | func = newFunc }
        newStyle = 
          Animation.interrupt
            [ Animation.to [Animation.path (funcWithScalars |> functionToPoints |> pointsToPathCommand)] ]
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

        div [] [Html.text (String.fromFloat model.outerScalar ++ "(sin/cos/tan)" ++ "(" ++ String.fromFloat model.innerScalar ++ "x)")]

        --debug
        --div [] [text (String.left 200 (model |> addScalarsToFunction |> functionToPoints |> pointsToString))]
      ],

      Svg.svg [
        Svg.Attributes.width (String.fromFloat graphWidth),
        Svg.Attributes.height (String.fromFloat graphHeight)
        ]
        [
          Svg.path (defaultLineAttributes ++ Animation.render model.style)
            []
      ]
    ]


-- AUXILiARY FUNCTIONS
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
  Animation.moveTo 0 (graphHeight/2) :: (List.map (\n -> Animation.lineTo n.x n.y) points)

pointsToTuple : List Point -> List (Float, Float)
pointsToTuple points = 
  List.map (\n -> (n.x, n.y) ) points

addScalarsToFunction : Model -> (Float -> Float)
addScalarsToFunction model =
  (\n -> clamp -10 (graphHeight+10) (model.outerScalar*(model.func (degrees (model.innerScalar)*n)) + graphHeight/2))