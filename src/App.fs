module Sudokube

open System
open Fable.Import
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Elmish
open Elmish.React

module B = Fable.Import.Browser

type SVG = SVGAttr

type Movement =
    | Started of float * float
    | Horizontal of float
    | Vertical of float

type Color =
    | Red of int
    | Green of int
    | Blue of int

type CellMovement =
    { Horizontal : Color * bool
      Vertical : Color * bool }

type Cell =
    { mutable Content : int
      Color : string
      Position : int * int
      Move : CellMovement
      Face : int }

type Model =
    { Cells : Cell [] [] []
      Move : (CellMovement * Movement) option }

type Messages =
    | StartMoving of float * float * CellMovement
    | Moved of float * float
    | EndMoving

let initialState() =
    let colors =
        [| "#ff9999", 250, 50,
           (fun x y ->
           { Horizontal = (Red y), false
             Vertical = (Green x), false })
           "#99ff99", 50, 250,
           (fun x y ->
           { Horizontal = (Blue y), false
             Vertical = (Red x), true })
           "#9999ff", 250, 250,
           (fun x y ->
           { Horizontal = (Blue y), false
             Vertical = (Green x), false })
           "#ff99ff", 450, 250,
           (fun x y ->
           { Horizontal = (Blue y), false
             Vertical = (Red (3-x)), false })
           "#99ffff", 250, 450,
           (fun x y ->
           { Horizontal = (Red(3 - y)), true
             Vertical = (Green x), false })
           "#ffff99", 700, 700,
           (fun x y ->
           { Horizontal = (Green(3 - y)), false
             Vertical = (Blue(3 - x)), false }) |]
    { Move = None
      Cells = Array.init 6 (fun z -> Array.init 4 (fun x -> Array.init 4 (fun y ->
                                                                let color, bx,by, m = colors.[z]
                                                                { Content =
                                                                      4 * x + y
                                                                  Color = color
                                                                  Move = m x y
                                                                  Position =
                                                                      bx
                                                                      + x * 50,
                                                                      by
                                                                      + y * 50
                                                                  Face = z }))) },
    Cmd.none

let reds =
    Array.init 4 (fun j ->
        [| for i in 0..3 -> 0, i, j
           for i in 0..3 -> 3, 3 - j, i
           for i in 0..3 -> 4, 3 - i, 3 - j
           for i in 0..3 -> 1, j, 3 - i |])
let greens =
    Array.init 4 (fun j ->
        [| for i in 0..3 -> 0, j, i
           for i in 0..3 -> 2, j, i
           for i in 0..3 -> 4, j, i
           for i in 0..3 -> 5, i, 3 - j |])
let blues =
    Array.init 4 (fun j ->
        [| for i in 0..3 -> 1, i, j
           for i in 0..3 -> 2, i, j
           for i in 0..3 -> 3, i, j
           for i in 0..3 -> 5, 3 - j, i |])

let update msg (model : Model) =
    let move next color flip =
        let r =
            match color with
            | Green(n) -> greens.[n]
            | Red(n) -> reds.[n]
            | Blue(n) -> blues.[n]

        let d = next <> flip

        let f =
            if d then List.rev
            else id
        match r
              |> Array.map (fun (f, j, k) -> model.Cells.[f].[j].[k].Content)
              |> Array.toList
              |> f with
        | [] -> []
        | a :: b -> b @ [ a ] |> f
        |> List.iteri (fun i e ->
               let (f, j, k) = r.[i] in model.Cells.[f].[j].[k].Content <- e)
    match msg with
    | StartMoving(x, y, cm) ->
        { model with Move = Some(cm, (Started(x, y))) }, Cmd.none
    | EndMoving -> { model with Move = None }, Cmd.none
    | Moved(x', y') ->
        match model.Move with
        | Some(cm, (Started(x, y))) ->
            if (abs (x - x') >= 50.) then
                cm.Horizontal ||> move (x' > x)
                { model with Move = Some(cm, Horizontal x') }, Cmd.none
            elif (abs (y - y') >= 50.) then
                cm.Vertical ||> move (y' > y)
                { model with Move = Some(cm, Vertical y') }, Cmd.none
            else model, Cmd.none
        | Some(cm, Horizontal x) ->
            if (abs (x - x') >= 50.) then
                cm.Horizontal ||> move (x' > x)
                { model with Move = Some(cm, Horizontal x') }, Cmd.none
            else model, Cmd.none
        | Some(cm, Vertical y) ->
            if (abs (y - y') >= 50.) then
                cm.Vertical ||> move (y' > y)
                { model with Move = Some(cm, Vertical y') }, Cmd.none
            else model, Cmd.none
        | None -> model, Cmd.none

let paintCell { Content = n; Position = (x, y); Color = cl; Face = f } =
    [ rect [ SVG.Width "50px"
             SVG.Height "50px"
             SVG.X(sprintf "%ipx" x)
             SVG.Y(sprintf "%ipx" y)
             SVG.Fill cl ] []
      text [ SVG.Width "50px"
             SVG.Height "50px"
             SVG.X(sprintf "%ipx" (x + 10))
             SVG.Y(sprintf "%ipx" (y + 30)) ] [ //         [str (string f + "," + string a + ","+string b)]
                                                str (match n with
                                                     | 10 -> "A"
                                                     | 11 -> "B"
                                                     | 12 -> "C"
                                                     | 13 -> "D"
                                                     | 14 -> "E"
                                                     | 15 -> "F"
                                                     | n -> string n) ] ]

let view (model : Model) dispatch =
    let mutable rSVG : B.SVGElement option = None

    let toSVGPoint x y =
        rSVG
        |> Option.map (fun s ->
               let p : B.SVGPoint = s?createSVGPoint ()
               p.x <- x
               p.y <- y
               let x = p.matrixTransform ((s?getScreenCTM()?inverse()))
               x.x, x.y)
    
    svg [ ViewBox "0 0 950 950"
          SVG.Width "100vh"
          Style [ Custom("userSelect", "none") ]
          Ref(fun e ->
              if isNull e |> not then rSVG <- Some !!e)

          OnMouseMove
              (fun e ->
              toSVGPoint e.clientX e.clientY |> Option.iter (Moved >> dispatch))
          OnTouchMove  (fun e ->
              toSVGPoint e.touches.[0.].clientX e.touches.[0.].clientY |> Option.iter (Moved >> dispatch))
          OnMouseUp(fun _ -> dispatch EndMoving)
          OnTouchCancel(fun _ -> dispatch EndMoving)
          OnTouchEnd(fun _ -> dispatch EndMoving) ]
        [ yield rect [ SVG.Width "100%"
                       SVG.Height "100%"
                       SVG.Fill "lightgrey" ] []
          for face in model.Cells do
              for row in face do
                  for cell in row do
                      yield! paintCell cell

          let border n : string =
              sprintf
                  "a %i,%i 0 0 1 %i,%i v 200 a %i,%i 0 0 1 -%i,%i h -200 a %i,%i 0 0 1 -%i,-%i v -200 a %i,%i 0 0 1 %i,-%i Z"
                  n n n n n n n n n n n n n n n n
          yield path
                    [ SVG.Stroke "red"
                      SVG.StrokeWidth "3px"
                      SVG.Fill "none"

                      SVG.D
                      <| sprintf
                             "M 450 50 %s M 450 100 %s M 450 150 %s M 450 200 %s"
                             (border 200) (border 150) (border 100) (border 50) ]
                    []
          yield path [ SVG.Stroke "blue"
                       SVG.StrokeWidth "3px"
                       SVG.Fill "none"
                       SVG.D """
                    M 50 250
                    h 600
                    a 250,250 0 0 1 250,250
                    v 400
                    M 50 300
                    h 600
                    a 200,200 0 0 1 200,200
                    v 400
                    M 50 350
                    h 600
                    a 150,150 0 0 1 150,150
                    v 400
                    M 50 400
                    h 600
                    a 100,100 0 0 1 100,100
                    v 400
                    M 50 450
                    h 600
                    a 50,50 0 0 1 50,50
                    v 400
                    """ ] []
          yield path [ SVG.Stroke "green"
                       SVG.StrokeWidth "3px"
                       SVG.Fill "none"
                       SVG.D """
                    M 250 50
                    v 600
                    a 250,250 0 0 0 250,250
                    h 400
                    M 300 50
                    v 600
                    a 200,200 0 0 0 200,200
                    h 400
                    M 350 50
                    v 600
                    a 150,150 0 0 0 150,150
                    h 400
                    M 400 50
                    v 600
                    a 100,100 0 0 0 100,100
                    h 400
                    M 450 50
                    v 600
                    a 50,50 0 0 0 50,50
                    h 400
                    """ ] []
          for face in model.Cells do
              for row in face do
                  for { Position = (x, y); Move = cm } in row do
                      yield rect
                                [ SVG.Width "50px"
                                  SVG.Height "50px"
                                  SVG.X(sprintf "%ipx" x)
                                  SVG.Y(sprintf "%ipx" y)
                                  SVG.Fill "transparent"
                                  OnTouchStart
                                      (fun e ->
                                      toSVGPoint e.touches.[0.].clientX e.touches.[0.].clientY
                                      |> Option.iter
                                             (fun (x, y) ->
                                             dispatch (StartMoving(x, y, cm))))
                                  OnMouseDown
                                      (fun e ->
                                      toSVGPoint e.clientX e.clientY
                                      |> Option.iter
                                             (fun (x, y) ->
                                             dispatch (StartMoving(x, y, cm)))) ]
                                [] ]

let withReactSynchronous placeholderId (program : Elmish.Program<_, _, _, _>) =
    let setState model dispatch =
        Fable.Import.ReactDom.render
            (program.view model dispatch,
             Fable.Import.Browser.document.getElementById (placeholderId))
    { program with setState = setState }

// App
Program.mkProgram initialState update view
|> withReactSynchronous "elmish-app"
|> Program.run
