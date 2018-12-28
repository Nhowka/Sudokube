module Sudokube



open System
open Fable.Import
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Elmish
open Elmish.React

module B = Fable.Import.Browser

let inline animateMotion props children = svgEl "animateMotion" props children

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
      Move : (CellMovement * Movement) option
      Displacements : Map<(int*int),(int*int)>
      ToAnimate: bool }

type Messages =
    | StartMoving of float * float * CellMovement
    | Moved of float * float
    | EndMoving
    | SetAnimating of bool

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
      ToAnimate = false
      Displacements = Map.empty
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
              |> Array.map (fun (f, j, k) ->
                 let c = model.Cells.[f].[j].[k]
                 c.Content, c.Position)
              |> Array.toList
              |> f with
        | [] -> []
        | a :: b -> b @ [ a ] |> f
        |> List.mapi (fun i (e,p) ->
               let (f, j, k) = r.[i]
               let d = model.Cells.[f].[j].[k]
               d.Content <- e
               d.Position, p)
        |> Map.ofList
    match msg with
    | SetAnimating t ->
        { model with ToAnimate = t}, Cmd.none
    | StartMoving(x, y, cm) ->
        { model with Move = Some(cm, (Started(x, y))) }, Cmd.none
    | EndMoving -> { model with Move = None }, Cmd.none
    | Moved(x', y') ->
        match model.Move with
        | Some(cm, (Started(x, y))) ->
            if (abs (x - x') >= 50.) then
                { model with ToAnimate = true; Move = Some(cm, Horizontal x'); Displacements = cm.Horizontal ||> move (x' > x) }, Cmd.none
            elif (abs (y - y') >= 50.) then
                { model with ToAnimate = true; Move = Some(cm, Vertical y') ; Displacements = cm.Vertical ||> move (y' > y) }, Cmd.none
            else model, Cmd.none
        | Some(cm, Horizontal x) ->
            if (abs (x - x') >= 50.) then
                { model with ToAnimate = true; Move = Some(cm, Horizontal x') ; Displacements = cm.Horizontal ||> move (x' > x) }, Cmd.none
            else model, Cmd.none
        | Some(cm, Vertical y) ->
            if (abs (y - y') >= 50.) then
                { model with ToAnimate = true; Move = Some(cm, Vertical y'); Displacements = cm.Vertical ||> move (y' > y) }, Cmd.none
            else model, Cmd.none
        | None -> model, Cmd.none

let paintCell { Content = n; Position = (x, y) as p; Color = cl; Face = f } toAnimate dispatch displacements =
    [ rect [ SVG.Width "50px"
             SVG.Height "50px"
             SVG.X(sprintf "%ipx" x)
             SVG.Y(sprintf "%ipx" y)
             SVG.Fill cl ] []
      text [ SVG.Width "50px"
             SVG.Height "50px"
             SVG.TextAnchor "middle"
             SVG.Custom ("align-baseline","middle")
             SVG.X(sprintf "%ipx" (x + 25))
             SVG.Y(sprintf "%ipx" (y + 25)) ]
                [ match displacements |> Map.tryFind p with
                  | Some (x',y') ->
                        yield animateMotion [
                            Ref (fun e ->
                                if toAnimate && e |> isNull |> not then
                                    e?beginElement()
                                    dispatch (SetAnimating false)
                                )
                            SVG.Custom ("onEnded", fun _ -> dispatch (SetAnimating true))
                            SVG.Custom ("dur", "250ms")
                            SVG.Custom ("begin", 0)
                            SVG.Custom("from",sprintf "%i,%i" (x'-x) (y'-y))
                            SVG.Custom("to",sprintf "%i,%i" 0 0)

                            SVG.Custom("repeatCount", "1")][]
                  | None -> ()
                  yield
                   str (match n with
                        | 10 -> "A"
                        | 11 -> "B"
                        | 12 -> "C"
                        | 13 -> "D"
                        | 14 -> "E"
                        | 15 -> "F"
                        | n -> string n) ]
       ]
let border n : string =
              sprintf
                  "M 450 %i a %i,%i 0 0 1 %i,%i v 200 a %i,%i 0 0 1 -%i,%i h -200 a %i,%i 0 0 1 -%i,-%i v -200 a %i,%i 0 0 1 %i,-%i Z"
                  (250 - n) n n n n n n n n n n n n n n n n
let view (model : Model) dispatch =
    let mutable rSVG : B.SVGSVGElement option = None

    let toSVGPoint x y =
        rSVG
        |> Option.map (fun s ->
               let p : B.SVGPoint = s.createSVGPoint()
               p.x <- x
               p.y <- y
               let x = p.matrixTransform ((s.getScreenCTM().inverse()))
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
              e.preventDefault()
              toSVGPoint e.touches.[0.].clientX e.touches.[0.].clientY |> Option.iter (Moved >> dispatch))
          OnMouseUp(fun _ -> dispatch EndMoving)
          OnTouchCancel(fun e ->
            e.preventDefault()
            dispatch EndMoving)
          OnTouchEnd(fun e ->
            e.preventDefault()
            dispatch EndMoving) ]
        [ yield rect [ SVG.Width "100%"
                       SVG.Height "100%"
                       SVG.Fill "lightgrey" ] []
          yield rect [
             SVG.Width "220px"
             SVG.Height "180px"
             SVG.X "670px"
             SVG.Y "40px"
             SVG.Fill "white"
             SVG.Stroke "black" ] []
          yield text [
             SVG.Width "220px"
             SVG.Height "180px"
             SVG.X "670px"
             SVG.Y "40px"
             SVG.FontSize "30px"   ] [
                 tspan [
                     SVG.Dx "1em"
                     SVG.Dy "1.2em"] [str "Duplicates:"]
                 tspan [
                     SVG.X "695px"
                     SVG.Dy "30px"
                     ] [str "• "]
                 tspan [
                     SVG.X "710px"] [str " Face group"]
                 tspan [
                     SVG.X "695px"
                     SVG.Dy "30px"
                     SVG.Fill "red"
                     ] [str "• "]
                 tspan [
                     SVG.X "710px"] [str " Red group"]
                 tspan [
                     SVG.X "695px"
                     SVG.Dy "30px"
                     SVG.Fill "green"
                     ] [str "• "]
                 tspan [
                     SVG.X "710px"] [str " Green group"]
                 tspan [
                     SVG.X "695px"
                     SVG.Dy "30px"
                     SVG.Fill "blue"
                     ] [str "• "]
                 tspan [
                     SVG.X "710px"] [str " Blue group"]
             ]
          for face in model.Cells do
              for row in face do
                  for cell in row do
                      yield! paintCell cell model.ToAnimate dispatch model.Displacements


          yield path
                    [ SVG.Stroke "red"
                      SVG.StrokeWidth "3px"
                      SVG.Fill "none"

                      SVG.D
                      <| sprintf
                             "%s %s %s %s"
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
          let paintErrors color offx offy cells =
              [
                let dups =
                    cells
                    |> Seq.groupBy (fun e -> e.Content)
                    |> Seq.filter (snd >> Seq.length >> ((<) 1))
                    |> Seq.collect snd
                for {Position = (x,y)} in dups do
                    yield
                        circle [
                            SVG.Cx (x+offx)
                            SVG.Cy (y+offy)
                            SVG.R 4
                            SVG.Fill color][]]
          for c in 0..3 do
            yield! reds.[c] |> Array.map(fun (i,j,k) -> model.Cells.[i].[j].[k]) |> paintErrors "red" 10 10
            yield! greens.[c] |> Array.map(fun (i,j,k) -> model.Cells.[i].[j].[k]) |> paintErrors "green" 10 40
            yield! blues.[c] |> Array.map(fun (i,j,k) -> model.Cells.[i].[j].[k]) |> paintErrors "blue" 40 10

          for face in model.Cells do
            yield! face |> Seq.collect id |> paintErrors "black" 40 40

         // for face in model.Cells do
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
