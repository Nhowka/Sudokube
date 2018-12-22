module Sudokube

open System
open Fable.Import
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Elmish
open Elmish.React
type SVG = SVGAttr

type Cell =
    {
        mutable Content : int*int
        Color: string
        Position: int * int
        Face: int
    }

type Model = Cell[][][]
type Messages = 
    | Red of int * bool
    | Green of int * bool
    | Blue of int * bool

let initialState() =
    let colors =
        [|
            "#ff9999"
            "#99ff99"
            "#9999ff"
            "#ff99ff"
            "#99ffff"
            "#ffff99"
        |]
    Array.init 6
        (fun z ->
            Array.init 4
                (fun x ->
                    Array.init 4
                        (fun y ->
                            let bx,by =
                                match z with
                                |0 -> 250,50
                                |1 -> 50,250
                                |2 -> 250,250
                                |3 -> 450,250
                                |4 -> 250,450
                                |_ -> 700,700
                            {
                                Content = x,y
                                Color = colors.[z]
                                Position = bx+x*50,by+y*50
                                Face=z}))), Cmd.none

let reds =
  Array.init 4
    (fun j ->
        [|
          for i in 0..3 -> 0,i,j
          for i in 0..3 -> 3,3-j,i
          for i in 0..3 -> 4,3-i,3-j
          for i in 0..3 -> 1,j,3-i
        |]
        )
let greens =
    Array.init 4 (fun j ->
     [|
      for i in 0 .. 3 -> 0,j,i
      for i in 0 .. 3 -> 2,j,i
      for i in 0 .. 3 -> 4,j,i
      for i in 0 .. 3 -> 5,i,3-j
     |])
let blues =
    Array.init 4
     (fun j ->        
        [|
          for i in 0..3 -> 1,i,j
          for i in 0..3 -> 2,i,j
          for i in 0..3 -> 3,i,j
          for i in 0..3 -> 5,3-j,i
        |])
let update msg (model:Model) =
    let (r,d) = 
        match msg with
        | Green (n,d) -> greens.[n],d
        | Red (n,d) -> reds.[n],d
        | Blue (n,d) -> blues.[n],d
    let f = if d then List.rev else id
    match r |> Array.map (fun (f,j,k) -> model.[f].[j].[k].Content)|> Array.toList |> f with
    |[] -> []
    |a::b -> b@[a] |> f
    |> List.iteri (fun i e -> let (f,j,k) = r.[i] in model.[f].[j].[k].Content <- e)           
    model, Cmd.none

let paintCell {Content = (a,b); Position = (x,y); Color = cl;Face = f} =
   [
    rect [
        SVG.Width "50px"
        SVG.Height "50px"
        SVG.X (sprintf "%ipx" x)
        SVG.Y (sprintf "%ipx" y)
        SVG.Fill cl
    ][]
    text [
        SVG.Width "50px"
        SVG.Height "50px"
        SVG.X (sprintf "%ipx" (x+10))
        SVG.Y (sprintf "%ipx" (y+30))        
        ]
//         [str (string f + "," + string a + ","+string b)]
         [str (string (4*a+b))]
    ]

let view (model:Model) dispatch =
     
    
    svg
      [ ViewBox "0 0 950 950"
        SVG.Width "100vh" ]
        [
            yield rect [
                SVG.Width "100%"
                SVG.Height "100%"
                SVG.Fill "lightgrey"][]
            for face in model do
                for row in face do
                    for cell in row do
                        yield! paintCell cell
            let border n : string = sprintf  """
                    h 200
                    a %i,%i 0 0 1 %i,%i
                    v 200
                    a %i,%i 0 0 1 -%i,%i
                    h -200
                    a %i,%i 0 0 1 -%i,-%i
                    v -200"""           n n n n n n n n n n n n

            yield path[
                SVG.Stroke "red"
                SVG.StrokeWidth "3px"
                SVG.Fill "transparent"
                SVG.D <| sprintf """
                    M 250 50
                    %s
                    M 250 100
                    %s
                    M 250 150
                    %s
                    M 250 200
                    %s""" (border 200) (border 150) (border 100) (border 50) ][]
            
            
            yield path[
                SVG.Stroke "blue"
                SVG.StrokeWidth "3px"
                SVG.Fill "transparent"
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
                    """  ][]
            yield path[
                SVG.Stroke "green"
                SVG.StrokeWidth "3px"
                SVG.Fill "transparent"
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
                    """  ][]
            for i in 0..3 do
              let x = 260+50*i
              yield 
                path [
                    OnClick (fun _ -> dispatch (Green (i,false)))
                    SVG.Stroke "green"
                    SVG.Fill "transparent"
                    SVG.StrokeWidth "3px"
                    SVG.D <| sprintf """
                        M %i,30
                        h 10
                        v 10
                        h 10
                        v -10
                        h 10
                        l -15,-10
                        Z
                        """ x
                ][]
            for i in 0..3 do
              let y = 860-50*i
              yield 
                path [
                    OnClick (fun _ -> dispatch (Green (i,true)))
                    SVG.Stroke "green"
                    SVG.Fill "transparent"
                    SVG.StrokeWidth "3px"
                    SVG.D <| sprintf """
                        M 920,%i
                        v 10
                        h -10
                        v 10
                        h 10
                        v 10
                        l 15,-15
                        
                        Z
                        """ y
                ][]
            for i in 0..2 do
              let y = 60+50*i
              yield 
                path [
                    OnClick (fun _ -> dispatch (Red (i,false)))
                    SVG.Stroke "red"
                    SVG.Fill "transparent"
                    SVG.StrokeWidth "3px"
                    SVG.D <| sprintf """
                        M 230,%i
                        v 10
                        h 10
                        v 10
                        h -10
                        v 10
                        l -15,-15
                        
                        Z
                        """ y
                ][]
            yield 
                path [
                    OnClick (fun _ -> dispatch (Red (3,false)))
                    SVG.Stroke "red"
                    SVG.Fill "transparent"
                    SVG.StrokeWidth "3px"
                    SVG.D """
                        M 230,210
                        v 10
                        h 10                        
                        Z
                        """ 
                ][]
            for i in 0..2 do
              let x = 60+50*i
              yield 
                path [
                    OnClick (fun _ -> dispatch (Red (i,true)))
                    SVG.Stroke "red"
                    SVG.Fill "transparent"
                    SVG.StrokeWidth "3px"
                    SVG.D <| sprintf """
                        M %i,230
                        h 10
                        v 10
                        h 10
                        v -10
                        h 10
                        l -15,-10
                        Z
                        """ x
                ][]
            yield 
                path [
                    OnClick (fun _ -> dispatch (Red (3,true)))
                    SVG.Stroke "red"
                    SVG.Fill "transparent"
                    SVG.StrokeWidth "3px"
                    SVG.D """
                        M 210,230
                        h 10                        
                        v 10
                        Z
                        """ 
                ][]
            for i in 0..3 do
              let y = 260+50*i
              yield 
                path [
                    OnClick (fun _ -> dispatch (Blue (i,false)))
                    SVG.Stroke "blue"
                    SVG.Fill "transparent"
                    SVG.StrokeWidth "3px"
                    SVG.D <| sprintf """
                        M 30,%i
                        v 10
                        h 10
                        v 10
                        h -10
                        v 10
                        l -15,-15
                        
                        Z
                        """ y
                ][]
            for i in 0..3 do
              let x = 860-50*i
              yield 
                path [
                    OnClick (fun _ -> dispatch (Blue (i,true)))
                    SVG.Stroke "blue"
                    SVG.Fill "transparent"
                    SVG.StrokeWidth "3px"
                    SVG.D <| sprintf """
                        M %i,920
                        h 10
                        v -10
                        h 10
                        v 10
                        h 10
                        l -15,10
                        Z
                        """ x
                ][]    
        ]

      

// App
Program.mkProgram initialState update view
|> Program.withConsoleTrace
|> Program.withReact "elmish-app"
|> Program.run