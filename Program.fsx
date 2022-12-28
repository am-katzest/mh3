#r "nuget: MathNet.Spatial, 0.6.0"
#r "nuget: Plotly.NET, 3.0.0"
#r "nuget: FSharp.Collections.ParallelSeq, 1.2.0"

open MathNet.Spatial.Euclidean
open System
open System.IO
open FSharp.Collections.ParallelSeq


module Utils =
    let meow_when_done f i =
        let a = f ()
        printfn "meow %d" i
        a

    let rng = Random().NextDouble


    let delay2 f x y () = f x y

    let rng_between x y =
        let delta = y - x
        x + (rng () * delta)

    let run_parallel f n =
        PSeq.init n (meow_when_done f) |> List.ofSeq

    let run_seq f n =
        Seq.init n (meow_when_done f) |> List.ofSeq


    let iterate func initial count =
        let rec inner intermediate n =
            if n = 1 then
                func intermediate
            else
                inner (func intermediate) (n - 1)

        inner initial count

    let iterations func initial count =
        let rec inner intermediate n =
            if n = 0 then
                [ intermediate ]
            else
                intermediate :: inner (func intermediate) (n - 1)

        inner initial count

    let rec without_item item =
        function
        | x :: xs when x = item -> xs
        | x :: xs -> x :: without_item item xs
        | [] -> []



module Functions =
    type test_function =
        { fn: float -> float -> float
          scale: float -> float
          x1: float
          x2: float
          y1: float
          y2: float
          name: string }

    let booth =
        let f y x =
            (x + 2. * y + 7.) ** 2. + (2. * x + y - 7.) ** 2.

        { fn = f
          x1 = -10
          x2 = 10
          y1 = -10
          y2 = 10
          scale = log10
          name = "Booth" }

    let gold =
        let f y x =
            (1.
             + (x + y + 1.) ** 2.
               * (19. - 14. * x + 3. * x ** 2. - 14. * y
                  + 6. * x * y
                  + 3. * y ** 2.))
            * (30.
               + (2. * x - 3. * y) ** 2.
                 * (18. - 32. * x + 12. * x ** 2. + 48. * y
                    - 36. * x * y
                    + 27. * y ** 2.))

        { fn = f
          x1 = -2
          x2 = 2
          y1 = -2
          y2 = 2
          scale = log10
          name = "Goldstein-Price" }

type gennum = unit -> float

type conf =
    { particle_count: int
      inertia: float
      exploration: gennum
      socialisation: gennum
      res: int
      fn: Functions.test_function }


// konfiguracja
let mutable conf =
    { particle_count = 10
      inertia = 0.5
      exploration = Utils.delay2 Utils.rng_between 0.0 1.0
      socialisation = Utils.rng
      res = 500
      fn = Functions.booth }



module Particles =
    type particle =
        { position: Point2D
          velocity: Vector2D
          best_position: Point2D
          best_value: float }

    type state =
        { particles: List<particle>
          best_one: Option<particle> }

    let move p = p

    let advance s = s

    let simulate () =
        let init = [ 1..3 ]
        Utils.iterate




module Plot =
    open Plotly.NET

    let display (fn: Functions.test_function) =
        let xw = (fn.x2 - fn.x1) / (float (conf.res - 1))
        let xs = [ fn.x1 .. xw .. fn.x2 + 0.5 * xw ]
        let yw = (fn.y2 - fn.y1) / (float (conf.res - 1))
        let ys = [ fn.y1 .. yw .. fn.y2 + 0.5 * yw ]

        let matrix =
            ys
            |> List.map (fun x -> List.map (fn.scale << (fn.fn x)) xs)


        Chart.Heatmap(matrix, X = xs, Y = ys)
        |> Chart.withTitle (fn.name)
//        |> Chart.withXAxisStyle (MinMax = (fn.x1, fn.x2))
//        |> Chart.withYAxisStyle (MinMax = (fn.y1, fn.y2))



//    Plot.display Functions.booth
//    |> Plotly.NET.Chart.show

Utils.iterations ((+) 1) 0 5
