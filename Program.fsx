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
    type point = { position: Point2D; value: float }

    type particle =
        { current: point
          velocity: Vector2D
          best: point }

    type state =
        { particles: List<particle>
          best: point }

    let probe (p: Point2D) =
        { position = p
          value = conf.fn.fn p.X p.Y }

    let new_particle _ =
        let x = Utils.rng_between conf.fn.x1 conf.fn.x2
        let y = Utils.rng_between conf.fn.y1 conf.fn.y2
        let p = probe (Point2D(x, y))

        { current = p
          velocity = Vector2D(0, 0)
          best = p }

    let best_position ps =
        (List.minBy (fun x -> x.current.value) ps).current

    let move b p =
        let inertia = conf.inertia * p.velocity
        let (soc: Vector2D) = conf.socialisation () * (b - p.current.position)

        let exp =
            conf.exploration ()
            * (p.best.position - p.current.position)

        let velocity' = inertia + soc + exp
        let position' = probe (p.current.position + velocity')

        let best =
            if position'.value < p.best.value then
                position'
            else
                p.best

        { current = position'
          velocity = velocity'
          best = best }


    let advance s =
        let particles = List.map (move s.best.position) s.particles
        let new_best = best_position particles

        let best =
            if s.best.value < new_best.value then
                s.best
            else
                new_best

        { particles = particles; best = best }

    let simulate () =
        let random_particles = List.init conf.particle_count new_particle
        let best = best_position random_particles

        let init =
            { particles = random_particles
              best = best }

        let res = Utils.iterations advance init 10
        res




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

(Particles.simulate () |> List.last).best

//    Plot.display Functions.booth
//    |> Plotly.NET.Chart.show
