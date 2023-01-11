#r "nuget: MathNet.Spatial, 0.6.0"
#r "nuget: Plotly.NET, 3.0.0"
#r "nuget: FSharp.Collections.ParallelSeq, 1.2.0"
#r "nuget: FSharp.Stats, 0.4.9"

open MathNet.Spatial.Euclidean
open System
open System.IO
open FSharp.Collections.ParallelSeq
open FSharp.Stats

module Utils =
    let meow_when_done f i =
        let a = f ()
        if false then printfn "meow %d" i
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
          min: float
          x1: float
          x2: float
          y1: float
          y2: float
          name: string }

    let booth =
        let f x y =
            (x + 2. * y + 7.) ** 2. + (2. * x + y - 7.) ** 2.

        { fn = f
          min = 0
          x1 = -10
          x2 = 10
          y1 = -10
          y2 = 10
          scale = log10
          name = "Booth" }

    let himmelblau =
        let f x y =
            (x * x + y - 11.) ** 2. + (x + y * y - 7.) ** 2.

        { fn = f
          min = 0
          x1 = -5
          x2 = 5
          y1 = -5
          y2 = 5
          scale = log10
          name = "Himmelblau" }

    let Schaffer2 =
        let f x y =
            0.5
            + ((sin (x * x - y * y)) ** 2. - 0.5)
              / ((1. + 0.001 * (x * x + y * y)) ** 2.)

        { fn = f
          min = 0
          x1 = -100
          x2 = 100
          y1 = -100
          y2 = 100
          scale = id
          name = "Schaffer function N.2" }


    let gold =
        let f x y =
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
          min = 3
          x1 = -2
          x2 = 2
          y1 = -3
          y2 = 1
          scale = log10
          name = "Goldstein-Price" }

    let Rastrigin =
        let A = 10.
        let n = 2.
        let s x = x * x - A * (cos (2. * Math.PI * x))
        let f x1 x2 = A * n + (s x1) + (s x2)

        { fn = f
          min = 0
          x1 = -5.12
          x2 = 5.12
          y1 = -5.12
          y2 = 5.12
          scale = id
          name = "Rastrigin" }

type gennum = unit -> float

type conf =
    { particle_count: int
      inertia: float
      exploration: gennum
      socialisation: gennum
      generations: int
      res: int
      sample: int
      fn: Functions.test_function }


// konfiguracja
let mutable conf =
    { particle_count = 10
      inertia = 0.7
      exploration = Utils.delay2 Utils.rng_between 0.0 1.0
      socialisation = Utils.rng
      generations = 50
      res = 100
      sample = 500
      fn = Functions.Rastrigin }


module Particles =
    // types --
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
    // --
    // newpart --
    let new_particle _ =
        let x = Utils.rng_between conf.fn.x1 conf.fn.x2
        let y = Utils.rng_between conf.fn.y1 conf.fn.y2
        let p = probe (Point2D(x, y))

        { current = p
          velocity = Vector2D(0, 0)
          best = p }
    // --

    let better a b = if a.value < b.value then a else b

    let best_position ps =
        ps
        |> List.map (fun x -> x.current)
        |> List.reduce better

    let move b p =
        let inertia = conf.inertia * p.velocity
        let (soc: Vector2D) = conf.socialisation () * (b - p.current.position)

        let exp =
            conf.exploration ()
            * (p.best.position - p.current.position)

        let velocity' = inertia + soc + exp
        let position' = probe (p.current.position + velocity')

        { current = position'
          velocity = velocity'
          best = better position' p.best }

    // advance --
    let advance s =
        let particles = List.map (move s.best.position) s.particles

        { particles = particles
          best = better s.best (best_position particles) }
    // --
    // simulate --
    let simulate () =
        let random_particles = List.init conf.particle_count new_particle

        let init =
            { particles = random_particles
              best = best_position random_particles }

        Utils.iterations advance init conf.generations
    // --
    let extract sims =
        sims |> List.map (fun x -> x.best.value)

module Plot =
    open Plotly.NET
    open Plotly.NET.LayoutObjects
    open Particles

    let display (fn: Functions.test_function) =
        let xw = (fn.x2 - fn.x1) / (float (conf.res - 1))
        let xs = [ fn.x1 .. xw .. fn.x2 + 0.5 * xw ]
        let yw = (fn.y2 - fn.y1) / (float (conf.res - 1))
        let ys = [ fn.y1 .. yw .. fn.y2 + 0.5 * yw ]

        let xs, ys = ys, xs

        let matrix =
            ys
            |> List.map (fun x -> List.map (fn.scale << (fn.fn x)) xs)

        Chart.Heatmap(matrix, X = ys, Y = xs, Transpose = true)
        |> Chart.withTitle (fn.name)


    let gen2plot state =
        let xys =
            state.particles
            |> List.map (fun l -> (l.current.position.X, l.current.position.Y))
            |> Seq.ofList

        Chart.Point(xys)


    let scattersChart generations =
        generations
        |> Seq.map (fun gen ->
            gen
            |> gen2plot
            |> Chart.withTraceInfo (Visible = StyleParam.Visible.False))
        |> GenericChart.combine

    let sliderSteps gens =
        let n = List.length gens + 1

        [ 0 .. n - 1 ]
        |> Seq.map (fun i ->
            let visible =
                // Set true only for the current step
                (fun index -> index = 0 || index = i)
                |> Array.init n
                |> box

            let pretty x =
                sprintf "najlepsza wartość = %f, pozycja (%.2f,%.2f)" x.value x.position.X x.position.Y

            let title =
                match i with
                | 0 -> conf.fn.name
                | x when (x <= (n - 1)) -> sprintf "pokolenie %d, %s" (x - 1) (pretty gens[x - 1].best)
                | _ -> conf.fn.name

            SliderStep.init (
                Args = [ "visible", visible; "title", title ],
                Method = StyleParam.Method.Update,
                Label = string (i)
            ))

    let slider gens =
        Slider.init (
            CurrentValue = SliderCurrentValue.init (Prefix = "pokolenie: "),
            Padding = Padding.init (T = 50),
            Steps = sliderSteps gens
        )

    let cons a b = [ a; b ]

    let make_graph () =
        let gens = () |> Particles.simulate

        gens
        |> scattersChart
        |> cons (conf.fn |> display)
        |> Chart.combine
        |> Chart.withXAxisStyle (MinMax = (conf.fn.x1, conf.fn.x2))
        |> Chart.withYAxisStyle (MinMax = (conf.fn.y1, conf.fn.y2))
        |> Chart.withSlider (slider gens)
        |> Chart.show

    let make_up_data cfg =
        conf <- cfg

        Utils.run_parallel (Particles.extract << Particles.simulate) conf.sample


    let graph ((cfg, title), color) =
        let best_so_far = cfg |> make_up_data |> List.transpose

        let worst5 =
            best_so_far
            |> List.map (Seq.ofList >> Quantile.mode 0.9)

        let best5 =
            best_so_far
            |> List.map (Seq.ofList >> Quantile.mode 0.1)

        let mean =
            best_so_far
            |> List.map (Seq.ofList >> Quantile.mode 0.5)

        printfn "%s done!" title

        [ Chart.Line([ 1 .. conf.generations ], mean, LineColor = color, Name = title + " (mediana)")
          Chart.Line(
              [ 1 .. conf.generations ],
              best5,
              Name = title + " (najlepsze 10%)",
              Opacity = 0.5,
              LineColor = color,
              LineDash = StyleParam.DrawingStyle.Dash
          )
          Chart.Line(
              [ 1 .. conf.generations ],
              worst5,
              Name = title + " (najgorsze 10%)",
              Opacity = 0.5,
              LineColor = color,
              LineDash = StyleParam.DrawingStyle.DashDot
          ) ]

    let add_colors lst =
        [ "#1f77b4"
          "#ff7f0e"
          "#2ca02c"
          "#d62728"
          "#9467bd"
          "#8c564b"
          "#e377c2"
          "#7f7f7f"
          "#bcbd22"
          "#17becf" ]
        |> List.map Color.fromHex
        |> List.take (List.length lst)
        |> List.zip lst

    let graph_multiple title lst =
        let same = lst |> List.head |> fst

        lst
        |> add_colors
        |> List.map graph
        |> List.concat
        |> Chart.combine
        |> Chart.withTitle (sprintf "funkcja:%s, ilość uruchomień:%d, %s" same.fn.name same.sample title)
        |> Chart.withSize (1900, 800)
        |> Chart.withXAxisStyle ("iteracja")
        //|> Chart.withYAxisStyle ("dystans", MinMax = (bounds same.filename))
        |> Chart.withYAxisStyle ("wynik")
        //|> Chart.withXAxis "pokolenie"
        //|> Chart.withYTitle "dystans"
        |> Chart.show



for (fn, cnt) in
    [ (Functions.Schaffer2, 15)
      (Functions.Rastrigin, 10) ] do
    let cfg =
        { particle_count = cnt
          inertia = 0.5
          exploration = Utils.rng
          socialisation = Utils.rng
          generations = 50
          res = 100
          sample = 1000 // 00
          fn = fn }

    let cfg_smol = { cfg with sample = cfg.sample / 10 }

    [ //-
      ({ cfg with inertia = 0.0 }, "0.0")
      ({ cfg with inertia = 0.5 }, "0.5")
      ({ cfg with inertia = 0.9 }, "0.9")
      //-
      ]
    |> Plot.graph_multiple "porównanie wpływu inercji"


    [ //-
      ({ cfg_smol with particle_count = 200 }, "200")
      ({ cfg_smol with particle_count = 50 }, "50")
      ({ cfg_smol with particle_count = 20 }, "20")
      ({ cfg_smol with particle_count = 10 }, "10")
      ({ cfg_smol with particle_count = 5 }, "5")
      ({ cfg_smol with particle_count = 2 }, "2")
      //-
      ]
    |> Plot.graph_multiple "porównanie wpływu ilości cząstek"


    [ //-
      ({ cfg with
          exploration = Utils.rng
          socialisation = Utils.rng },
       "s=0-1 e=0-1")
      ({ cfg with
          exploration = (fun _ -> 0.5)
          socialisation = (fun _ -> 0.5) },
       "s=0.5 e=0.5")
      ({ cfg with
          exploration = Utils.delay2 Utils.rng_between 1.0 2.0
          socialisation = Utils.rng },
       "s=0-1 e=1-2")
      ({ cfg with
          exploration = Utils.rng
          socialisation = Utils.delay2 Utils.rng_between 1.0 2.0 },
       "s=1-2 e=0-1")
      //-
      ]
    |> Plot.graph_multiple "porównanie wpływu socialisacji i exploracji"
