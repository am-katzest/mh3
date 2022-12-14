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

    let run_parallel f n =
        PSeq.init n (meow_when_done f) |> List.ofSeq

    let run_seq f n =
        Seq.init n (meow_when_done f) |> List.ofSeq

    let rng = Random().NextDouble

    let choose_weighted key lst =
        let pick = rng () * List.sumBy key lst

        let rec crawl lst sum =
            match lst with
            | (h :: t) ->
                let sum' = sum + key h
                if sum' >= pick then h else crawl t sum'
            | [] -> failwith "pusta lista wyborów"

        crawl lst 0.

    let choose_random = choose_weighted (fun _ -> 1.)

    let iterate func initial count =
        let rec inner intermediate n =
            if n = 1 then
                func intermediate
            else
                inner (func intermediate) (n - 1)

        inner initial count

    let iterations func initial count =
        let rec inner intermediate n =
            if n = 1 then
                [ intermediate ]
            else
                intermediate :: inner (func intermediate) (n - 1)

        inner initial count

    let rec without_item item =
        function
        | x :: xs when x = item -> xs
        | x :: xs -> x :: without_item item xs
        | [] -> []

// atrakcja --
type atrakcja = { id: int; loc: Point2D }
// --
// antr --
type ant =
    { visited: List<atrakcja>
      unvisited: List<atrakcja> }
// --

// ants --
type ant_stat =
    { path: List<atrakcja>
      edges: List<atrakcja * atrakcja>
      distance: float }
// --

type state =
    { pheromones: array<float>
      ants: List<ant_stat> }

type conf =
    { ant_population: int
      rand_chance: float
      pheromone_weight: float
      heuristic_weight: float
      iteration_count: int
      evaporation_rate: float
      filename: string
      runs: int }

let files =
    [ "./A-n32-k5.txt"
      "./A-n80-k10.txt"
      "./B-n31-k5.txt"
      "./B-n78-k10.txt"
      "./P-n16-k8.txt"
      "./P-n76-k5.txt" ]

let bounds =
    function
    | "./A-n32-k5.txt" -> (400, 700)
    | "./A-n80-k10.txt" -> (1000, 1800)
    | "./B-n31-k5.txt" -> (180, 350)
    | "./B-n78-k10.txt" -> (700, 1300)
    | "./P-n16-k8.txt" -> (120, 220)
    | "./P-n76-k5.txt" -> (700, 1200)
    | _ -> 0, 1000



// konfiguracja
let mutable conf =
    { ant_population = 10
      rand_chance = 0.001
      pheromone_weight = 1.1
      heuristic_weight = 1.
      iteration_count = 200
      evaporation_rate = 0.1
      filename = "./A-n32-k5.txt" // "./A-n80-k10.txt"
      runs = 10 }


type mapa =
    { lokacje: List<atrakcja>
      ilość: int
      odległości: array<float> }
// niezmienne (w trakcie symulacji) dane
let mutable mapa =
    { lokacje = []
      ilość = 4
      odległości = [||] }

module Atrakcja =
    // index --
    //dla ilość=4 index wygląda tak:
    //-abcd
    //a
    //b1
    //c24
    //d356
    let order (a, b) = if a.id > b.id then (a, b) else (b, a)

    let idx (a, b) =
        let (a, b) = order (a, b)
        let a = a.id - 1
        let b = b.id - 1
        a * (a - 1) / 2 + b
// --
module Loading =
    let read_loc (s: string) =
        match s.Split(" ") |> List.ofArray with
        | [ _; a; b; c ] ->
            { id = (int a)
              loc = Point2D(float b, float c) }
        | _ -> failwithf "weird line (%A)" s

    let load = File.ReadLines >> Seq.map read_loc >> List.ofSeq

    let distance (a, b) = (a.loc - b.loc).Length

    let allpairs lst =
        lst
        |> List.map (fun x -> List.map (fun e -> (e, x)) lst)
        |> List.concat
        |> List.filter (fun (x, y) -> x.id > y.id)

    let init () =
        let places = load conf.filename
        let edges = allpairs places
        let distances = Array.create edges.Length 0.0

        for pair in edges do
            distances[Atrakcja.idx pair] <- distance pair

        mapa <-
            { lokacje = places
              ilość = places.Length
              odległości = distances }

module Ant =
    // stat --
    let stat ant =
        let edges = List.pairwise ant.visited
        let distance = List.sumBy (fun x -> mapa.odległości[Atrakcja.idx x]) edges

        { edges = edges
          distance = distance
          path = ant.visited }
    // --

    // liek --
    let liek state current destination =
        let i = Atrakcja.idx (current, destination)
        let ph = state.pheromones[i]
        let heur = 1.0 / mapa.odległości[i]

        (ph ** conf.pheromone_weight)
        * (heur ** conf.heuristic_weight)
    // --
    // dir --
    let choose_direction state ant =
        let current = List.head ant.visited

        if Utils.rng () > conf.rand_chance then
            Utils.choose_weighted (liek state current) ant.unvisited
        else
            Utils.choose_random ant.unvisited
    // --
    let move state ant =
        let next = choose_direction state ant
        // lepiej niż O(n^2) się i tak nie da
        let remaining = Utils.without_item next ant.unvisited

        { unvisited = remaining
          visited = next :: ant.visited }

    // ant --
    let run state =
        let start = Utils.choose_random mapa.lokacje

        let starting_ant =
            { unvisited = Utils.without_item start mapa.lokacje
              visited = [ start ] }

        Utils.iterate (move state) starting_ant (mapa.ilość - 1)
        |> stat
// --


module Simulation =
    // trails --
    let sum_trails ants =
        let ph = Array.create mapa.odległości.Length 0.0

        for ant in ants do
            let edges = ant.edges
            let weight = (1.0 / ant.distance)

            for edge in edges do
                let i = Atrakcja.idx edge
                ph[i] <- ph[i] + weight

        ph
    // --
    // advance --
    let advance state =
        let ants = [ for _ in 1 .. conf.ant_population -> Ant.run state ]

        let phsum =
            state.pheromones
            |> Array.map ((*) (1.0 - conf.evaporation_rate))
            |> Array.map2 (+) (sum_trails ants)

        { pheromones = phsum; ants = ants }
    // --

    let create_initial_state () =
        { pheromones = Array.create mapa.odległości.Length 1e-9
          ants = [] }

    let simulate () =
        let initial = create_initial_state ()

        Utils.iterations advance initial conf.iteration_count
        |> List.tail



module Plot =
    open Plotly.NET

    let ant (ant: ant_stat) =
        printfn "length = %f" ant.distance

        Chart.Line(
            List.map (fun p -> p.loc.X) ant.path,
            List.map (fun p -> p.loc.Y) ant.path,
            Opacity = 0.5,
            Name = sprintf "length = %f" ant.distance
        )

    let ants results =
        results
        |> List.concat
        |> List.map (fun s -> s.ants)
        |> List.concat
        |> List.sortBy (fun a -> a.distance)
        |> List.take 5
        |> List.map ant
        |> Chart.combine
        |> Chart.withSize (1100, 1000)
        |> Chart.withTitle (sprintf "%A" conf)
        |> Chart.show


    let median l =
        let c = List.length l
        let l = List.sort l

        if c % 2 = 1 then
            l[c / 2]
        else
            (l[(c + 1) / 2] + l[(c - 1) / 2]) / 2.

    let make_up_data cfg =
        conf <- cfg
        Loading.init ()
        Utils.run_parallel Simulation.simulate conf.runs


    let graph ((cfg, title), color) =
        let best_so_far =
            cfg
            |> make_up_data
            |> List.map (
                (List.map (fun s -> List.map (fun a -> a.distance) s.ants))
                >> (List.map List.min)
                >> (fun (x :: xs) -> List.scan min x xs)
            )

        printfn "%s done!" title

        [ Chart.Line(
              [ 1 .. conf.iteration_count ],
              (best_so_far |> List.transpose |> List.map median),
              LineColor = color,
              Name = title + " (mediana)"
          )
          Chart.Line(
              [ 1 .. conf.iteration_count ],
              (best_so_far |> List.transpose |> List.map List.min),
              Name = title + " (najlepsze)",
              Opacity = 0.5,
              LineColor = color,
              LineDash = StyleParam.DrawingStyle.Dash
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
        |> Chart.withTitle (sprintf "plik:%s, ilość uruchomień:%d, %s" same.filename same.runs title)
        |> Chart.withSize (1900, 800)
        |> Chart.withXAxisStyle ("pokolenie")
        //|> Chart.withYAxisStyle ("dystans", MinMax = (bounds same.filename))
        |> Chart.withYAxisStyle ("dystans")
        //|> Chart.withXAxis "pokolenie"
        //|> Chart.withYTitle "dystans"
        |> Chart.show


let cfg =
    { ant_population = 10
      rand_chance = 0.1
      pheromone_weight = 2.
      heuristic_weight = 3.
      iteration_count = 100
      evaporation_rate = 0.1
      filename = files[1]
      runs = 5 }

conf <- cfg
Loading.init ()

#time "on"

Utils.run_seq Simulation.simulate conf.runs
|> Plot.ants

#time "off"
