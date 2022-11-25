#r "nuget: MathNet.Spatial, 0.6.0"
#r "nuget: Plotly.NET, 3.0.0"

open MathNet.Spatial.Euclidean
open System
open System.IO



module Utils =
    let run_parallel f n =
        Array.replicate n ()
        |> Array.Parallel.map f
        |> List.ofArray

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
                [ func intermediate ]
            else
                intermediate :: inner (func intermediate) (n - 1)

        inner initial count

type atrakcja = { id: int; loc: Point2D }


type ant =
    { visited: List<atrakcja>
      edges: List<atrakcja * atrakcja>
      distance: float }


type state =
    { pheromones: array<float>
      ants: List<ant> }

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
    | "./A-n80-k10.txt" -> (700, 1100)
    | "./B-n31-k5.txt" -> (180, 300)
    | "./B-n78-k10.txt" -> (450, 750)
    | "./P-n16-k8.txt" -> (120, 220)
    | "./P-n76-k5.txt" -> (700, 1500)
    | _ -> 0, 1000



// statyczna konfiguracja
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
// niezmienne dane
let mutable mapa =
    { lokacje = []
      ilość = 4
      odległości = [||] }

module Atrakcja =
    let order (a, b) = if a.id > b.id then (a, b) else (b, a)

    //przechowywujemy dane w jednowymiarowej macieży
    //dla ilość=4 indeksowanie wygląda tak:
    //-abcd
    //a
    //b1
    //c24
    //d356
    let idx (a, b) =
        if a = b then failwith "loopback"
        let (a, b) = order (a, b)
        let a = a.id - 1
        let b = b.id - 1
        a * (a - 1) / 2 + b

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

    let stat ant =
        let edges = List.pairwise ant
        let distance = List.sumBy (fun x -> mapa.odległości[Atrakcja.idx x]) edges

        { edges = edges
          distance = distance
          visited = ant }

    let liek state current destination =
        let i = Atrakcja.idx (current, destination)
        let ph = state.pheromones[i]
        let heur = 1.0 / mapa.odległości[i]

        (ph ** conf.pheromone_weight)
        * (heur ** conf.heuristic_weight)

    let choose_direction state ant =
        let current = List.head ant
        let avialable = mapa.lokacje |> List.except ant

        if Utils.rng () > conf.rand_chance then
            Utils.choose_weighted (liek state current) avialable
        else
            Utils.choose_random avialable

    let move state ant =
        let next = choose_direction state ant
        next :: ant

    let run state =
        let start = Utils.choose_random mapa.lokacje

        Utils.iterate (move state) [ start ] (mapa.ilość - 1)
        |> stat



module Simulation =
    let sum_trails ants =
        let ph = Array.create mapa.odległości.Length 0.0

        for ant in ants do
            let edges = ant.edges
            let weight = (1.0 / ant.distance)

            for edge in edges do
                let i = Atrakcja.idx edge
                ph[i] <- ph[i] + weight

        ph

    let advance state =
        let ants = [ for _ in 1 .. conf.ant_population -> Ant.run state ]

        let phsum =
            state.pheromones
            |> Array.map ((*) (1.0 - conf.evaporation_rate))
            |> Array.map2 (+) (sum_trails ants)

        { pheromones = phsum; ants = ants }


    let create_initial_state () =
        { pheromones = Array.create mapa.odległości.Length 1e-9
          ants = [] }

    let simulate () =
        let initial = create_initial_state ()

        Utils.iterations advance initial conf.iteration_count
        |> List.tail



module Plot =
    open Plotly.NET

    let ant (ant: ant) =
        printfn "length = %f" ant.distance

        Chart.Line(
            List.map (fun p -> p.loc.X) ant.visited,
            List.map (fun p -> p.loc.Y) ant.visited,
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

        [ Chart.Line(
              [ 1 .. conf.iteration_count ],
              (best_so_far |> List.transpose |> List.map median),
              LineColor = color,
              Name = "mediana" + title
          )
          Chart.Line(
              [ 1 .. conf.iteration_count ],
              (best_so_far |> List.transpose |> List.map List.min),
              Name = "najlepsze" + title,
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
        |> Chart.withTitle (sprintf "plik:%s, runs:%d" same.filename same.runs)
        |> Chart.withSize (1900, 800)
        |> Chart.withXAxisStyle ("pokolenie")
        |> Chart.withYAxisStyle ("dystans", MinMax = (bounds same.filename))
        //|> Chart.withXAxis "pokolenie"
        //|> Chart.withYTitle "dystans"
        |> Chart.show



// printf "%A" conf
let cfg =
    { ant_population = 10
      rand_chance = 0.3
      pheromone_weight = 1.
      heuristic_weight = 1.
      iteration_count = 50
      evaporation_rate = 0.1
      filename = files[5]
      runs = 2 }

[ (cfg, "ɑ=1, β=1")
  ({ cfg with pheromone_weight = 2. }, "ɑ=1, β=2")
  ({ cfg with heuristic_weight = 3. }, "ɑ=3, β=1")
  ({ cfg with
      heuristic_weight = 3.
      pheromone_weight = 2. },
   "ɑ=3, β=2") ]
|> Plot.graph_multiple "porównanie wag ɑ i β"

//  ({ cfg with evaporation_rate = 0.5 }, "uwu")
//  ({ cfg with ant_population = 30 }, "uwu")
//  ({ cfg with ant_population = 50 }, "uwu")
//  ({ cfg with rand_chance = 0.1 }, "uwu") ]
