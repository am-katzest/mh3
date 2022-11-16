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

type ant = List<atrakcja>


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
    | "./P-n76-k5.txt" -> (600, 900)
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
    let edges = List.pairwise

    let travel_distance = List.sumBy (fun x -> mapa.odległości[Atrakcja.idx x])

    let distance = edges >> travel_distance


    let liek state current destination =
        let i = Atrakcja.idx (current, destination)
        let ph = state.pheromones[i]
        let heur = 1.0 / mapa.odległości[i]

        (ph ** conf.pheromone_weight)
        * (heur ** conf.heuristic_weight)

    let choose_direction state (ant: ant) =
        let current = ant.Head
        let avialable = mapa.lokacje |> List.except ant

        if Utils.rng () > conf.rand_chance then
            Utils.choose_weighted (liek state current) avialable
        else
            Utils.choose_random avialable

    let move state ant : ant =
        let next = choose_direction state ant
        next :: ant

    let run state : ant =
        let start = Utils.choose_random mapa.lokacje
        Utils.iterate (move state) [ start ] (mapa.ilość - 1)



module Simulation =
    let advance state =
        let ph' = Array.create mapa.odległości.Length 0.0
        let ants = [ for _ in 1 .. conf.ant_population -> Ant.run state ]

        for ant in ants do
            let edges = Ant.edges ant
            let weight = (1.0 / Ant.travel_distance edges)

            // apply trail
            for edge in edges do
                let i = Atrakcja.idx edge
                ph'[i] <- ph'[i] + weight

        let phsum =
            Array.map2 (fun v v' -> v * (1.0 - conf.evaporation_rate) + v') state.pheromones ph'

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
        printfn "length = %f" (Ant.distance ant)

        Chart.Line(
            List.map (fun p -> p.loc.X) ant,
            List.map (fun p -> p.loc.Y) ant,
            Opacity = 0.5,
            Name = sprintf "length = %f" (Ant.distance ant)
        )

    let ants ants =
        ants
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

    let graph () =
        Loading.init ()
        let results = Utils.run_parallel Simulation.simulate conf.runs

        let best_ones =
            results
            |> List.concat
            |> List.map (fun s -> s.ants)
            |> List.concat
            |> List.sortBy Ant.distance
            |> List.take 5

        let distances =
            results
            |> List.map (List.map (fun s -> List.map Ant.distance s.ants))

        let best_of_each_generation = List.map (List.map List.min) distances

        let best_so_far =
            List.map (fun (x :: xs) -> List.scan min x xs) best_of_each_generation



        printfn "%A %A" best_so_far[0].Length best_of_each_generation[0].Length
        ants best_ones

        [ Chart.Line(
              [ 1 .. conf.iteration_count ],
              (best_of_each_generation
               |> List.transpose
               |> List.map List.min),
              Name = "najlepsze w pokoleniu (min)"
          )
          Chart.Line(
              [ 1 .. conf.iteration_count ],
              (best_so_far |> List.transpose |> List.map median),
              Name = "najlepsze dotychcasz (mediana)"
          )
          Chart.Line(
              [ 1 .. conf.iteration_count ],
              (best_so_far |> List.transpose |> List.map List.min),
              Name = "najlepsze dotychcasz (minimum)"
          ) ]
        |> Chart.combine
        |> Chart.withTitle (sprintf "%A" conf)
        |> Chart.withSize (1900, 800)
        |> Chart.withXAxisStyle ("pokolenie")
        |> Chart.withYAxisStyle ("dystans", MinMax = (bounds conf.filename))
        //|> Chart.withXAxis "pokolenie"
        //|> Chart.withYTitle "dystans"
        |> Chart.show

        "meow"
// printf "%A" conf

conf <-
    { ant_population = 10
      rand_chance = 0.001
      pheromone_weight = 1.
      heuristic_weight = 1.
      iteration_count = 200
      evaporation_rate = 0.5
      filename = files[5]
      runs = 10 }

Plot.graph ()
