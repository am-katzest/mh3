#r "nuget: MathNet.Spatial, 0.6.0"
#r "nuget: XPlot.Plotly, 4.0.6"

open MathNet.Spatial.Euclidean
open System
open System.IO


module Utils =
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
      filename: string }
// statyczna konfiguracja
let mutable conf =
    { ant_population = 100
      rand_chance = 0.3
      pheromone_weight = 10.0
      heuristic_weight = 1.
      iteration_count = 1000
      evaporation_rate = 0.3
      filename = "./A-n32-k5.txt" }
//filename = "./A-n80-k10.txt" }

type mapa =
    { lokacje: List<atrakcja>
      ilość: int
      odległości: array<float> }
// niezmienne dane
let mutable mapa =
    { lokacje = []
      ilość = 4
      odległości = [||] }


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
            distances[idx pair] <- distance pair

        mapa <-
            { lokacje = places
              ilość = places.Length
              odległości = distances }

module Ant =
    let edges = List.pairwise

    let travel_distance = List.sumBy (fun x -> mapa.odległości[idx x])

    let distance = edges >> travel_distance


module Simulation =
    let liek state current destination =
        let i = idx (current, destination)
        let ph = state.pheromones[i]
        let dist = mapa.odległości[i]

        //(conf.heuristic_weight / dist)
        //+ (conf.pheromone_weight * ph)
        ph / dist

    let choose_direction state (ant: ant) =
        let current = ant.Head
        let avialable = mapa.lokacje |> List.except ant
        Utils.choose_weighted (liek state current) avialable

    let move state ant : ant =
        let next = choose_direction state ant
        next :: ant

    let run_ant state : ant =
        let start = Utils.choose_random mapa.lokacje
        Utils.iterate (move state) [ start ] (mapa.ilość - 1)

    let advance state =
        let ph' = Array.create mapa.odległości.Length 0.0
        let ants = [ for _ in 1 .. conf.ant_population -> run_ant state ]

        for ant in ants do
            let edges = Ant.edges ant
            let weight = (1.0 / Ant.travel_distance edges)

            // apply trail
            for edge in edges do
                let i = idx edge
                ph'[i] <- ph'[i] + weight

        let phsum =
            Array.map2 (fun v v' -> v * (1.0 - conf.evaporation_rate) + v') state.pheromones ph'

        { pheromones = phsum; ants = ants }


    let create_initial_state () =
        { pheromones = Array.create mapa.odległości.Length 0.
          ants = [] }

    let simulate () =
        let initial = create_initial_state ()

        Utils.iterations advance initial conf.iteration_count
        |> List.tail


module Plot =
    open XPlot.Plotly

    let extract states =
        states
        |> List.map (fun x -> x.ants)
        |> List.map (List.map (fun ant -> Ant.distance ant))

    let min = List.min

    let median l =
        let c = List.length l
        let l = List.sort l

        if c % 2 = 1 then
            l[c / 2]
        else
            (l[(c + 1) / 2] + l[(c - 1) / 2]) / 2.


    let gen data =
        let data = extract data

        [ Scatter(x = [ 1 .. conf.iteration_count ], y = List.map min data, mode = "lines", name = "min")
          Scatter(x = [ 1 .. conf.iteration_count ], y = List.map median data, mode = "lines", name = "median") ]

    let show data =
        gen data
        |> Chart.Plot
        //        |> Chart.WithLayout styledLayout
        |> Chart.WithWidth 1900
        |> Chart.WithHeight 800
        |> Chart.Show

    let ant (ant: ant) =
        printfn "length = %f" (Ant.distance ant)

        Scatter(
            x = List.map (fun p -> p.loc.X) ant,
            y = List.map (fun p -> p.loc.Y) ant,
            mode = "lines",
            opacity = 0.5,
            name = sprintf "length = %f" (Ant.distance ant)
        )

    let ants ants =
        ants |> List.map ant |> Chart.Plot |> Chart.Show


Loading.init () // welp, no multithreading :/

#time "on"
let results = Simulation.simulate ()

let best_ones =
    results
    |> List.map (fun x -> x.ants)
    |> List.concat
    |> List.sortByDescending Ant.distance
    |> List.take 5

Plot.ants best_ones
Plot.show results
#time "off"
