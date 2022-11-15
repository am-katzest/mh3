#r "nuget: MathNet.Spatial, 0.6.0"
#load "Utils.fsx"

open MathNet.Spatial.Euclidean
open System
open System.IO


module Utils =
    let rng = Random().NextDouble

    let choose_weighted lst key =
        let pick = rng () * List.sumBy key lst

        let rec crawl lst sum =
            match lst with
            | (h :: t) ->
                let sum' = sum + key h
                if sum' >= pick then h else crawl t sum'
            | [] -> failwith "pusta lista wyborów"


        crawl lst 0.

    let choose_random lst = choose_weighted lst (fun _ -> 1.)

    let iterate func initial count =
        let rec inner intermediate n =
            if n = 1 then
                func intermediate
            else
                inner (func intermediate) (n - 1)

        inner initial count

type atrakcja = { id: int; loc: Point2D }

type ant = List<atrakcja>

type state = { pheromones: array<float> }

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
    { ant_population = 10
      rand_chance = 0.3
      pheromone_weight = 1.
      heuristic_weight = 1.
      iteration_count = 100
      evaporation_rate = 0.1
      filename = "./A-n32-k5.txt" }

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



module Simulation =

    let choose_direction state (ant: ant) =
        let current = ant.Head
        let avialable = mapa.lokacje |> List.except ant
        // randomly, TODO: fix
        Utils.choose_random avialable

    let move state ant : ant =
        let next = choose_direction state ant
        next :: ant

    let run_ant state : ant =
        let start = Utils.choose_random mapa.lokacje
        Utils.iterate (move state) [ start ] mapa.ilość

    let create_initial_state () = Array.create mapa.odległości.Length 0.0

    let simulate () =
        let initial = create_initial_state ()
        3


Loading.init () // welp, no multithreading :/
//simulate ()
let zeros = Simulation.create_initial_state ()

Simulation.run_ant zeros
