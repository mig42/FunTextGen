namespace FunTextGen

open FunTextGen.Types
open FunTextGen.Types.Chain

module Chain =
    module Transition =
        let add word transitions =
            let addWeight word transition =
                match transition with
                    | t when t.Word = word ->
                        { t with Weight = t.Weight + 1 }
                    | t -> t

            let isWordInTransitions word transitions =
                List.exists (fun t -> t.Word = word) transitions

            match isWordInTransitions word transitions with
            | true -> List.map (addWeight word) transitions
            | false ->
                {
                    Word = word
                    Weight = 1
                } :: transitions

        let getNext random transitions =
            let rec findTransition currentWeight transitions targetWeight =
                match transitions with
                    | [] -> Word.Last
                    | t :: rest ->
                        let newWeight = currentWeight + t.Weight
                        match newWeight with
                            | w when w <= targetWeight ->
                                findTransition newWeight rest targetWeight
                            | _w -> t.Word

            transitions
                |> List.sumBy (fun t -> t.Weight)
                |> random.GetNext
                |> findTransition 0 transitions

    let addTransition (chain: Chain) words nextWord: Chain =
        let buildTransitions word existingTransitions =
            match existingTransitions with
            | Some(transitions) -> Transition.add word transitions
            | None -> [{ Word = nextWord; Weight = 1 }]

        chain
            |> Map.tryFind words
            |> buildTransitions nextWord
            |> Map.add words <| chain

    let getNextWord (chain: Chain) words random =
        match Map.tryFind words chain with
        | Some(transitions) -> Transition.getNext random transitions
        | None -> Word.Last
