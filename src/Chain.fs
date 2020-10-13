namespace FunTextGen

open FunTextGen.Types
open FunTextGen.Types.Chain

module Chain =
    module Transition =
        let add transitions word =
            let addWeight word transition =
                match transition with
                    | t when t.Word = word ->
                        {
                            t with Weight = t.Weight + 1
                        }
                    | t -> t

            let isWordInTransitions transitions word =
                List.exists (fun t -> t.Word = word) transitions

            match isWordInTransitions transitions word with
            | true -> List.map (addWeight word) transitions
            | false ->
                {
                    Word = word
                    Weight = 1
                } :: transitions

        let getWeight transition =
            transition.Weight

        let getNext random transitions =
            let targetWeight = random.GetNext <| List.sumBy getWeight transitions
            Token.Last

    let addTransition (chain: Chain) key word: Chain =
        let getNewTransitions word existingTransitions =
            match existingTransitions with
            | Some(transitions) -> Transition.add transitions word
            | None ->
                [{
                    Word = word
                    Weight = 1
                }]

        chain
            |> Map.tryFind key
            |> getNewTransitions word
            |> Map.add key <| chain

    let getNextWord (chain: Chain) key random =
        match Map.tryFind key chain with
        | Some(transitions) -> Transition.getNext random transitions
        | None -> Token.Last
