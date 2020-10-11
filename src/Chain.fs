namespace FunTextGen

module Chain =
    type Token = First | Last

    type Word =
        | Text of string
        | Token of Token

    type WeightedTransition = {
        Word: Word
        Weight: int
    }

    type Chain = Map<Word list, WeightedTransition list>

    let addTransition (chain: Chain) (key: Word list) word: Chain =
        let addTransitionToList transitions word =
            let addWeight transition =
                match transition with
                    | t when t.Word = word -> { t with Weight = t.Weight + 1 }
                    | _ -> transition

            if List.exists (fun t -> t.Word = word) transitions then
                List.map addWeight transitions
            else
                { Word = word; Weight = 1 } :: transitions

        let newTransitions =
            match Map.tryFind key chain with
            | Some(transitions) -> addTransitionToList transitions word
            | None -> [{ Word = word; Weight = 1 }]

        Map.add key newTransitions chain
