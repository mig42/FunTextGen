namespace FunTextGen

type Token = First | Last

type Word =
    | Text of string
    | Token of Token

type WeightedTransition = {
    Word: Word
    Weight: int
}

type IRandom =
    abstract member getNext : int -> int

type Chain = Map<Word list, WeightedTransition list>

module Chain =
    module Transition =
        let add transitions word =
            let addWeight transition =
                match transition with
                    | t when t.Word = word -> { t with Weight = t.Weight + 1 }
                    | _ -> transition

            if List.exists (fun t -> t.Word = word) transitions then
                List.map addWeight transitions
            else
                { Word = word; Weight = 1 } :: transitions

        let getNext (random: IRandom) transitions =
            let totalWeight = transitions |> List.sumBy (fun t -> t.Weight)
            let targetWeight = random.getNext totalWeight
            Token.Last

    let addTransition (chain: Chain) (key: Word list) word: Chain =
        let newTransitions =
            match Map.tryFind key chain with
            | Some(transitions) -> Transition.add transitions word
            | None -> [{ Word = word; Weight = 1 }]

        Map.add key newTransitions chain

    let getNextWord (chain: Chain) key random =
        match Map.tryFind key chain with
        | Some(transitions) -> Transition.getNext random transitions
        | None -> Token.Last

