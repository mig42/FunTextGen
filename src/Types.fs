namespace FunTextGen

module rec Types =
    type Random = {
        GetNext : int -> int
    }

    type Chain = Map<Chain.Word list, Chain.WeightedTransition list>

    module Chain =
        type Token =
            | First
            | Last

        type Word =
            | Text of string
            | Token of Token

        type WeightedTransition = {
            Word: Word
            Weight: int
        }


