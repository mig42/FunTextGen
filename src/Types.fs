namespace FunTextGen

module rec Types =
    type Random = {
        GetNext : int -> int
    }

    type Chain = Map<Chain.Word list, Chain.WeightedTransition list>

    module Chain =
        type Word =
            | Text of string
            | First
            | Last

        type WeightedTransition = {
            Word: Word
            Weight: int
        }


