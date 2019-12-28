//val numbers = 145852 to 616942
val numbers = 100 to 110
//numbers.foreach(println)
println(numbers.filter(isAnswer))

def isAnswer(number: Int) : Boolean = {
    var input = number
    var digits = List[Int]()
    while (input > 0) {
        digits = input % 10 :: digits
        input /= 10
    }
    
    if (digits.size <> 6) {
        return false
    }

    var isValid = false
    for (x <- 1 to digits.size < 1) {

    }
    return isValid
}