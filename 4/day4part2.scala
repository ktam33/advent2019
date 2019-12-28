val numbers = 145852 to 616942
//val numbers = 111111 to 111111
//numbers.foreach(println)
val answer = numbers.filter(isAnswer)
println(answer.size)


def isAnswer(number: Int) : Boolean = {
    var input = number
    var digits = List[Int]()
    while (input > 0) {
        digits = input % 10 :: digits
        input /= 10
    }
    
    return isSixDigits(digits) && hasDoubles(digits) && isIncreasingDigits(digits)
}

def isSixDigits(digits: List[Int]) : Boolean = {
    digits.size == 6
}

def hasDoubles(digits: List[Int]) : Boolean = {
    for (x <- 0 to digits.size - 2) {
        if (digits(x) == digits(x + 1)) {
            if (x == 0) {
                if (digits(x) != digits(x + 2)) return true
            } 
            else if (x == digits.size - 2) {
                if (digits(x) != digits(x - 1)) return true
            }
            else if (digits(x) != digits(x + 2) && digits(x) != digits(x - 1)) {
                return true
            }
        }
    }
    return false
}

def isIncreasingDigits(digits: List[Int]) : Boolean = {
    for (x <- 1 to digits.size - 1) {
        if (digits(x) < digits(x - 1)) return false    
    }
    return true
}