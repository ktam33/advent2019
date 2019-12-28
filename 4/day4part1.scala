val numbers = 145852 to 616942
//val numbers = 100 to 110
//numbers.foreach(println)
println(numbers.filter(isAnswer).size)

def isAnswer(number: Int) : Boolean = {
    var input = number
    var digits = List[Int]()
    while (input > 0) {
        digits = input % 10 :: digits
        input /= 10
    }
    
    return isSixDigits(digits) && hasConsecutiveIdentical(digits) && isIncreasingDigits(digits)
}

def isSixDigits(digits: List[Int]) : Boolean = {
    digits.size == 6
}

def hasConsecutiveIdentical(digits: List[Int]) : Boolean = {
    for (x <- 1 to digits.size - 1) {
        if (digits(x - 1) == digits(x)) return true
    }
    return false
}

def isIncreasingDigits(digits: List[Int]) : Boolean = {
    for (x <- 1 to digits.size - 1) {
        if (digits(x) < digits(x - 1)) return false    
    }
    return true
}