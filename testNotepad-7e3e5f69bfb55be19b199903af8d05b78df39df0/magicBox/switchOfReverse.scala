/*
2015A test -__-
*/

object Googol {

    def convertChar(c: Char): Char = {
        if (c == '0') '1'
        else '0'
    }

    def codejamSwitch(n: Int): String = {
        if (n < 0) "Captcha value is wrong"
        else if (n == 0) ""
        else {
            codejamSwitch(n-1) + "0" + codejamSwitch(n-1).reverse.map(convertChar)
        }
    }

    def wantValue(n: Int) = codejamSwitch(n).toList(n -1)    //리스트를 만든 후 가져와야 하는데.. 그래야 했는데 orz.
}

