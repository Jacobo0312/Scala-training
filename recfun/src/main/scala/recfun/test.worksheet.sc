/*

PASCAL

   0 1 2 3 
0  1
1  1 1
2  1 2 1
3  1 3 3 1
4  1 4 6 4 1

pascal(2,4) = 6

= pascal (2,4)
= pascal(1,3) + pascal(2,3)
= pascal(0,2) + pascal(1,2) + pascal(1,2) + pascal(2,2)
= 1 + pascal(0,1) + pascal(1,1) + pascal(0,1) + pascal(1,1) + 1
= 1 + 1 + 1 + 1 + 1 + 1
=6

*/

val exampleGood="(if (zero? x) max (/ 1 x))".toList
val exampleBad="()(()S())A(".toList

def balance(chars: List[Char]): Boolean = 
    def balanceHelper(chars: List[Char],count: Int):Boolean=
        if (chars.isEmpty) then count==0
        else if (chars.head.equals('(')) then balanceHelper(chars.tail,count+1)
        else if (chars.head == ')') then 
            if (count > 0) then balanceHelper(chars.tail, count - 1) 
            else false        
        else balanceHelper(chars.tail,count)
    end balanceHelper
    balanceHelper(chars,0)
end balance


balance(exampleGood)



def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0) then 1
    else if (money < 0) then 0
    else if (coins.isEmpty) then 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
end countChange

countChange(300,List(5,10,20,50,100,200,500))
