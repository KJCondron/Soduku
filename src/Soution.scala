// To execute Scala, please define main on an object named Solution
// Unfortunately, Scala interpreted mode is not supported yet

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Set

object Solution {
  
  final val size = 2
  final val max = size+1
  
  final val set : Set[(Integer,Integer,Integer)] = Set[(Integer,Integer,Integer)]()

  /*def main(args: Array[String]) {
    println("Start")
    val board = Array(Array(3, 0, 6, 5, 0, 8, 4, 0, 0),
                      Array(5, 2, 0, 0, 0, 0, 0, 0, 0),
                      Array(0, 8, 7, 0, 0, 0, 0, 3, 1),
                      Array(0, 0, 3, 0, 1, 0, 0, 8, 0),
                      Array(9, 0, 0, 8, 6, 3, 0, 0, 5),
                      Array(0, 5, 0, 0, 9, 0, 6, 0, 0),
                      Array(1, 3, 0, 0, 0, 0, 2, 5, 0),
                      Array(0, 0, 0, 0, 0, 0, 0, 7, 4),
                      Array(0, 0, 5, 2, 0, 6, 3, 0, 0));
    solve(board)
  }*/
  
  def main(args: Array[String]) {
    println("Start")
    val board = Array(Array(1, 0, 0),
                      Array(0, 1, 0),
                      Array(0, 0, 1))
    solve(board)
  }

  private def checkColumn( xs  : Array[Array[Int]], colNum : Integer ) : Boolean =
  {
      val col : Array[Int] = xs.map( row => row(colNum) )
      checkRow(col)
  }
  

  private def checkSquare( xs  : Array[Array[Int]], sqNum : Integer ) : Boolean =
  {
      //val square : Array[Int] = getSquare(xs, sqNum)
      //checkRow(square)
      true
  }
  
  private def getSquare(xs: Array[Array[Int]], sqNum: Int): Array[Int] = {
    var lb = ListBuffer.empty[Int]
    for {
      x <- 3*(sqNum/3) to 3*(sqNum/3) + 2
      y <- 3*(sqNum%3) to 3*(sqNum%3) + 2
    } lb.append(xs(x)(y))
    lb.toArray
  }
  
  private def checkRow9(row: Array[Int]): Boolean = {
    row.toSet == Set(1,2,3,4,5,6,7,8,9)
  }
  
  private def checkRow(row: Array[Int]): Boolean = {
    row.toSet == (1 to max).toSet
  }
  
  private def solve(board: Array[Array[Int]], depth : Int = 1) : Boolean = { 
    board.foreach( row => println(row.mkString(" ")))
    println
    if( (board.forall(checkRow) && 
         (0 to size).map( 
           c =>  checkColumn(board,c) && 
                 checkSquare(board,c) ).foldLeft(true)(_&&_) ))
         {
    		println("works")
    	//board.foreach( row => println(row.mkString(" ")))
           return true
         }
         else
        {  
          (0 to size).foreach( r=> {
        	  	val used = board(r).filterNot(_==0)
            	val unused = (1 to max).filterNot( x=> used.contains(x) ).sorted
            	(0 to size).foreach( c => if(board(r)(c) == 0) 
            	{
            	  unused.foreach( v=> {
            		  println( ">>" + r +"," +c +"," + v +"," + depth)
            		  if(set.exists(_==(r,c,v)))
            		    println( "duplicate >>" + r +"," +c +"," + v +"," + depth)
            		   else
            		     set += ((r,c,v))
            		  board(r)(c) = v
            		  solve(board, depth+1)
            			})
            		board(r)(c)=0
                })
          })
          println( ">>>>" + depth)
          false
       }
    }
}
  
  /*private def solve(board: Array[Array[Int]], used : Array[Int] = Array() ): Boolean = {

    if( (board.forall(checkRow) && 
         (0 to 8).map( 
           c =>  checkColumn(board,c) && 
                 checkSquare(board,c) ).foldLeft(true)(_&&_) ))
         {
           board.foreach( row => println(row.mkString(" ")))
           return true
         }
         else
        {
          
          (0 to 8).foreach( r=> {
            val usedInThisRow : Array[Int] = 
              if( used==Array() )
            	board(r).filterNot(_==0)
            	else
            	  used
            	  
            (0 to 8).foreach( c=> if(board(r)(c) == 0) 
            	{
            		(1 to 9).filterNot( vp => usedInThisRow.contains(vp)).foreach(
            		    v => {
            		    	   usedInThisRow :+ v
            	  			   println(r +"," +c +"," + v)
                               board(r)(c) = v
                               if(r==8)
                                 board.foreach( row => println(row.mkString(" ")))
                               solve(board,usedInThisRow)
                               board(r)(c) = 0
                              })
                 }
             )
          })
          
          false

          for {
              r <- 1 to 9
              c <- 1 to 9 if(board(r)(c) == 0)
          } yield {
              (1 to 9).forany( v=>
                               board(r)(c) = v
                               solveBoard(board) )
          }
   
       }
    }*/
