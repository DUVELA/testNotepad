package programmers.babbling
import scala.io.Source
import java.io.{File, PrintWriter}
import scala.util.matching.Regex

object Main {
    def main(args: Array[String]): Unit = {
        val inputFileName = "programmers/input_babbling.txt"
        val lines = Source.fromFile(inputFileName).getLines().toList
        
        //첫 번째 줄에서 테스트 케이스의 개수
        val numCases = lines.head.toInt

        //작업 시작
        val patterns = "^(aya(?!.*aya)|ye(?!.*ye)|woo(?!.*woo)|ma(?!.*ma))+$".r
        def solution(babbling: List[String]): Int = {
            babbling.map(i => {
                patterns.matches(i)
            }).count(_ == true)
        }
        val results = lines.tail.zipWithIndex.map{case (lineString, index) =>
            val bab = lineString.drop(1).dropRight(1).split(", ").map(_.replace("\"", "")).toList
            s"Case #${index + 1}: ${solution(bab)}"
        }
        //작업 끝

        //output생성
        val outputFileName = "programmers/output_babbling.txt"
        val writer = new PrintWriter(new File(outputFileName))
        results.foreach{result =>
            println(result)
            writer.println(result)
        }
        writer.close()
    }
}
