package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite  {

  test("wordOccurrences: abcd") {
    println(wordOccurrences("abcd"));
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert") {
    def helperAddReducedElement(curList2: Occurrences, element: (Char, Int)) = {
        if (element._2-1 != 0){ (element._1, element._2 -1)::curList2 }
        else { curList2}
    }
    def helperAllWaystoRemoveOneCharacter(curList: Occurrences): List[Occurrences] =  {
         curList.map(e => helperAddReducedElement( curList.filterNot(_==e), e))
      }
    def recursiveAddReduced(aggregateList: List[Occurrences], listToAdd: List[Occurrences]): 
      List[Occurrences] = {
      println(listToAdd)
      if (listToAdd.isEmpty){ aggregateList;}
      else {recursiveAddReduced(aggregateList++listToAdd, listToAdd.map( e => 
        helperAllWaystoRemoveOneCharacter(e)).flatten.toSet.toList )}
      
    }
    val k = List(('a', 2), ('e', 1), ('t', 1));
    println("here")
    //println(k);
    println(recursiveAddReduced(List(), List(k)))
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }


  test("sentenceOccurrences: abcd e") {
    println(sentenceOccurrences(List("abcde", "ee")));
    assert(sentenceOccurrences(List("abcde", "aef")) === List(('a', 2), ('b', 1), ('c', 1), ('d', 1), ('e', 2), ('f', 1)))
  }


  test("dictionaryByOccurrences.get: eat") {
    val t = List("hello", "goodbye", "llohe");
    val t2 = t.map(e => (wordOccurrences(e), e));
   val t3 = t2.groupBy(e =>e._1)
   //val t4 = t3.map(e => (e._1, e._2.reduceLeft(List(_._2) :: List(_._2))))
  val t4 =  for {  
  i <- t3 
   
} yield (i._1, i._2.map(e => e._2))
    //val t1 = t.groupBy( e=> e._1)
    //val t2 = 
    println(t4);
    val n = dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet);
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) === Some(Set("ate", "eat", "tea")))
  }


  test("word anagrams: married") {
    
    
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
  }

  test("word anagrams: player") {
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }



  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    val k = (lard++lad).groupBy(_._1). map {
      e => e match  {
        case (z, List((x1, y1), (x2, y2))) => (z, Math.abs(y1-y2))
        case (z, List((x1, y1))) => (x1, y1)
      }
    }
    val nb = List(('a', 1), ('d', 1))
    val jb = List(('a', 1), ('d', 1))
    println("nbjb")
    println(subtract(nb, jb) )
    println("here2:")
    println(k.toList.filter(_._2!=0))
    assert(subtract(lard, r) === lad)
  }


  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: abba") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    println("combination abba test")
     def recGetCombos(aggregComb: List[Occurrences], occurrencesToAdd : List[Occurrences]) : List[Occurrences] = {
          
          if (occurrencesToAdd.isEmpty) { aggregComb }
          else {
          val newAggregComb = aggregComb ++ occurrencesToAdd
          
          val newOccurrencesToAdd = 
            for {
                   e <- occurrencesToAdd
                   i <- 0 until e.length
                } yield {
                  if (e(i)._2==1) { e.take(i) ++ e.drop(i+1)}
                  else { e.updated(i, (e(i)._1, e(i)._2-1)) }
                }
          
         // recGetCombos(newAggregComb, newOccurrencesToAdd )
              newOccurrencesToAdd.distinct 
         }
    }
    
    val m1 = recGetCombos(List(), List(abba))
    println(m1)
    val m2 = recGetCombos(List(), m1)
    println(m2)
    //print(combinations(abba))
    assert(combinations(abba).toSet === abbacomb.toSet)
  }


  test("sentence anagrams: []") {
    val sentence = List()
    println("hmm")
    println (List(Nil).head == Nil)
    assert(sentenceAnagrams(sentence) === List(Nil))
  }

  test("sentence anagrams: Linux rulez") {
    val sentence = List("Linux", "rulez")
    val sentence2 = List("Kmart", "stinks")
    val sentence3 = List("YOLO", "No")
    
    def combiningHelper(wordList: List[Word], wordList2: List[Word]) : List[Sentence] = {
      if (wordList.isEmpty) { List(wordList2)}
      if (wordList2.isEmpty) { List(wordList)}
      for {
        i <- wordList
        j <- wordList2
      } yield List(i, j)
      
    }
    println(combiningHelper(sentence, sentence2))
     def combiningHelper2(sentenceList: List[Sentence], wordList2: List[Word]) : List[Sentence] = {
      if (sentenceList.isEmpty) { List(wordList2)}
      if (wordList2.isEmpty) { sentenceList}
      for {
        i <- sentenceList
        
        
      }  yield i++wordList2
      
    }
     def combiningHelper3(sentenceList: List[Sentence], sentenceList2: List[Sentence]) : List[Sentence] = {
      if (sentenceList.isEmpty) { sentenceList2}
      if (sentenceList2.isEmpty) { sentenceList}
      for {
        i <- sentenceList
        j<- sentenceList2
        
      }  yield i++j
      
    }
    val j = combiningHelper(sentence, sentence2);
    val j2 = combiningHelper2(j, sentence3) 
    println(j2)
    
    
    
    
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    
     def getWordList(sentOccurrence: Occurrences) : List[Word] = {
      var temp2 = dictionaryByOccurrences get sentOccurrence
      var temp3 = temp2 match {
      case Some(x) => x;
      case None => List();
      }
      temp3;
    }
       def recursiveHelper(occurrences: Occurrences) : List[Sentence] = {
             if (occurrences.isEmpty){ return List(List("t")) }
             println(occurrences.isEmpty)
             return List(List("No"))
              val combos = combinations(occurrences)
              
              for {
                combo <- combos
                wordList <- getWordList(combo)
                sentList <- recursiveHelper(subtract(occurrences, combo))
                
              } yield List(wordList)++sentList
    
    }
      
       println("rcHelper")
       println(recursiveHelper(List()))
       println(sentenceAnagrams(sentence))
    
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }

}
