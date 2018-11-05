//scalac -cp scalatest-app_2.12-3.0.4.jar IntScalaTest.scala IntSet.scala
//scala -cp scalatest-app_2.12-3.0.4.jar org.scalatest.run IntScalaTest

import org.scalatest.FunSuite

class IntScalaTest extends FunSuite{

  //test("HEIKL"){
  //  assert() // bool
  //  intercept[AssertionError]{  }
  //  IllegalArgumentError 
  //}
  
  test("1. Find an element"){
    val set1: IntSet = IntSet(1,2,3)
    assert(set1.find(2).datum == 2)
  }
  
  test("2. Convert empty set to string"){
    val empty1: IntSet = IntSet()
    assert(empty1.toString == "{}")
  }  
  
  test("3. Convert set with multiple elements to string"){
    val set2: IntSet = IntSet(5,4,3,2,1)
    assert(set2.toString == "{1, 2, 3, 4, 5}")
  }
  
  test("4. Add an element to a set"){
    val set3: IntSet = IntSet(1,2,3)
    set3.add(5)
    assert(set3.toString == "{5, 3, 2, 1}")
  }    
  
  test("5. Add an element to an empty set"){
    val empty2 = IntSet()
    empty2.add(5) 
    assert(empty2.toString == "{5}")
  }
  
  test("6. Don't add an element if it's already there"){
    val set4 = IntSet(1,2,3,4)
    set4.add(3)
    assert(set4.toString == "{4, 3, 2, 1}")
  }    
  
  test("7. Find the size of a set"){
    val set5 = IntSet(1,2,3,4,5)
    assert(set5.size == 5)
  }
  
  test("8. Find the size of an empty list"){
    val empty3 = IntSet()
    assert(empty3.size == 0)
  } 
  
  test("9. Test if a set contains an element (and it does)"){
    val set6 = IntSet(1,2,3,4,5)
    assert(set6.contains(3) == true)
  }
  
  test("10. Test if a set contains an element (and it doesn't)"){
    val set7 = IntSet(1,2,3,4,5)
    assert(set7.contains(6) == false)
  } 
  
  test("11. Test if the empty set contains an element"){
    val empty4 = IntSet()
    assert(empty4.contains(3) == false)
  }
  
  test("12. 'Any' returns the first element in the set (most recently added)"){
    val set8 = IntSet(1,2,3)
    assert(set8.any == 3)
  }     
  
  test("13. 'Any' on empty set throws exception"){
    val empty5 = IntSet()
    intercept[AssertionError]{empty5.any}
  }  
  
  test("14. Our set equals that set"){
    val set9 = IntSet(1,2,3)
    assert(set9.equals(IntSet(1,3,2)) == true)
  }   
  
  test("15. Our set doesn't equal that set (but same length)"){
    val set10 = IntSet(1,2,3,4)
    assert(set10.equals(IntSet(1,2,4,5)) == false)
  }
  
  test("16. Our set doesnt't equal that set (and different length)"){
    val set11 = IntSet(1,2,3,4)
    assert(set11.equals(IntSet(1,2,3)) == false)
  }    
  
  test("17. Our set equals that set and they are both empty"){
    val empty6 = IntSet()
    assert(empty6.equals(IntSet()) == true)
  }
  
  test("18. Our set doesn't equal that set and our set is empty"){
    val empty7 = IntSet()
    assert(empty7.equals(IntSet(1,2,3)) == false)
  }
  
  test("19. Our set doesn't equal that set and that set is empty"){
    val empty8 = IntSet(1,2,3)
    assert(empty8.equals(IntSet()) == false)
  }  
  
  test("20. Our set doesn't equal that set and that set is not an IntSet"){
    val set12 = IntSet(1,2,3)
    assert(set12.equals(Array(1,2,3)) == false)
  }  
  
  test("21. Remove an element in the set which is there"){
    val set13 = IntSet(1,2,3,4)
    assert(set13.remove(3) == true)
    set13.remove(3)
    assert(set13.toString == "{4, 2, 1}")
  }  
  
  test("22. Remove an element in the set which is not there"){
    val set14 = IntSet(1,2,3)
    assert(set14.remove(4) == false)
  }
  
  test("23. Our set is a subset of that set"){
    val set15 = IntSet(1,2,3,4,5)
    assert(set15.subsetOf(IntSet(1,2,3)) == true)
  }    
  
  test("24. Our set is not a subset of that set"){
    val set16 = IntSet(1,2,3)
    assert(set16.subsetOf(IntSet(1,2,3,4,5)) == false)
  }  
  
  test("25. Map (+5) to all the elements of the set"){
    val set17 = IntSet(1,2,3)
    assert(set17.map(_+5).toString == "{6, 7, 8}")
  }  

  test("26. Filter"){
    val set18 = IntSet(1,2,3,4,5)
    assert(set18.filter(_>3).toString == "{4, 5}")
  }  

}  
