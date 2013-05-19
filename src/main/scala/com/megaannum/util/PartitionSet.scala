
package com.megaannum.util
import scala.collection._

/** I found a Java implementation that pinted the partion setout of the net.
 * This was months ago.
 * I converted to Scala and had it return the set. 
 * In the future, I should make this return an Iterator.
 *
 * Prior to checking this in I searched for the original but failed to
 * find it. If you know, I will add my thanks and attributio here.
 */
object PartitionSet {
  def apply(nos: Int): PartitionSet = new PartitionSet(nos)

  def toString(ps: Array[Array[Int]], n: Int): String = {
    val buf = new StringBuilder(64)

    var first_time = true
    for (p <- ps) {
      if (first_time) first_time = false
      else buf.append(" ")
      buf.append('{')
      var first_time_inner = true
      for (e <- p) {
        if (first_time_inner) first_time_inner = false
        else buf.append(", ")
        buf.append(e)
      }
      buf.append('}')
    }
    buf.toString
  }


  /**
   * Print Partition
   */
  def printp(s: Array[Int], n: Int): Unit = {
    // Get the total number of partitions.
    var part_num = 1
    var i = 0
    while (i < n) {
      if (s(i) > part_num) part_num = s(i)
      i += 1
    }

    // Print the partitions.
    var p = part_num
    while (p >= 1) {
      print("{")
      // If s(i) == p, then i + 1 is part of the pth partition.
      i = 0
      var had_one = false
      while (i < n) {
        if (s(i) == p) {
          if (had_one) print(", ")
          else had_one = true

          print((i + 1).toString)
        }
        i += 1
      }
      print("} ")
      p -= 1
    }
    println()
  }

  def record(s: Array[Int], n: Int): Array[Array[Int]] = {
    val r = new mutable.ArrayBuffer[Array[Int]]
    // Get the total number of partitions.
    var part_num = 1
    var i = 0
    while (i < n) {
      if (s(i) > part_num) part_num = s(i)
      i += 1
    }

    // Get the partitions.
    val part = new mutable.ArrayBuffer[Int]
    var p = part_num
    while (p >= 1) {
      part.clear()

      // If s(i) == p, then i + 1 is part of the pth partition.
      i = 0
      while (i < n) {
        if (s(i) == p) {
          part += (i + 1)
        }
        i += 1
      }
      r += part.toArray
      p -= 1
    }

    r.toArray
  }

  def generateNext(s: Array[Int], m: Array[Int], n: Int): Boolean = {
    var i = 0
    s(i) = s(i) + 1

    while ((i < n - 1) && (s(i) > m(i+1) + 1)) {
      s(i) = 1
      i += 1
      s(i) = s(i) + 1
    }

    // 
    // If i is has reached n-1 th element, 
    // then the last unique partitiong has been found
    // 
    if (i == n - 1) return false

    // 
    // Because all the first i elements are now 1, s(i) (i + 1 th element)
    // is the largest. 
    // So we update max by copying it to all the first i positions in m.
    // 
    if (s(i) > m(i)) m(i) = s(i)

    var j = i - 1
    while (j >= 0) {
      m(j) = m(i)
      j -= 1
    }

    true
  }

  def main(arg: Array[String]): Unit = {
    // s(i) is the number of the set in which the ith element should go 
    val s: Array[Int] = new Array(16) 

    // m(i) is the largest of the first i elements in s
    val m: Array[Int] = new Array(16) 

    val n = 3

    // The first way to partition a set is to put all the 
    // elements in the same subset.
    var i = 0
    while (i < n) {
      s(i) = 1
      m(i) = 1
      i += 1
    }

    // Print the first partitioning.
    printp(s, n);

    // Print the other partitioning schemes.
    while (generateNext(s, m, n)) printp(s, n)


    i = 1
    while (i < 5) {
      val p = new PartitionSet(i)
      println("size=" +i)
      print(p.toString)
      println()
      i += 1
    }
  }
}

class PartitionSet(nos: Int) {
  lazy val sets: Array[Array[Array[Int]]] = generate

  private def generate: Array[Array[Array[Int]]] = {
    // s(i) is the number of the set in which the ith element should go 
    val s: Array[Int] = new Array(16) 

    // m(i) is the largest of the first i elements in s
    val m: Array[Int] = new Array(16) 

    // The first way to partition a set is to put all the 
    // elements in the same subset.
    var i = 0
    while (i < nos) {
      s(i) = 1
      m(i) = 1
      i += 1
    }

    val sets = new mutable.ArrayBuffer[Array[Array[Int]]]
    sets += PartitionSet.record(s, nos)

    while (PartitionSet.generateNext(s, m, nos)) 
      sets += PartitionSet.record(s, nos)

    sets.toArray
  }

  override def toString: String = {
    val buf = new StringBuilder(64)
    for (set <- sets) {
      buf.append(PartitionSet.toString(set, nos))
      buf.append('\n')
    }
    buf.toString
  }

}
