// One can run this code by pasting it to https://scastie.scala-lang.org/

// I assume that active range is queried in the beginning of ProcessRange and is not updated
// during the call. That could affect the following scenario:
// val rrp = RangeRequestProcessor(n=2, s=10)
// rrp.ProcessRange(0, 100) // an array with 100 Blocks
// rrp.ProcessRange(0, 100) // an array with 100 Blocks
// print(rrp.GetActiveRange())
// There may be two options when ProcessRange is called second time:
// 1. (which I assume) request processor has an active range of (0, 9) initially and it only processes first 10 blocks;
//   then the active range will become (10, 19)
// 2. Request processor processes blocks one by one, then with each block the range will become:
//   (0, 9) -> (1, 10) -> (2, 11) -> ... -> (100, 109), which is the answer.

import scala.collection.mutable.{TreeMap, HashSet => MutableSet}

class Block(content: String) {}

// The class keeps interval map with the following invariants:
// 1. There is at least one element in the map (first element key equals to h)
// 2. First element count is < n
// 3. There are no two consequent elements with the same height
// 4. The height of each element is <= n and >= 0
// 5. Last element count == 0, since the range is not infinite
class RangeResponseProcessor(val n: Long, val s: Long) {
  // Counts represent an interval sorted map of height counts;
  // For example: if heights [0, 9] appeared twice and [10, 19] once - 
  // counts map will be: 0 -> 2, 10 -> 1, 20 -> 0
  protected val heightCounts = TreeMap[Long, Long]()
  heightCounts(0) = 0
  
  def ProcessRange(start: Long, count: Long): Unit = {
    val h = heightCounts.firstKey
    if (start >= h + s || count <= 0) return
    val correctedStart = math.max(start, h)
    
    val toRemove = MutableSet[Long]();
    
    if (!heightCounts.contains(correctedStart)) {
      // Maybe split the segment, if its height is not already at n
      // Because of the first invariant there will always be a smaller segment
      val prevHeightCount = heightCounts.maxBefore(correctedStart).get._2;
      if (prevHeightCount < n) {
        heightCounts(correctedStart) = prevHeightCount + 1L
      }
    } else { // the segment is already in the range
      if (correctedStart != h) { // Not the first segment
        if (heightCounts.maxBefore(correctedStart).get._2 == heightCounts(correctedStart) + 1) {
          // Height of this segment reached the height of the previous one
          toRemove.add(correctedStart)
        } else {
          heightCounts(correctedStart) = math.min(heightCounts(correctedStart) + 1L, n)
        }
      } else { // first element
        val firstHeightCount = heightCounts(correctedStart);
        if (firstHeightCount == n-1) {
          heightCounts(h) = n
          toRemove.add(h)
        } else {
          heightCounts(correctedStart) = firstHeightCount + 1L
        }
      }
    }
    val it = heightCounts.iteratorFrom(correctedStart)
    
    
    val activeRangeEnd = start + count
    it.dropWhile { (height, _) => height <= correctedStart }
      .takeWhile { (height, _) => height < activeRangeEnd }
      .foreach{ (height, count) =>
      val newCount = math.min(count + 1L, n)
      heightCounts(height) = newCount
      if (heightCounts.maxBefore(height).get._2 == newCount) {
          // Height of this segment is equal to the height of the previous one
        toRemove.add(height)
      }
    }
    heightCounts.subtractAll (toRemove)
    // Since the range is not infinite, we need to put the limiting segment.
    // This could happen only if the height of the previous segment was increased.
    if (!heightCounts.contains(activeRangeEnd)) {
      heightCounts(activeRangeEnd) = heightCounts.maxBefore(activeRangeEnd).get._2 - 1L
    }
    
    // The head was removed, so we need to optionally remove the new head 
    // if its height is at n to satisfy the 2. invariant.
    if (heightCounts.head._2 == n) {
      heightCounts -= heightCounts.firstKey
    }
  }
  
  def GetActiveRange() = {
    val h = heightCounts.firstKey
    (h, h + s-1)
  }
  
  // We can notice that ProcessRange does not depend on the content of blocks, only on their count,
  // according to the problem definition.
  def ProcessRange(start: Long, blocks: Iterable[Block]): Unit = {
    ProcessRange(start, blocks.size)
  }
}

// Let me keep it simple and not involve any testing framework, for the sake of code compactness.
class RangeResponseProcessorTester(override val n: Long, override val s: Long)
  extends RangeResponseProcessor(n, s) {
  def AssertInvariants(): Unit = {
    if (heightCounts.isEmpty) {
      throw new IllegalArgumentException("Empty counts")
    }
    if (heightCounts.head._2 >= n) {
      throw new IllegalArgumentException("First height >= n")
    }
    if (heightCounts.exists { (_, height) => height > n }) {
      throw new IllegalArgumentException("There is an element with the height > n")
    }
    if (heightCounts.exists { (_, height) => height < 0 }) {
      throw new IllegalArgumentException("There is an element with the height < 0")
    }
    // For each two consecutive elements check whether their values are different
    if (heightCounts.sliding(2).map { _.values }.exists { it => it.head == it.last }) {
      throw new IllegalArgumentException("There are two consecutive elements with the same height")
    }
    if (heightCounts.last._2 != 0) {
      throw new IllegalArgumentException("Last height count is not zero")
    }
  }
  
  override def ProcessRange(start: Long, count: Long): Unit = {
    super.ProcessRange(start, count)
    AssertInvariants()
  }
  
  override def GetActiveRange() = {
    val activeRange = super.GetActiveRange()
    AssertInvariants()
    return activeRange
  }
}

// Some test code
val rrp = new RangeResponseProcessorTester(3, 10)

rrp.ProcessRange(1, 5)
rrp.ProcessRange(1, 6)
rrp.ProcessRange(1, 7)

println(rrp.GetActiveRange())

rrp.ProcessRange(0,1)
rrp.ProcessRange(0,1)
rrp.ProcessRange(0,1)

println(rrp.GetActiveRange())
