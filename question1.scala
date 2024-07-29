import scala.collection.mutable.Map

object demo {

  val inventory1: Map[Int, (String, Int, Double)] = Map.empty
  val inventory2: Map[Int, (String, Int, Double)] = Map.empty


  def retrieveProductNames(inventory: Map[Int, (String, Int, Double)]): List[String] = {
    inventory.values.map(_._1).toList
  }

  def calculateTotalValue(inventory: Map[Int, (String, Int, Double)]): Double = {
    inventory.values.map { case (_, quantity, price) => quantity * price }.sum
  }

  def isInventoryEmpty(inventory: Map[Int, (String, Int, Double)]): Boolean = {
    inventory.isEmpty
  }


  def mergeInventories(inv1: Map[Int, (String, Int, Double)], inv2: Map[Int, (String, Int, Double)]): Map[Int, (String, Int, Double)] = {
    inv2.foreach { case (id, (name, quantity, price)) =>
      if (inv1.contains(id)) {
        val (existingName, existingQuantity, existingPrice) = inv1(id)
        inv1(id) = (existingName, existingQuantity + quantity, math.max(existingPrice, price))
      } else {
        inv1(id) = (name, quantity, price)
      }
    }
    inv1
  }


  def checkAndPrintProductDetails(inventory: Map[Int, (String, Int, Double)], productId: Int): Unit = {
    inventory.get(productId) match {
      case Some((name, quantity, price)) =>
        println(s"Product found: ID = $productId, Name = $name, Quantity = $quantity, Price = $price")
      case None =>
        println(s"Product with ID $productId does not exist in inventory.")
    }
  }


  def addProductToInventory(inventory: Map[Int, (String, Int, Double)]): Unit = {
    println("Enter product ID: ")
    val id = scala.io.StdIn.readInt()
    println("Enter product name: ")
    val name = scala.io.StdIn.readLine()
    println("Enter product quantity: ")
    val quantity = scala.io.StdIn.readInt()
    println("Enter product price: ")
    val price = scala.io.StdIn.readDouble()
    inventory(id) = (name, quantity, price)
  }


  def main(args: Array[String]): Unit = {

    println("Add products to inventory1:")
    addProductToInventory(inventory1)
    addProductToInventory(inventory1)

    println("Add products to inventory2:")
    addProductToInventory(inventory2)
    addProductToInventory(inventory2)


    val productNames = retrieveProductNames(inventory1)
    println(s"Product names in inventory1: $productNames")


    val totalValue = calculateTotalValue(inventory1)
    println(s"Total value of all products in inventory1:  $totalValue")


    val isEmpty = isInventoryEmpty(inventory1)
    println(s"Is inventory1 empty? $isEmpty")


    val mergedInventory = mergeInventories(inventory1, inventory2)
    println("Merged Inventory:")
    mergedInventory.foreach { case (id, (name, quantity, price)) =>
      println(s"ID = $id, Name = $name, Quantity = $quantity, Price = $price")
    }


    println("Enter product ID to check: ")
    val productIdToCheck = scala.io.StdIn.readInt()
    checkAndPrintProductDetails(inventory1, productIdToCheck)
  }
}
