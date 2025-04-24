import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.Try

object MyApp {
  // Map that holds food prices
  val foodPrices: Map[String, List[Double]] = readData("data.txt")

  // Reading data file and reading map
  def readData(filePath: String): Map[String, List[Double]] = {
    Try {
      // Open file
      val source = Source.fromFile(filePath)
      // Go through each line
      val data = source.getLines().map { line =>
        // Split the line by commas and clean up extra spaces
        val parts = line.split(",").map(_.trim)
        // Check if the line has enough bits (food name + 24 prices)
        if (parts.length >= 25) {
          val foodName = parts(0)
          // Take the parts as prices and turn them into numbers
          val prices = parts.drop(1).take(24).flatMap(p => Try(p.toDouble).toOption).toList
          Some(foodName -> prices)
        } else {
          None
        }
      }.collect { case Some(entry) => entry }.toMap // Only keeping valid entries
      source.close() // Close the file
      data
    }.getOrElse {
      // Print error message
      println(s"Error reading file $filePath. Using empty data.")
      Map.empty[String, List[Double]]
    }
  }

  // Price conversion from pence to £
  def formatPrice(pence: Double): String = {
    val pounds = pence / 100.0
    f"$pence%.2f pence (£$pounds%.2f)"
  }

  // Gets latest price
  def getCurrentPrices(prices: Map[String, List[Double]]): Map[String, Double] = {
    prices.map { case (food, priceList) =>
      // Grab the last price in the list, or use 0.0 if the list is empty
      food -> priceList.lastOption.getOrElse(0.0)
    }
  }

  // Print current price
  def showCurrentPrices(): Unit = {
    val current = getCurrentPrices(foodPrices)
    println("\nCurrent Prices (per kg/litre):")
    // Print name and price
    current.toList.sortBy(_._1).foreach { case (food, price) =>
      println(f"$food%-10s: ${formatPrice(price)}%-20s")
    }
  }

  // Finds the highest and lowest price
  def getHighLowPrices(prices: Map[String, List[Double]]): Map[String, (Double, Double)] = {
    prices.map { case (food, priceList) =>
      // Print 0.0 if no price
      val minPrice = priceList.minOption.getOrElse(0.0)
      val maxPrice = priceList.maxOption.getOrElse(0.0)
      food -> (minPrice, maxPrice)
    }
  }

  // Print the highest and lowest price
  def showHighLowPrices(): Unit = {
    val highLow = getHighLowPrices(foodPrices)
    println("\nHighest and Lowest Prices (per kg/litre):")
    highLow.toList.sortBy(_._1).foreach { case (food, (min, max)) =>
      println(f"$food%-10s: Lowest: ${formatPrice(min)}%-20s, Highest: ${formatPrice(max)}%-20s")
    }
  }

  // Calculate median price
  def getMedianPrices(prices: Map[String, List[Double]]): Map[String, Double] = {
    prices.map { case (food, priceList) =>
      val sorted = priceList.sorted // Sort the prices
      // If empty use 0.0
      val median = if (sorted.isEmpty) {
        0.0
      } else if (sorted.length % 2 == 0) {
        (sorted(sorted.length / 2 - 1) + sorted(sorted.length / 2)) / 2.0
      } else {
        sorted(sorted.length / 2)
      }
      food -> median
    }
  }

  // Print median price
  def showMedianPrices(): Unit = {
    val medians = getMedianPrices(foodPrices)
    println("\nMedian Prices (per kg/litre):")
    medians.toList.sortBy(_._1).foreach { case (food, median) =>
      println(f"$food%-10s: ${formatPrice(median)}%-20s")
    }
  }

  // Calculate average price
  def getAveragePrice(prices: List[Double]): Double = {
    // If empty use 0.0
    if (prices.isEmpty) 0.0
    else prices.sum / prices.length
  }

  // Comparing two item prices
  def compareAveragePrices(): Unit = {
    println("\nEnter first food name:")
    val food1 = readLine().trim.toUpperCase // First option
    println("Enter second food name:")
    val food2 = readLine().trim.toUpperCase // Second option

    // Get average price or use 0.0 if empty
    val avg1 = foodPrices.get(food1).map(getAveragePrice).getOrElse(0.0)
    val avg2 = foodPrices.get(food2).map(getAveragePrice).getOrElse(0.0)

    println("\nAverage Prices (per kg/litre):")
    // Print avg for option 1
    if (foodPrices.contains(food1)) {
      println(f"$food1%-10s: ${formatPrice(avg1)}%-20s")
    } else {
      println(s"Warning: $food1 not found.")
    }
    // Print avg for option 2
    if (foodPrices.contains(food2)) {
      println(f"$food2%-10s: ${formatPrice(avg2)}%-20s")
    } else {
      println(s"Warning: $food2 not found.")
    }
  }

  // Get total basket value
  def calculateBasketValue(basket: List[(String, Float)], prices: Map[String, List[Double]]): (Double, List[String], List[(String, Float)]) = {
    basket.foldLeft((0.0, List.empty[String], List.empty[(String, Float)])) { case ((total, invalid, valid), (food, qty)) =>
      prices.get(food) match {
        // Add latest price to total
        case Some(priceList) =>
          val currentPrice = priceList.lastOption.getOrElse(0.0)
          (total + currentPrice * qty, invalid, (food, qty) :: valid)
        case None =>
          (total, food :: invalid, valid)
      }
    }
  }

  // Suggest an available item
  // Guide used to implement functionality: https://www.scala-lang.org/blog/2020/05/05/scala-3-import-suggestions.html
  def suggestFood(input: String, validFoods: Set[String]): Option[String] = {
    val inputLower = input.toLowerCase
    validFoods.find(food => food.toLowerCase.startsWith(inputLower) || food.toLowerCase.contains(inputLower))
  }

  // Building basket
  def showBasketValue(): Unit = {
    println("\n== Basket Calculation ==")
    // Print available items
    println("\nAvailable items: " + foodPrices.keys.toList.sorted.mkString(", "))

    // Function to add items to the basket
    def readBasket(acc: List[(String, Float)]): List[(String, Float)] = {
      println("\nEnter item name or 'done' : ")
      val foodInput = readLine().trim
      if (foodInput.toLowerCase == "done") acc // Stop if they type 'done'
      else {
        val food = foodInput.toUpperCase
        // Check if valid
        if (foodPrices.contains(food)) {
          println(s"Enter quantity for $food (kg/litre):")
          // Try to read a valid quantity
          Try(readLine().trim.toFloat).toOption match {
            case Some(qty) if qty >= 0 =>
              println(s"Added: $food, $qty kg/litre")
              readBasket((food, qty) :: acc) // Add it and continue
            case _ =>
              println("Invalid quantity. Please enter a non-negative number!")
              readBasket(acc) // Try again
          }
        } else {
          // Suggest a similar item if invalid entry
          suggestFood(food, foodPrices.keySet) match {
            case Some(suggestion) =>
              println(s"Item '$food' not found. Did you mean '$suggestion'?")
            case None =>
              println(s"Item '$food' not found. Please choose from: ${foodPrices.keys.toList.sorted.mkString(", ")}")
          }
          readBasket(acc) // Try again
        }
      }
    }

    // Function to confirm or edit the basket
    def confirmBasket(basket: List[(String, Float)]): Unit = {
      if (basket.isEmpty) {
        println("\nNo items in basket. Returning to menu.")
        return
      }

      // Print full basket
      println("\nYour basket contains:")
      basket.reverse.foreach { case (food, qty) =>
        println(f"- $food%-10s: $qty%.2f kg/litre")
      }
      // Options menu
      println("\n")
      println("- y: Calculate total value")
      println("- n: Cancel and return to menu")
      println("- c: Continue with basket")
      print("Enter your choice: ")
      val choice = readLine().trim.toLowerCase
      choice match {
        case "y" =>
          // Calculate and print total cost
          val (total, invalidFoods, validBasket) = calculateBasketValue(basket, foodPrices)
          println("\n== Basket Value ==")
          validBasket.reverse.foreach { case (food, qty) =>
            val price = foodPrices(food).lastOption.getOrElse(0.0)
            // Format price to both pence and £
            println(f"\n$food%-10s: $qty%.2f kg/litre @ ${formatPrice(price)}%-20s = ${formatPrice(price * qty)}%-20s")
          }
          invalidFoods.foreach(food => println(s"Warning: $food not found."))
          println(f"\nTotal: ${formatPrice(total)}%-20s")
        case "n" =>
          println("Basket cancelled. Returning to menu.")
        case "c" =>
          // Add more items
          val updatedBasket = readBasket(basket)
          confirmBasket(updatedBasket)
        case _ =>
          println("Invalid choice. Please enter the correct choice!")
          confirmBasket(basket) // Try again
      }
    }

    // Building the basket
    val basket = readBasket(List.empty)
    confirmBasket(basket)
  }

  // Welcome message
  def welcomePrompt(): Boolean = {
    println("\n== Welcome! ==")
    println("\nWould you like to continue?")
    println("\nContinue : 'c'")
    println("Quit : 'q'")
    println("\nInput: ")
    val input = readLine().trim.toLowerCase
    input match {
      case "c" => true
      case "q" => false
      case _ =>
        println("Invalid input. Please enter 'c' or 'q'.")
        welcomePrompt() // loop until valid input
    }
  }

  // Print features menu
  def displayMenu(): Int = {
    println("\n== Features ==")
    println("\n1. Current prices")
    println("2. Highest and lowest prices")
    println("3. Median prices")
    println("4. Compare average prices of two items")
    println("5. Calculate basket value")
    println("6. Quit")
    print("\nEnter: ")
    // Try to read a valid option
    Try(readLine().trim.toInt).toOption.getOrElse(-1)
  }

  // Runs main
  def run(): Unit = {
    if (welcomePrompt()) {
      var continue = true
      // Keep loop until broken
      while (continue) {
        val choice = displayMenu()
        choice match {
          case 1 => showCurrentPrices()
          case 2 => showHighLowPrices()
          case 3 => showMedianPrices()
          case 4 => compareAveragePrices()
          case 5 => showBasketValue()
          case 6 => continue = false
          case _ => println("Invalid choice. Please enter a number between 1 and 6.")
        }
      }
    }
    // Print exit message
    println("== Shutting Down ==")
    println(" 3 . . . 2 . . . 1 . . .")
  }

  // main
  def main(args: Array[String]): Unit = {
    run()
  }
}